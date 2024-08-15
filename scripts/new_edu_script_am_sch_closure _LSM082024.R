

library(tidyverse)
#library(tidytidbits)
library(tidycmprsk)
library(lubridate)
#library(survivalAnalysis)
library(survival)
library(survminer)
library(ggsurvfit)
library(gtsummary)



# importing data ===============================================================
data <- read.table("dados/inep_am.txt", stringsAsFactors = FALSE)

##checking
#View(data)
head(data)
table(data$nu_ano_censo, data$sg_uf)
table(data$nu_ano_censo, data$tp_situacao_funcionamento)
table(data$in_fund_ai, data$tp_situacao_funcionamento)
table(data$in_fund_af, data$tp_situacao_funcionamento)
table(data$in_med, data$tp_situacao_funcionamento)
table(data$nu_ano_censo, data$tp_localizacao)
table(data$nu_ano_censo, data$tp_localizacao_diferenciada)
#table(data$co_entidade, data$nu_ano_censo)


# crearing a copy
data.copy <- data
#data <- data.copy

# creating new categories for school teaching level ============================
#1==fundamental 1 only; 2==fundamental 2 only; 3==fundamental 1 e 2
#4==medio; 5==fundamental 1 e medio; 6==fundamental 2 e medio; 7==fundamental 1 e 2, e medio
#0==prof AND/OR prof-tech AND/OR eja
data$in_nivel_ensino <- ifelse(data$in_fund_ai==1 & data$in_fund_af==0 & data$in_med==0, 1,
                               ifelse(data$in_fund_ai==0 & data$in_fund_af==1 & data$in_med==0, 2,
                                      ifelse(data$in_fund_ai==1 & data$in_fund_af==1 & data$in_med==0, 3,
                                             ifelse(data$in_fund_ai==0 & data$in_fund_af==0 & data$in_med==1, 4,
                                                    ifelse(data$in_fund_ai==1 & data$in_fund_af==0 & data$in_med==1, 5,
                                                           ifelse(data$in_fund_ai==0 & data$in_fund_af==1 & data$in_med==1, 6,
                                                                  ifelse(data$in_fund_ai==1 & data$in_fund_af==1 & data$in_med==1, 7, 0)))))))

table(data$in_nivel_ensino)


# creating new categories for school location ==================================
## setting the [standard] location of the school 
## based on the most frequent category between 2012 and 2018
for (sl in unique(data$co_entidade)) {
  
  df <- data[data$co_entidade==sl,]
  
  if(length(df[df$nu_ano_censo>=2012 & df$nu_ano_censo<=2018,"tp_localizacao_diferenciada"])<=1) next
  
  
  data[data$co_entidade==sl,"tp_localizacao_diferenciada"] <- as.numeric(tail(names(sort(table(df[df$nu_ano_censo>=2012 & df$nu_ano_censo<=2018,"tp_localizacao_diferenciada"]))), 1))
  
  }

rm(df); gc()

## new categories
#0==urbana; 1==assentamento; 2==terra indigena; 3=quilombo; 4==sustainable use area ;5==other non-urban
data$tp_localizacao2 <- ifelse(data$tp_localizacao==1 & data$tp_localizacao_diferenciada==0, 0,
                               ifelse(data$tp_localizacao_diferenciada==5, 2,
                                      ifelse(data$tp_localizacao_diferenciada==6, 3,
                                             ifelse(data$tp_localizacao==2 & data$tp_localizacao_diferenciada==0, 5,
                                                    data$tp_localizacao_diferenciada))))


# schools info: year base 2011 =================================================
# replacing missing data (NAs) by 0
data <- data %>% mutate(across(qt_salas_utilizadas:qt_doc_eja_med, ~ifelse(is.na(.),0,.)))

## location and teaching level (grade)
school.2011.info <- data %>% filter(nu_ano_censo==2011 & tp_situacao_funcionamento==1) %>% 
  dplyr::select(co_entidade:tp_localizacao_diferenciada, in_nivel_ensino, tp_localizacao2)

## lon-lat
school.2011.geo <- read.csv("dados/inep_am2011_local.csv")
school.2011.geo <- school.2011.geo %>% filter(co_entidade %in% school.2011.info$co_entidade)


# calculating survival times ===================================================
## data will often come with start and end dates rather than pre-calculated survival times. 
## the first step is to make sure these are formatted as dates in R.

## converting to wide format to see years in columns
data.wide <- data %>% pivot_wider(id_cols = co_entidade, 
                                  names_from = nu_ano_censo, 
                                  values_from = tp_situacao_funcionamento,
                                  values_fill = 0) %>% 
                      filter(`2011` == 1)

## adding status: 
## 2== an event occurred (closed)
## 1== an event occurred ('paralysed')
## 0== data is censored -- no event by end of fixed study period (2022) OR loss to follow-up

data.wide$status <- 0

## making 2011 the first year of monitoring
data.wide$first_date <- ymd(2011, truncated = 2L)

## adding the last follow up date
data.wide$last_fup_date <- 2011


for (sl in unique(data.wide$co_entidade)) {
  
  v <- data.wide[data.wide$co_entidade==sl,]
  
  if(all(v[,2:13] %in% c(0,1), na.rm = T)) {
    
    data.wide[data.wide$co_entidade==sl,"last_fup_date"] <- as.numeric(last(names(v[,2:13])[which(v[,2:13]==1)]))
    
  }
  
  
  if(any(v[,2:13]==3, na.rm = T)) {
    
    data.wide[data.wide$co_entidade==sl,"last_fup_date"] <- as.numeric(names(v[,2:13])[which(v[,2:13]==3)])
    data.wide[data.wide$co_entidade==sl,"status"] <- 2
    
  }
  
  if(any(v[,2:13] %in% c(1, 2), na.rm = T) & !any(v[,2:13] == 3, na.rm = T) & !all(v[,2:13] %in% c(0,1), na.rm = T)) {
    
    teste<-t(v[,2:13])==2 & lag(t(v[,2:13]))!=2
    data.wide[data.wide$co_entidade==sl,"last_fup_date"] <- as.numeric(last(names(teste[teste[,1]==T,])))
        data.wide[data.wide$co_entidade==sl,"status"] <- 1
    
  }
  
}

rm(v); rm(teste); gc()

## make sure these are formatted as dates
data.wide$last_fup_date <- ymd(data.wide$last_fup_date, truncated = 2L)

## adding duration:
## calculate the difference between start and end dates
data.wide <- data.wide %>% mutate(duration = as.duration(first_date %--% last_fup_date) / dyears(1))

### checking if there is any NA
anyNA(data.wide$last_fup_date)
### checking if the school starts as paralysed and ended as active
length(which(data.wide$`2011`==2 & data.wide$status==0))
### checking if the school starts as paralysed and ended as closed
length(which(data.wide$`2011`==2 & data.wide$status==1))
### checking if the school has 0 years duration
length(which(data.wide$duration==0))

### excluding school: (a) that started as paralised and ended as closed; and (b) has 0 years duration
#data.wide <- data.wide %>% filter(duration!=0)

data.wide <- data.wide %>% left_join(school.2011.info)

### summary
#### how many schools by "tp_localizacao"?
#### all brazilian legal amazon
#data.wide %>% group_by(tp_localizacao) %>% summarise(n())
#### only Amazonas state
#data.wide %>% filter(sg_uf=="AM") %>% group_by(tp_localizacao) %>% summarise(n())
#
#
#### how many 'rural' schools by "in_nivel_ensino"?
#### all brazilian legal amazon
#data.wide %>% filter(tp_localizacao==2) %>% group_by(in_nivel_ensino) %>% summarise(n())
#### only Amazonas state
#data.wide %>% filter(sg_uf=="AM" & tp_localizacao==2) %>% group_by(in_nivel_ensino) %>% summarise(n())

# crearing a copy
data.wide.copy <- data.wide

# converting from tibble to data frame and [re]formating data types
data.wide <- as.data.frame(data.wide)
data.wide <- data.wide %>% 
  mutate(
    sg_uf = factor(sg_uf, levels=c("AC","AM","AP","MA","MT","PA","RO","RR","TO")),
    co_municipio = factor(co_municipio),
    tp_localizacao = factor(tp_localizacao, levels=c("1","2"), labels=c("Urban","Rural")),
    tp_localizacao2 = factor(tp_localizacao2, levels=c("0","1","2","3","4","5"), labels=c("Urban","Settlement", "Indigenous", "Quilombo", "SUU", "Rural")),
    in_nivel_ensino = factor(in_nivel_ensino, levels=c("1","2","3","4","5","6","7","0"), labels=c("F1", "F2", "F1.2", "M", "F1M", "F2M", "F1.2M","PTE"))
  )


# school-level drivers of closure ==============================================
## number of rooms
schools.2011.qt.rooms <- data %>% filter(nu_ano_censo==2011 & tp_situacao_funcionamento==1) %>%
  mutate(in_eja = ifelse(qt_mat_eja_fund!=0 | qt_mat_eja_med!=0, 1, 0)) %>% 
  rename(qt_rooms_11 = qt_salas_utilizadas) %>% 
  dplyr::select(co_entidade, in_eja, qt_rooms_11)

schools.2011.qt.rooms$dqt_rooms <- NA
for (sl in schools.2011.qt.rooms$co_entidade) {
  
  year.fn <- year(data.wide[data.wide$co_entidade==sl, "last_fup_date"])
  
  qt.room.fn <- as.numeric(data %>% filter(nu_ano_censo==year.fn & co_entidade==sl) %>%
                            dplyr::select(qt_salas_utilizadas))
  
  qt.room.11 <- as.numeric(schools.2011.qt.rooms[schools.2011.qt.rooms$co_entidade==sl,"qt_rooms_11"])
  
  schools.2011.qt.rooms[schools.2011.qt.rooms$co_entidade==sl,"dqt_rooms"] <- (qt.room.fn - qt.room.11)/qt.room.11
  
  }

### checking
#nrow(schools.2011.qt.rooms[is.na(schools.2011.qt.rooms$dqt_mat),])

rm(year.fn); rm(qt.room.fn); rm(qt.room.11); gc()


## number of 'matriculas'
schools.2011.qt.matriculas <- data %>% filter(nu_ano_censo==2011 & tp_situacao_funcionamento==1) %>%
  mutate(qt_matriculas_11 = qt_mat_fund_ai + qt_mat_fund_af + qt_mat_med + 
           qt_mat_prof + qt_mat_prof_tec + qt_mat_eja_fund + qt_mat_eja_med) %>%
  dplyr::select(co_entidade, qt_matriculas_11)

schools.2011.qt.matriculas$dqt_mat <- NA
for (sl in schools.2011.qt.matriculas$co_entidade) {
  
  year.fn <- year(data.wide[data.wide$co_entidade==sl, "last_fup_date"])
  
  qt.mat.fn <- as.numeric(data %>% filter(nu_ano_censo==year.fn & co_entidade==sl) %>%
                            mutate(qt_mat = qt_mat_fund_ai + qt_mat_fund_af + qt_mat_med +
                                     qt_mat_prof + qt_mat_prof_tec + qt_mat_eja_fund + qt_mat_eja_med) %>%
                            dplyr::select(qt_mat))
  
  j=1
  while (is.na(qt.mat.fn)) {
    year.fn <- year(year.fn)-j
    qt.mat.fn <- as.numeric(data %>% filter(nu_ano_censo==year.fn & co_entidade==sl) %>%
                              mutate(qt_mat = qt_mat_fund_ai + qt_mat_fund_af + qt_mat_med +
                                       qt_mat_prof + qt_mat_prof_tec + qt_mat_eja_fund + qt_mat_eja_med) %>%
                              dplyr::select(qt_mat))
    j=j+1
    }
  
  qt.mat.11 <- as.numeric(schools.2011.qt.matriculas[schools.2011.qt.matriculas$co_entidade==sl,"qt_matriculas_11"])
  
  m=2012
  while (qt.mat.11==0 | is.na(qt.mat.11)) {
    qt.mat.11 <- as.numeric(data %>% filter(nu_ano_censo==m & co_entidade==sl) %>%
                              mutate(qt_mat = qt_mat_fund_ai + qt_mat_fund_af + qt_mat_med +
                                       qt_mat_prof + qt_mat_prof_tec + qt_mat_eja_fund + qt_mat_eja_med) %>%
                              dplyr::select(qt_mat))
    m=m+1
    if(m==2022) break
    }
  
  if(qt.mat.11==0 | is.na(qt.mat.11)) next
  
  schools.2011.qt.matriculas[schools.2011.qt.matriculas$co_entidade==sl,"dqt_mat"] <- (qt.mat.fn - qt.mat.11)/qt.mat.11
  
  }

### checking
#nrow(schools.2011.qt.matriculas[is.na(schools.2011.qt.matriculas$dqt_mat),])
### there are 2893 school with no information about 'matriculas'

rm(year.fn); rm(qt.mat.fn); rm(qt.mat.11); rm(j); rm(m); gc()


## number of 'docentes'
schools.2011.qt.docentes <- data %>% filter(nu_ano_censo==2011 & tp_situacao_funcionamento==1) %>%
  mutate(qt_doc_11 = qt_doc_fund_ai + qt_doc_fund_af + qt_doc_med + 
           qt_doc_prof + qt_doc_prof_tec + qt_doc_eja_fund + qt_doc_eja_med) %>%
  dplyr::select(co_entidade, qt_doc_11)

schools.2011.qt.docentes$dqt_doc <- NA
for (sl in schools.2011.qt.docentes$co_entidade) {
  
  year.fn <- year(data.wide[data.wide$co_entidade==sl, "last_fup_date"])
  
  qt.doc.fn <- as.numeric(data %>% filter(nu_ano_censo==year.fn & co_entidade==sl) %>%
                            mutate(qt_doc = qt_doc_fund_ai + qt_doc_fund_af + qt_doc_med + 
                                     qt_doc_prof + qt_doc_prof_tec + qt_doc_eja_fund + qt_doc_eja_med) %>%
                            dplyr::select(qt_doc))
  
  j=1
  while (is.na(qt.doc.fn)) {
    year.fn <- year(year.fn)-j
    qt.doc.fn <- as.numeric(data %>% filter(nu_ano_censo==year.fn & co_entidade==sl) %>%
                              mutate(qt_doc = qt_doc_fund_ai + qt_doc_fund_af + qt_doc_med + 
                                       qt_doc_prof + qt_doc_prof_tec + qt_doc_eja_fund + qt_doc_eja_med) %>%
                              dplyr::select(qt_doc))
    j=j+1
    }
  
  qt.doc.11 <- as.numeric(schools.2011.qt.docentes[schools.2011.qt.docentes$co_entidade==sl,"qt_doc_11"])
  
  m=2012
  while (qt.doc.11==0 | is.na(qt.doc.11)) {
    qt.doc.11 <- as.numeric(data %>% filter(nu_ano_censo==m & co_entidade==sl) %>%
                              mutate(qt_doc = qt_doc_fund_ai + qt_doc_fund_af + qt_doc_med + 
                                       qt_doc_prof + qt_doc_prof_tec + qt_doc_eja_fund + qt_doc_eja_med) %>%
                              dplyr::select(qt_doc))
    m=m+1
    if(m==2022) break
    }
  
  if(qt.doc.11==0 | is.na(qt.doc.11)) next
  
  schools.2011.qt.docentes[schools.2011.qt.docentes$co_entidade==sl,"dqt_doc"] <- (qt.doc.fn - qt.doc.11)/qt.doc.11
  
}

### checking
#nrow(schools.2011.qt.docentes[is.na(schools.2011.qt.docentes$dqt_doc),])
### there are 2893 school with no information about 'teachers'

rm(year.fn); rm(qt.doc.fn); rm(qt.doc.11); rm(j); rm(m); gc()

## joiing variables
data.wide <- data.wide %>% left_join(schools.2011.qt.rooms) %>%
  left_join(schools.2011.qt.matriculas) %>%
  left_join(schools.2011.qt.docentes)

### checking
#View(data.wide)
#teste <- data.wide %>% filter(is.na(dqt_mat))
#unique(teste$in_nivel_ensino)
#unique(teste$in_eja)
#unique(teste$tp_localizacao)

## there are 2893 school with no information about 'matriculas' and 'teachers'
## they also have no information about 'nivel de ensino'
## excluding NAs
data.wide <- data.wide %>% filter(!is.na(dqt_mat))

## saving
write.csv(data.wide, "dados/input_survival_schools_all.csv", row.names = F)
#data.wide <- read.csv("dados/input_survival_schools_all.csv")
#data.wide <- data.wide %>% 
#  mutate(
#    first_date = ymd(first_date, truncated = 2L),
#    last_fup_date = ymd(last_fup_date, truncated = 2L),
#    sg_uf = factor(sg_uf, levels=c("AC","AM","AP","MA","MT","PA","RO","RR","TO")),
#    co_municipio = factor(co_municipio),
#    tp_localizacao = factor(tp_localizacao, levels=c("Urban","Rural")),
#    tp_localizacao2 = factor(tp_localizacao2, levels=c("Urban","Settlement", "Indigenous", "Quilombo", "SUU", "Rural")),
#    in_nivel_ensino = factor(in_nivel_ensino, levels=c("F1", "F2", "F1.2", "M", "F1M", "F2M", "F1.2M","PTE"))
#  )

## summary
data.wide %>% group_by(sg_uf, tp_localizacao2) %>% summarise(N=n()) %>% ggplot(aes(x=sg_uf, y=N)) + 
  geom_bar(aes(fill=tp_localizacao2), stat = "identity", position = position_dodge(.8), width = .7)+
  scale_fill_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000"))

data.wide %>% group_by(sg_uf, in_nivel_ensino) %>% summarise(N=n()) %>% ggplot(aes(x=sg_uf, y=N)) + 
  geom_bar(aes(fill=in_nivel_ensino), stat = "identity", position = position_dodge(.8), width = .7)+
  scale_fill_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe"))

# Creating survival objects and curves =========================================
## considering both full closure and paralysis as an event
lato.stop <- data.wide %>% mutate(status = if_else(status==0, status, 1))

## considering either full closure or paralysis as an event
stricto.stop <- data.wide %>% mutate(last_fup_date = if_else(status==1, ymd("2022-01-01"), last_fup_date))
stricto.stop <- stricto.stop %>% mutate(duration = as.duration(first_date %--% last_fup_date) / dyears(1))
stricto.stop <- stricto.stop %>% mutate(status = if_else(status==2, 1, 0))

# checking
# Kaplan Meier Survival Curve
km <- with(stricto.stop, Surv(duration, status))
head(km,80)



## creates survival curves using the Kaplan-Meier method
s1 <- survfit(Surv(duration, status) ~ 1, data = stricto.stop)
str(s1)
summary(s1)


s1 %>% 
  ggsurvfit() +
  scale_x_continuous(
    breaks = 0:11,
    label = 2011:2022
  ) +
  scale_y_continuous(limits = c(.7,1))+
  labs(
    title = "Full closure",
    x = "Years",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()


##estimate the probability of surviving to 3 year
#s1 %>% 
#  tbl_survfit(
#    times = 3,
#    label_header = "**3-year survival (95% CI)**"
#  )


## comparing survival times between 'nivel de ensino'
s2 <- coxph(Surv(duration, status) ~ in_nivel_ensino, data = stricto.stop)
#str(s2)
summary(s2)
car::Anova(s2)
lsm.s2 <- emmeans::emmeans(s2, ~ in_nivel_ensino, infer=T, level = .95, adjust = "bonferroni", type="response")
summary(lsm.s2)
#pairs(lsm.s2)
mc.s2 <- multcomp::cld(lsm.s2, alpha=0.05, Letters=letters)

mc.s2 %>% ggplot(aes(x = in_nivel_ensino, y = response, color=in_nivel_ensino))+
  geom_point(show.legend = F)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .15, show.legend = F)+
  scale_x_discrete(limits=rev)+coord_flip()+
  scale_color_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe"))+
  annotate("text", x = mc.s2$in_nivel_ensino, y = mc.s2$asymp.UCL+0.5, label = gsub(" ", "", mc.s2$.group))


survfit2(Surv(duration, status) ~ in_nivel_ensino, data = stricto.stop) %>% 
  ggsurvfit() +
  scale_x_continuous(
    breaks = 0:11,
    label = 2011:2022
  ) +
  scale_y_continuous(limits = c(.7,1))+
  scale_color_manual(
    values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe")
  ) +
  #scale_fill_manual(
  #  values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe")
  #) +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) +
  #add_confidence_interval() +
  #add_risktable()+
  theme(legend.position = c(.07,.3))



### considering only 'ensino fundamental' schools
lato.stop.ef <- lato.stop %>% filter(in_nivel_ensino %in% c("F1", "F2", "F1.2"))
stricto.stop.ef <- stricto.stop %>% filter(in_nivel_ensino %in% c("F1", "F2", "F1.2"))


## comparing survival times between location
s3 <- coxph(Surv(duration, status) ~ tp_localizacao2, data = stricto.stop.ef)
#str(s3)
summary(s3)
car::Anova(s3)
lsm.s3 <- emmeans::emmeans(s3, ~ tp_localizacao2, infer=T, level = .95, adjust = "bonferroni", type="response")
summary(lsm.s3)
#pairs(lsm.s3)
mc.s3 <- multcomp::cld(lsm.s3, alpha=0.05, Letters=letters)

mc.s3 %>% ggplot(aes(x = tp_localizacao2, y = response, color=tp_localizacao2))+
  geom_point(show.legend = F)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .15, show.legend = F)+
  scale_x_discrete(limits=rev)+coord_flip()+
  scale_color_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000"))+
  annotate("text", x = mc.s3$tp_localizacao2, y = mc.s3$asymp.UCL+0.5, label = gsub(" ", "", mc.s3$.group))


survfit2(Surv(duration, status) ~ tp_localizacao2, data = stricto.stop.ef) %>% 
  ggsurvfit() +
  scale_x_continuous(
    breaks = 0:11,
    label = 2011:2022
  ) +
  scale_y_continuous(limits = c(.7,1))+
  scale_color_manual(
    values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000")
  ) +
  #scale_fill_manual(
  #  values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000")
  #) +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) +
  #add_confidence_interval() +
  #add_risktable()+
  theme(legend.position = c(.09,.3))


### considering only rural area schools with 'ensino fundamental'
lato.stop.ef.ru <- lato.stop.ef %>% filter(tp_localizacao=="Rural") %>% droplevels()
stricto.stop.ef.ru <- stricto.stop.ef %>% filter(tp_localizacao=="Rural") %>% droplevels()


## comparing survival times considering municipality (clusters) and location (strata)
## The cluster() function is used to specify non-independent cases (such as several individuals in the same municipality), 
## and the strata() function may be used to divide the data into sub-groups with potentially different baseline hazard functions
### first create categories by dissecting the "dqt_*" variables into class interval
stricto.stop.ef.ru <- stricto.stop.ef.ru %>% 
  mutate(dqt_rooms_cat = cut(dqt_rooms,
                             breaks = c(-1.1, -0.5, -0.0001, 156),
                             labels = c(">50", "50-0", "0-155")),
         dqt_mat_cat = cut(dqt_mat,
                             breaks = c(-1.1, -0.5, -0.0001, 42),
                             labels = c(">50", "50-0", "0-41")),
         dqt_doc_cat = cut(dqt_doc,
                           breaks = c(-1.1, -0.5, -0.0001, 51),
                           labels = c(">50", "50-0", "0-50"))
         )
#xtabs(~ dqt_rooms_cat, data=stricto.stop.ef.ru)


s4a <- coxph(Surv(duration, status) ~ dqt_rooms_cat + dqt_mat_cat + dqt_doc_cat + tp_localizacao2, 
             data = stricto.stop.ef.ru)

s4b <- coxph(Surv(duration, status) ~ dqt_rooms_cat + dqt_mat_cat + dqt_doc_cat + strata(tp_localizacao2), 
             data = stricto.stop.ef.ru)

s4c <- coxph(Surv(duration, status) ~ dqt_rooms_cat + dqt_mat_cat + dqt_doc_cat + tp_localizacao2, 
             cluster = co_municipio, data = stricto.stop.ef.ru)

#tests and graphical diagnostics for proportional hazard
par(mfrow=c(2, 2))
cox.zph(s4a)
plot(cox.zph(s4a))
#
#

cox.zph(s4b)
plot(cox.zph(s4b))
#
#

cox.zph(s4c)
plot(cox.zph(s4c))




#str(s4a)
summary(s4a)
car::Anova(s4a)
lsm.s4a <- emmeans::emmeans(s4a, ~ dqt_doc_cat, infer=T, level = .95, adjust = "bonferroni", type="response")
summary(lsm.s4a)
#pairs(lsm.s4a)
mc.s4a <- multcomp::cld(lsm.s4a, alpha=0.05, Letters=letters)

mc.s4a %>% ggplot(aes(x = dqt_doc_cat, y = response, color=dqt_doc_cat))+
  geom_point(show.legend = F)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .15, show.legend = F)+
  scale_x_discrete(limits=rev)+coord_flip()+
  scale_color_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000"))+
  annotate("text", x = mc.s4a$dqt_doc_cat, y = mc.s4a$asymp.UCL+0.0000000001, label = gsub(" ", "", mc.s4a$.group))


survfit2(Surv(duration, status) ~ tp_localizacao2, data = stricto.stop.ef.ru) %>% 
  ggsurvfit() +
  scale_x_continuous(
    breaks = 0:11,
    label = 2011:2022
  ) +
  scale_y_continuous(limits = c(.7,1))+
  scale_color_manual(
    values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000")
  ) +
  #scale_fill_manual(
  #  values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000")
  #) +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) +
  #add_confidence_interval() +
  #add_risktable()+
  theme(legend.position = c(.09,.3))



































## comparing survival times between states
s5 <- coxph(Surv(duration, status) ~ sg_uf, data = stricto.stop.ef.ru)
#str(s5)
summary(s5)
car::Anova(s5)
lsm.s5 <- emmeans::emmeans(s5, ~ sg_uf, infer=T, level = .95, adjust = "bonferroni", type="response")
summary(lsm.s5)
#pairs(lsm.s5)
mc.s5 <- multcomp::cld(lsm.s5, alpha=0.05, Letters=letters)

mc.s5 %>% ggplot(aes(x = sg_uf, y = response, color=sg_uf))+
  geom_point(show.legend = F)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .15, show.legend = F)+
  scale_x_discrete(limits=rev)+coord_flip()+
  scale_color_manual(values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000"))+
  annotate("text", x = mc.s5$tp_localizacao2, y = mc.s5$asymp.UCL+0.5, label = gsub(" ", "", mc.s5$.group))


survfit2(Surv(duration, status) ~ sg_uf, data = stricto.stop.ef.ru) %>% 
  ggsurvfit() +
  scale_x_continuous(
    breaks = 0:11,
    label = 2011:2022
  ) +
  scale_color_manual(
    values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe", "#590080")
  ) +
  #scale_fill_manual(
  #  values = c("#007f0e", "#001280", "#ffd800", "black", "#b100fe", "#fe0000", "#fe6a00", "#0094fe", "#590080")
  #) +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) +
  #add_confidence_interval() +
  #add_risktable() +
  #add_risktable_strata_symbol(symbol = "\U25CF", size = 10)+
  theme(legend.position = c(.05,.3))











####==================================|


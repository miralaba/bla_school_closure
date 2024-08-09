## AM education script june 2023

#library(dplyr)
#library(tidyr)
library(tidyverse)
library(ggmap)
library(dotenv)
load_dot_env("scripts/googleAPI.env")
register_google(Sys.getenv("API_KEY"))




# 2022
data2022 <- read.csv("dados/raw/microdados_ed_basica_2022.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2022) <- tolower(names(data2022))
#unique(data2022$sg_uf)
am.list <- c("RO","AC","AM","RR","PA","AP","TO","MA","MT")
am2022 <- subset (data2022,  data2022$sg_uf %in% am.list)

am2022 <- dplyr::select(am2022, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2022)

#unique(am2022$no_uf)
am2022$no_uf[am2022$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2022$no_uf[am2022$no_uf=="Par\xe1"] <- "Para"
am2022$no_uf[am2022$no_uf=="Amap\xe1"] <- "Amapa"
am2022$no_uf[am2022$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2022$tp_situacao_funcionamento)
anyNA(am2022$tp_situacao_funcionamento)

### 2021
data2021 <- read.csv("dados/raw/microdados_ed_basica_2021.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2021) <- tolower(names(data2021))
#unique(data2021$sg_uf)
am2021 <- subset (data2021,  data2021$sg_uf %in% am.list)

am2021 <- dplyr::select(am2021, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2021)

#unique(am2021$no_uf)
am2021$no_uf[am2021$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2021$no_uf[am2021$no_uf=="Par\xe1"] <- "Para"
am2021$no_uf[am2021$no_uf=="Amap\xe1"] <- "Amapa"
am2021$no_uf[am2021$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2021$tp_situacao_funcionamento)
anyNA(am2021$tp_situacao_funcionamento)

#2020
data2020 <- read.csv("dados/raw/microdados_ed_basica_2020.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2020) <- tolower(names(data2020))
#unique(data2020$sg_uf)
am2020 <- subset (data2020,  data2020$sg_uf %in% am.list)

am2020 <- dplyr::select(am2020, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2020)

#unique(am2020$no_uf)
am2020$no_uf[am2020$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2020$no_uf[am2020$no_uf=="Par\xe1"] <- "Para"
am2020$no_uf[am2020$no_uf=="Amap\xe1"] <- "Amapa"
am2020$no_uf[am2020$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2020$tp_situacao_funcionamento)
anyNA(am2020$tp_situacao_funcionamento)

# 2019
data2019 <- read.csv("dados/raw/microdados_ed_basica_2019.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2019) <- tolower(names(data2019))
#unique(data2019$sg_uf)
am2019 <- subset (data2019,  data2019$sg_uf %in% am.list)

am2019 <- dplyr::select(am2019, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2019)

#unique(am2019$no_uf)
am2019$no_uf[am2019$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2019$no_uf[am2019$no_uf=="Par\xe1"] <- "Para"
am2019$no_uf[am2019$no_uf=="Amap\xe1"] <- "Amapa"
am2019$no_uf[am2019$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2019$tp_situacao_funcionamento)
anyNA(am2019$tp_situacao_funcionamento)

# 2018
data2018 <- read.csv("dados/raw/microdados_ed_basica_2018.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2018) <- tolower(names(data2018))
#unique(data2018$sg_uf)
am2018 <- subset (data2018,  data2018$sg_uf %in% am.list)

am2018 <- dplyr::select(am2018, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2018)

#unique(am2018$no_uf)
am2018$no_uf[am2018$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2018$no_uf[am2018$no_uf=="Par\xe1"] <- "Para"
am2018$no_uf[am2018$no_uf=="Amap\xe1"] <- "Amapa"
am2018$no_uf[am2018$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2018$tp_situacao_funcionamento)
anyNA(am2018$tp_situacao_funcionamento)

# 2017
data2017 <- read.csv("dados/raw/microdados_ed_basica_2017.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2017) <- tolower(names(data2017))
#unique(data2017$sg_uf)
am2017 <- subset (data2017,  data2017$sg_uf %in% am.list)

am2017 <- dplyr::select(am2017, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2017)

#unique(am2017$no_uf)
am2017$no_uf[am2017$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2017$no_uf[am2017$no_uf=="Par\xe1"] <- "Para"
am2017$no_uf[am2017$no_uf=="Amap\xe1"] <- "Amapa"
am2017$no_uf[am2017$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2017$tp_situacao_funcionamento)
anyNA(am2017$tp_situacao_funcionamento)

#2016
data2016 <- read.csv("dados/raw/microdados_ed_basica_2016.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2016) <- tolower(names(data2016))
#unique(data2016$sg_uf)
am2016 <- subset (data2016,  data2016$sg_uf %in% am.list)

am2016 <- dplyr::select(am2016, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2016)

#unique(am2016$no_uf)
am2016$no_uf[am2016$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2016$no_uf[am2016$no_uf=="Par\xe1"] <- "Para"
am2016$no_uf[am2016$no_uf=="Amap\xe1"] <- "Amapa"
am2016$no_uf[am2016$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2016$tp_situacao_funcionamento)
anyNA(am2016$tp_situacao_funcionamento)

# 2015
data2015 <- read.csv("dados/raw/microdados_ed_basica_2015.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2015) <- tolower(names(data2015))
#unique(data2015$sg_uf)
am2015 <- subset (data2015,  data2015$sg_uf %in% am.list)

am2015 <- dplyr::select(am2015, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2015)

#unique(am2015$no_uf)
am2015$no_uf[am2015$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2015$no_uf[am2015$no_uf=="Par\xe1"] <- "Para"
am2015$no_uf[am2015$no_uf=="Amap\xe1"] <- "Amapa"
am2015$no_uf[am2015$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2015$tp_situacao_funcionamento)
anyNA(am2015$tp_situacao_funcionamento)

# 2014
data2014 <- read.csv("dados/raw/microdados_ed_basica_2014.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2014) <- tolower(names(data2014))
#unique(data2014$sg_uf)
am2014 <- subset (data2014,  data2014$sg_uf %in% am.list)

am2014 <- dplyr::select(am2014, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2014)

#unique(am2014$no_uf)
am2014$no_uf[am2014$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2014$no_uf[am2014$no_uf=="Par\xe1"] <- "Para"
am2014$no_uf[am2014$no_uf=="Amap\xe1"] <- "Amapa"
am2014$no_uf[am2014$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2014$tp_situacao_funcionamento)
anyNA(am2014$tp_situacao_funcionamento)

# 2013
data2013 <- read.csv("dados/raw/microdados_ed_basica_2013.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2013) <- tolower(names(data2013))
#unique(data2013$sg_uf)
am2013 <- subset (data2013,  data2013$sg_uf %in% am.list)

am2013 <- dplyr::select(am2013, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2013)

#unique(am2013$no_uf)
am2013$no_uf[am2013$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2013$no_uf[am2013$no_uf=="Par\xe1"] <- "Para"
am2013$no_uf[am2013$no_uf=="Amap\xe1"] <- "Amapa"
am2013$no_uf[am2013$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2013$tp_situacao_funcionamento)
anyNA(am2013$tp_situacao_funcionamento)

# 2012
data2012 <- read.csv("dados/raw/microdados_ed_basica_2012.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2012) <- tolower(names(data2012))
#unique(data2012$sg_uf)
am2012 <- subset (data2012,  data2012$sg_uf %in% am.list)

am2012 <- dplyr::select(am2012, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2012)

#unique(am2012$no_uf)
am2012$no_uf[am2012$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2012$no_uf[am2012$no_uf=="Par\xe1"] <- "Para"
am2012$no_uf[am2012$no_uf=="Amap\xe1"] <- "Amapa"
am2012$no_uf[am2012$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2012$tp_situacao_funcionamento)
anyNA(am2012$tp_situacao_funcionamento)

# 2011
data2011 <- read.csv("dados/raw/microdados_ed_basica_2011.csv", encoding = "Latin-ASCII",
                     stringsAsFactors = FALSE, sep=";", header=TRUE)
names(data2011) <- tolower(names(data2011))
#unique(data2011$sg_uf)
am2011 <- subset (data2011,  data2011$sg_uf %in% am.list)

am2011 <- dplyr::select(am2011, nu_ano_censo, 
                        no_entidade, co_entidade,
                        no_uf, sg_uf, no_mesorregiao, co_mesorregiao, no_municipio, co_municipio, 
                        tp_localizacao, tp_localizacao_diferenciada,
                        ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep,
                        tp_situacao_funcionamento,qt_salas_utilizadas, 
                        in_fund_ai, in_fund_af, in_med, 
                        in_prof, in_prof_tec, in_eja_fund, in_eja_med,
                        qt_mat_fund_ai, qt_mat_fund_af, qt_mat_med,
                        qt_mat_prof, qt_mat_prof_tec, qt_mat_eja_fund, qt_mat_eja_med,
                        qt_doc_fund_ai, qt_doc_fund_af, qt_doc_med,
                        qt_doc_prof, qt_doc_prof_tec, qt_doc_eja_fund, qt_doc_eja_med)

rm(data2011)

#unique(am2011$no_uf)
am2011$no_uf[am2011$no_uf=="Rond\xf4nia"] <- "Rondonia"
am2011$no_uf[am2011$no_uf=="Par\xe1"] <- "Para"
am2011$no_uf[am2011$no_uf=="Amap\xe1"] <- "Amapa"
am2011$no_uf[am2011$no_uf=="Maranh\xe3o"] <- "Maranhao"

table(am2011$tp_situacao_funcionamento)
anyNA(am2011$tp_situacao_funcionamento)


schooloc.df <- ifelse(str_detect(am2011$ds_endereco, pattern = "TERRA"),
                      paste(am2011$ds_complemento, #am2011$no_entidade, 
                            am2011$no_municipio, am2011$no_uf, "Brasil", sep = ","),
                      paste(am2011$ds_endereco, am2011$nu_endereco, #am2011$no_entidade, 
                            am2011$no_municipio, am2011$no_bairro, am2011$no_uf, "Brasil", sep = ","))

schooloc.df <- as.data.frame(schooloc.df)
schooloc.df$schooloc.df <- str_replace_all(schooloc.df$schooloc.df, "[^a-zA-Z0-9| |,]", " ") %>% tolower()
schooloc.geo <- suppressMessages(schooloc.df %>% mutate_geocode(schooloc.df))
schooloc.geo <- cbind(am2011$co_entidade, schooloc.geo)

write.csv(schooloc.geo, "dados/inep_am2011_local.csv", row.names = F)




# merging the datasets
am <- rbind(am2011, am2012, am2013, am2014, am2015, 
            am2016, am2017, am2018, am2019, am2020, 
            am2021, am2022)

# exporting data
write.table(am, "dados/inep_am.txt", sep="\t")


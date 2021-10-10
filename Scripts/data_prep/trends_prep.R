#### Preparation of trend data for diseases and all-cause mortality


# options(scipen=999)
suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(Hmisc))     # this has the 'describe' function in it


calculateDiseaseTrends <- function(incidence_trends_cancers, mortality_trends_cancers, 
                                   mortality_trends_cancer_2, trends_cvd, grim_books, trends_diabetes,
                                   trends_alzheimer_hospitalizations, trends_alzheimer_mortality,
                                   trends_ihme) {
  
  
  ### Data
   
  # incidence_trends_cancers="Data/original/aihw/cancer_incidence_AIHW_with_projections.xlsx"
  # mortality_trends_cancers_2="Data/original/aihw/cancer_mortality_AIHW_with_projections.xlsx"
  # mortality_trends_cancers="Data/original/aihw/cancers_trends_mortality_aihw.xls"
  # trends_cvd="Data/original/aihw/cardiovascular_disease_trends_aihw.xlsx"
  # grim_books="Data/original/aihw/grim_books_utf8.csv"
  # trends_diabetes="Data/original/aihw/diabetes_trends_aihw.xls"
  # trends_alzheimer_hospitalizations="Data/original/aihw/dementia_hospitalisations.xlsx"
  # trends_alzheimer_mortality="Data/original/aihw/dementia_mortality.xlsx"
  # trends_ihme="Data/ihme/year_trends.csv"

  
  
  #### DISEASES
  
  ### Methods
  #### 1) For data with future trends (incidence and mortality cancers): 
  ####    calculate annual change as log(data(t1)/data(t0))/diff(t1,t0).
  ####    Cancers' incidence trends applied to incidence and mortality to case fatality.
  #### 2) For data without future trends (mortality cardiovascular, COPD and diabetes): calculate future trend for 10 years from past data and derive
  ####    annual change as for data with trends. Each disease has a script to derive the trends.
  
  ### 1) Cancers: trend data available from AIHW for incidence and mortality  
  
  ### INCIDENCE TRENDS CANCERS
  
  data_incidence <- readxl::read_xlsx(incidence_trends_cancers, sheet = "Table 1 Incidence Summary", range = cell_rows(6:137973)) %>%
    filter(`Data type` == "Projections", `Age group (years)`=="All ages", Sex != "Persons")
  ### Orignal data forecast from 2017 to 2020
  
  
  ##### FEMALES
  ### Breast
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Breast cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(brsc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  incidence_trends_f <- data_2
  
  ### Uterine
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Uterine cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(utrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Lung 
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Lung cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(tbalc = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Colon
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Colon cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(carc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Kidney cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Kidney cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(kdnc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Bladder cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Bladder cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(bldc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Liver cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Liver cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(lvrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Multiple myeloma
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Multiple myeloma") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(mltm = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Chronic myeloid leukaemia (CML)
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Chronic myeloid leukaemia (CML)") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(chml = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Oesophageal cancer (Esophageal cancer)
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Oesophageal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(espc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Stomach cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Stomach cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(stmc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Head and neck cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Head and neck cancer (with lip)") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(hanc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Rectum cancer
  
  data <- data_incidence %>% filter(Sex == "Females", `Cancer group/site` == "Rectal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(rctc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ##### MALES
  
  ### Lung
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Lung cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(tbalc = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- data_2
  
  ### Colon
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Colon cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(carc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Prostate cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Prostate cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(prsc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Kidney cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Kidney cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(kdnc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Bladder cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Bladder cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(bldc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Liver cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Liver cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(lvrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Multiple myeloma
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Multiple myeloma") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(mltm = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Chronic myeloid leukaemia (CML)
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Chronic myeloid leukaemia (CML)") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(chml = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Oesophageal cancer (Esophageal cancer)
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Oesophageal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(espc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Stomach cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Stomach cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(stmc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Head and neck cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Head and neck cancer (with lip)") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(hanc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Rectum cancer
  
  data <- data_incidence %>% filter(Sex == "Males", `Cancer group/site` == "Rectal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(rctc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### MORTALITY TRENDS CANCERS
  
  #### FEMALES
  
  ### Breast
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Breast", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(brsc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- data_2
  
  ### Uterine
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Uterine", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(utrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Lung
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(tbalc = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Colorectal cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(carc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Kidney cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Kidney", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(kdnc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Bladder cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Bladder", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(bldc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Liver cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Liver", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(lvrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  
  ### Multiple myeloma
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Multiple Myeloma", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(mltm = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Chronic myeloid leukemia
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "CML", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(chml = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Esophageal cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Oesophageal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(espc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Stomach cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Stomach", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(stmc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Head and neck cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Head and neck", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change =log(data[[13,8]]/data[[2,8]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(hanc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Rectum cancer
  
  data_mortality <- readxl::read_xlsx(mortality_trends_cancers_2, sheet = "Table 2 Mortality Summary", range = cell_rows(6:137973)) %>%
    filter(Year %in% c(2017, 2018, 2019, 2020), `Age group (years)`=="All ages", Sex != "Persons")
 
  data <- data_mortality %>% filter(Sex == "Females", `Cancer group/site` == "Rectal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
    mutate(rctc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### MALES
  
  ### Lung
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(tbalc = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- data_2
  
  ### Colorectal cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(carc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Kidney cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Kidney", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(kdnc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Bladder cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Bladder", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(lvrc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Liver cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Liver", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(bldc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  
  ### Multiple myeloma
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Multiple Myeloma", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(mltm = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Chronic myeloid leukemia
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "CML", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(chml = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Esophageal cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Oesophageal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(espc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Stomach cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Stomach", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(stmc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Head and neck cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Head and neck", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(hanc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Prostate cancer
  
  data <- readxl::read_xls(mortality_trends_cancers, sheet = "Prostate", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)
  
  value_change=log(data[[13,3]]/data[[2,3]])
  
  value_year=data[[13,1]]-data[[2,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(prsc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Rectum cancer
  
  data_mortality <- readxl::read_xlsx(mortality_trends_cancers_2, sheet = "Table 2 Mortality Summary", range = cell_rows(6:137973)) %>%
    filter(Year %in% c(2017, 2018, 2019, 2020), `Age group (years)`=="All ages", Sex != "Persons")
  
  data <- data_mortality %>% filter(Sex == "Males", `Cancer group/site` == "Rectal cancer") %>%
    dplyr::select(`Age-standardised rate\r\n(per 100,000)`, Year) %>%
    mutate_all(as.numeric)
  
  value_change=log(data[[4,1]]/data[[1,1]])
  
  value_year=(data[[4,2]] - data[[1,2]])
  
  data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
    mutate(rctc = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  
  ### 2) Data without trends: copd, cvd and diabetes. Trends derived by Lucy Gunn (see code in folder XX, here summary code).
  ### Linear regressions for increasing trends and loglinear for decreasing. 
  
  #### Incidence
  
  ### Cardiovascular diseases 
  #### Use hospitalizations
  data <- readxl::read_xlsx(trends_cvd, sheet = "Table 2.9", range = "B8:H26") %>%mutate(Year = 2001:2018)
  
  
  ### Females                                                                     
  Females<-lm(data$Females...6~data$Year, data=data)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(2001,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFem
  
  FitFore.Fem<-data.frame(new_YEAR,predictFem) 
  
  value_change=log(FitFore.Fem[[23,2]]/FitFore.Fem[[18,2]])
  
  value_year=FitFore.Fem[[23,1]]-FitFore.Fem[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(ishd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year))) %>%
    mutate(strk=ishd)
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Males                                                                   
  Males<-lm(data$Males...5~data$Year, data=data)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(2001,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMale
  
  FitFore.Males<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Males[[23,2]]/FitFore.Males[[18,2]])
  
  value_year=FitFore.Males[[23,1]]-FitFore.Males[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(ishd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year))) %>%
    mutate(strk=ishd)
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### COPD
  ### Data for mortality up to year 2017, predict up to year 2023
  
  data <- read.csv(grim_books) %>%
    dplyr::filter(AGE_GROUP == "Total", SEX != "Persons", YEAR >= 2005,
                  cause_of_death == "Chronic obstructive pulmonary disease (COPD) (ICD-10 J40–J44)") %>%
    dplyr::select(YEAR, SEX, age_standardised_rate)
  
  data_female <- data %>% dplyr::filter(SEX == "Females")
  
  data_female$YEAR <- as.numeric(gsub("\\.", "", data_female$YEAR))
  data_female$age_standardised_rate <- as.numeric(gsub("\\.", "", data_female$age_standardised_rate))
  
  
  #### Females
  # Run the regression
  Females<-lm(data_female$age_standardised_rate~data_female$YEAR)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(2005,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  
  
  FitFore.Fem<-data.frame(new_YEAR,predictFem)  # combines data into one dataframe so it's easier to plot
  
  # Use projectd data to derive annual change
  
  value_change=log(FitFore.Fem[[19,2]]/FitFore.Fem[[13,2]])
  
  value_year=FitFore.Fem[[19,1]]-FitFore.Fem[[13,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(copd = ifelse(year <= value_year,
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  ### Apply trend to incidence
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  
  #### Males
  data_male <- data %>% filter(SEX == "Males")
  
  data_male$YEAR <- as.numeric(gsub("\\.", "", data_male$YEAR))
  data_male$age_standardised_rate <- as.numeric(gsub("\\.", "", data_male$age_standardised_rate))
  
  # Run the regression
  Males<-lm(data_male$age_standardised_rate~data_male$YEAR)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(2005,2022,1))   # creates a sequence for YEAR but include an additional 10 years
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  
  
  FitFore.Male<-data.frame(new_YEAR,predictMale)  # combines data into one dataframe so it's easier to plot
  
  # Use projectd data to derive annual change
  
  
  value_change=log(FitFore.Male[[18,2]]/FitFore.Male[[13,2]])
  
  value_year=FitFore.Male[[18,1]]-FitFore.Male[[13,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(copd = ifelse(year <= value_year,
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  ### Apply trend to incidence
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  
  #### Diabetes
  ### diabetes all includes diabetes as:underlying or associated cause of death
  data <-  readxl::read_xls(trends_diabetes,  sheet = "Table 3.1a & Table 3.1b", range = "B8:E30")
  
  ### Females
  
  Females<-lm(data$Females~data$Year, data=data)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(1997,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFem
  FitFore.Fem<-data.frame(new_YEAR,predictFem)  # combines data into one dataframe so it's easier to plot
  
  # Use projectd data to derive annual change
  
  value_change=log(FitFore.Fem[[27,2]]/FitFore.Fem[[22,2]])
  
  value_year=FitFore.Fem[[27,1]]-FitFore.Fem[[22,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(dmt2 = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  ### Apply trend to incidence
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  
  ### Males
  
  Males<-lm(data$Males~data$Year, data=data)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(1997,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMale
  FitFore.Males<-data.frame(new_YEAR,predictFem)  # combines data into one dataframe so it's easier to plot
  
  # Use projectd data to derive annual change
  
  value_change=log(FitFore.Males[[27,2]]/FitFore.Males[[22,2]])
  
  value_year=FitFore.Males[[27,1]]-FitFore.Males[[22,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(dmt2 = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))
  
  ### Apply trend to incidence
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Alzheimer diseases (data for all dementia)
  
  ### Females (increasing trend)
  
  data <-  readxl::read_excel(trends_alzheimer_hospitalizations,  sheet = "S9.2", range = "A3:G13") %>%
    mutate(year=seq(2010:2019))
  
  ### Females                                                                     
  Females<-lm(data$Women...6~data$year, data=data)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFem
  
  FitFore.Fem<-data.frame(new_YEAR,predictFem) 
  
  value_change=log(FitFore.Fem[[21,2]]/FitFore.Fem[[18,2]])
  
  value_year=FitFore.Fem[[21,1]]-FitFore.Fem[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(adaod  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ### Males (increasing trend)
  
  data <-  readxl::read_excel(trends_alzheimer_hospitalizations,  sheet = "S9.2", range = "A3:G13") %>%
    mutate(year=seq(2010:2019))
  
  ### Males                                                                   
  Males<-lm(data$Men...5~data$year, data=data)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMale
  
  FitFore.Male<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Male[[21,2]]/FitFore.Male[[18,2]])
  
  value_year=FitFore.Male[[21,1]]-FitFore.Male[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(adaod  = ifelse(year <= value_year, 
                           exp(value_change/value_year* year),
                           exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Depression
  # Use GBD data to derive future trends, not data from AIHW or other aus sources
  
  data <- read.csv(trends_ihme)
  
  ### Females
  ### Parkinson diseases incidence
  
  data_2 <- data %>% filter(measure_name %in% "Incidence",  sex_name %in% "Female", 
                            cause_name %in% "Parkinson's disease") %>%
                    arrange(year)
  
  Females<-lm(data_2$val~data_2$year, data=data_2)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFemales=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFemales
  
  FitFore.Female<-data.frame(new_YEAR,predictFemales) 
  
  value_change=log(FitFore.Female[[21,2]]/FitFore.Female[[18,2]])
  
  value_year=FitFore.Female[[21,1]]-FitFore.Female[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(prkd  = ifelse(year <= value_year, 
                           exp(value_change/value_year* year),
                           exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ###Males 
  
  data <- read.csv(trends_ihme)
  
  data_2 <- data %>% filter(measure_name %in% "Incidence",  sex_name %in% "Male", 
                            cause_name %in% "Parkinson's disease") %>%
    arrange(year)
  
  Males<-lm(data_2$val~data_2$year, data=data_2)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMales=Females$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMales
  
  FitFore.Male<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Male[[21,2]]/FitFore.Male[[18,2]])
  
  value_year=FitFore.Male[[21,1]]-FitFore.Male[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(prkd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  ### Females
  ### Depression incidence
  
  data <- read.csv(trends_ihme)
  
  data_2 <- data %>% filter(measure_name %in% "Incidence",  sex_name %in% "Female", 
                            cause_name %in% "Depressive disorders") %>%
    arrange(year)
  
  Females<-lm(data_2$val~data_2$year, data=data_2)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFemales=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFemales
  
  FitFore.Female<-data.frame(new_YEAR,predictFemales) 
  
  value_change=log(FitFore.Female[[21,2]]/FitFore.Female[[18,2]])
  
  value_year=FitFore.Female[[21,1]]-FitFore.Female[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(dprd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
  ###Males 
  
  data <- read.csv(trends_ihme)
  
  data_2 <- data %>% filter(measure_name %in% "Incidence",  sex_name %in% "Male", 
                            cause_name %in% "Depressive disorders") %>%
    arrange(year)
  
  Males<-lm(data_2$val~data_2$year, data=data_2)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMales=Females$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMales
  
  FitFore.Male<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Male[[21,2]]/FitFore.Male[[18,2]])
  
  value_year=FitFore.Male[[21,1]]-FitFore.Male[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(dprd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))
  
  
  ##### MORTALITY
  #### CARDIOVASCULAR
  #### Mortality trends applied to case fatality and hospitalisation to incidence
  #### log linear regression used for predominantely decreasing trends
  #### 2018 is last year with observed data
  #### Table 3.6 mortality data
  
  data <- readxl::read_xlsx(trends_cvd, sheet = "Table 3.6", range = "B7:E45")
  
  Females<-lm(log(data$Females)~data$Year, data=data)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(1981,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  
  
  # note there are some negative values, and these need to be set to zero for those years, according AIHW protocols for negative trend
  
  predictFem<-exp(predictFem)
  
  # join data into one dataframe for plotting and exporting purposes
  
  FitFore.Fem<-data.frame(new_YEAR,predictFem)
  
  
  ### Derive annual trends
  
  value_change=log(FitFore.Fem[[43,2]]/FitFore.Fem[[38,2]])
  value_year=FitFore.Fem[[43,1]]-FitFore.Fem[[38,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(ishd = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))%>%
    mutate(strk=ishd)
  
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Males
  
  Males<-lm(log(data$Males)~data$Year, data=data)
  
  # create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample
  
  new_YEAR=data.frame(seq(1981,2023,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  
  predictMale <- exp(predictMale)
  
  
  # predictMale<-exp(predictMale)
  
  # join data into one dataframe for plotting and exporting purposes
  
  FitFore.Male<-data.frame(new_YEAR,predictMale)
  
  value_change=log(FitFore.Male[[43,2]]/FitFore.Male[[38,2]])
  value_year=FitFore.Male[[43,1]]-FitFore.Male[[38,1]]
  
  
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(ishd = ifelse(year <= value_year, 
                         exp(value_change/value_year* year),
                         exp(value_change/value_year * value_year)))%>%
    mutate(strk=ishd)
  
  
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  

  mortality_trends_f <- mortality_trends_f %>% arrange(year) %>% ### diabetes 1 as trends only apply to incidence
    mutate(dmt2 = 1)
  mortality_trends_m <- mortality_trends_m %>% arrange(year) %>%
    mutate(dmt2 = 1)
  incidence_trends_f <- incidence_trends_f %>% arrange(year)
  incidence_trends_m <- incidence_trends_m %>% arrange(year)
  
  ### Alzheimer diseases (data for all dementia)
  
  ### Females (increasing trend)
  
  data <-  readxl::read_excel(trends_alzheimer_mortality,  sheet = "S3.3", range = "A3:G13") %>%
    mutate(year=seq(2010:2019))
  
  ### Females                                                                     
  Females<-lm(data$Women...6~data$year, data=data)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFem
  
  FitFore.Fem<-data.frame(new_YEAR,predictFem) 
  
  value_change=log(FitFore.Fem[[21,2]]/FitFore.Fem[[18,2]])
  
  value_year=FitFore.Fem[[21,1]]-FitFore.Fem[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(adaod  = ifelse(year <= value_year, 
                           exp(value_change/value_year* year),
                           exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ### Males (increasing trend)
  
  data <-  readxl::read_excel(trends_alzheimer_mortality,  sheet = "S3.3", range = "A3:G13") %>%
    mutate(year=seq(2010:2019))
  
  ### Males                                                                   
  Males<-lm(data$Men...5~data$year, data=data)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMale=Males$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMale
  
  FitFore.Male<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Male[[21,2]]/FitFore.Male[[18,2]])
  
  value_year=FitFore.Male[[21,1]]-FitFore.Male[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(adaod  = ifelse(year <= value_year, 
                           exp(value_change/value_year* year),
                           exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### Females
  ### Parkinson diseases mortality
  
  data <- read.csv(trends_ihme)
  
  data_2 <- data %>% filter(measure_name %in% "Deaths",  sex_name %in% "Female", 
                            cause_name %in% "Parkinson's disease") %>%
    arrange(year)
  
  Females<-lm(data_2$val~data_2$year, data=data_2)
  summary(Females)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictFemales=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
  predictFemales
  
  FitFore.Female<-data.frame(new_YEAR,predictFemales) 
  
  value_change=log(FitFore.Female[[21,2]]/FitFore.Female[[18,2]])
  
  value_year=FitFore.Female[[21,1]]-FitFore.Female[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
    mutate(prkd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))
  
  ###Males 
  
  data <- read.csv(trends_ihme)
  
  data_2 <- data %>% filter(measure_name %in% "Deaths",  sex_name %in% "Male", 
                            cause_name %in% "Parkinson's disease") %>%
    arrange(year)
  
  Males<-lm(data_2$val~data_2$year, data=data_2)
  summary(Males)
  
  # create the time trend sequence for use in the model to get predicted values in sample and forecast for out of sample
  
  new_YEAR=data.frame(seq(2010,2030,1))   # creates a sequence for YEAR but include an additional 10 years
  new_YEAR
  predictMales=Females$coefficients[1]+Males$coefficients[2]*new_YEAR
  predictMales
  
  FitFore.Male<-data.frame(new_YEAR,predictMale) 
  
  value_change=log(FitFore.Male[[21,2]]/FitFore.Male[[18,2]])
  
  value_year=FitFore.Male[[21,1]]-FitFore.Male[[18,1]]
  
  data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("male", 101)) %>%
    mutate(prkd  = ifelse(year <= value_year, 
                          exp(value_change/value_year* year),
                          exp(value_change/value_year * value_year)))
  
  mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))
  
  ### no mortality for depression, but add trend of 1 (nothing) othrwise error
  mortality_trends_f$dprd <- 1
  mortality_trends_m$dprd <- 1

  
  return(list(mortality_trends_f, mortality_trends_m, incidence_trends_f, incidence_trends_m))
  
}

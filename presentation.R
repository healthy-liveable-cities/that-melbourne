######################################### SCRIPTS TO PRESENT RESULTS ###############################################

suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(tidyr))


source("./Scripts/functions_presentation.R")

options(scipen=999)

# ---- Functions' inputs -----

# Combinations scenarios for graphs and results
scenariosDF <- crossing(data.frame(max_walk=c(0,1,2)),
                        data.frame(max_cycle=c(0,2,5,10)),
                        data.frame(purpose=c("commuting", "all"))) %>%
  filter(max_walk!=max_cycle) %>%
  mutate(scen=paste0(purpose,"_",max_walk,"_",max_cycle)) %>%
  mutate(title=paste0("Replace ", purpose, " trips ", "up to", " ", ifelse(max_walk>0, max_walk, max_cycle), "km with ",
         ifelse(max_walk>0, "walking", "cycling"), ifelse(max_walk>0 & max_cycle>0, paste(" and between", max_walk,
         "km and ", max_cycle, "km with cycling"), "")))
      

# Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))


# Load data ---------------------------------------------------------------
## select folder for probabilistic or deterministic results
## Deterministic
finalLocation <- "./output/melbourne-outputs"

# finalLocation <- "C:/Users/e95517/Dropbox/that-melbourne/alanoutput180421/melbourne-outputs"
# ### Probabilistic
# # finalLocation <- "./output/probabilistic/melbourne-outputs"



output_df_agg_all <-readRDS(paste0(finalLocation,"/output_df_agg.rds"))
output_diseases_change<-readRDS(paste0(finalLocation,"/output_diseases_change.rds"))
output_life_expectancy_change<-readRDS(paste0(finalLocation,"/output_life_expectancy_change.rds"))
output_life_years_change<-readRDS(paste0(finalLocation,"/output_life_years_change.rds"))
PAall<-readRDS(paste0(finalLocation,"/PAall.rds"))
PAallGuide<-readRDS(paste0(finalLocation,"/PAallGuide.rds"))
output_transport_modes<-readRDS(paste0(finalLocation,"/output_transport_modes.rds")) %>% ## add titles
  left_join(scenariosDF)

# Function inputs select
# age_val: "15-19"  "20-39"  "40-64"  "65plus" "all" 
# sex_val: "all"    "female" "male" 
# scen_cal: "all_0_2", "commuting_0_2", all_0_5", "commuting_0_5", "all_0_10", "commuting_0_10", "all_1_2", 
# "commuting_1_2", "all_1_5", "commuting_1_5", "all_1_10", "commuting_1_10", "all_2_5", "commuting_2_5", "all_2_10", "commuting_2_10"

AGE = "all"
SEX = "all"
SCEN = "all_2_5"

# Transport graph ----------------------------------------------
GraphsMode(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# PA minutes --------------------------------------------------
minutesTable <- minutesTable(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# PA minutes guidelines ---------------------------------------
minutesGuide <- minutesGuide(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# Diseases graphs ---------------------------------------------
## Overall percentage change in incidence over life course
diseasesChangeIncidence(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

## Overall percentage change in mortaility over life course
diseasesChangeDeaths(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

### Incidence diseases change per simulation year
incidenceDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

### Mortality diseases change per simulation year
mortalityDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# Diseases table -------------------------------------------------------------

diseases <- diseasesTable(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# Life years -----------------------------------------------------------------
## Healh Adjusted life years change per simulation year
halyGraph(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

## Healh Adjusted life years change per simulation year
lyGraph(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

# HALYs table ---------------------------------------------------------------

HALYsLYs <- HALYsTable(
  age_val= AGE,
  sex_val= SEX,
  scen_val= SCEN
)

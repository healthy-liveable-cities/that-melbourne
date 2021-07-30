######################################### SCRIPTS TO PRESENT RESULTS ###############################################
rm (list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)



source("./Scripts/functions_presentation.R")

options(scipen=999)

# ---- Functions' inputs -----

# Combinations scenarios for graphs and results

scenariosDF <- crossing(data.frame(max_walk=c(0,1,2)),
                        data.frame(max_cycle=c(0,2,5,10)),
                        data.frame(purpose=c("commuting", "all"))) %>%
  filter(max_walk!=max_cycle) %>%
  mutate(scen=paste0(purpose,"_",max_walk,"_",max_cycle)) %>%
  mutate(title1=paste0(ifelse(max_walk>0, max_walk, max_cycle), "km \u2265 ",
                       ifelse(max_walk>0, "walking", "cycling"), ifelse(max_walk>0 & max_cycle>0, paste(" ;", max_walk, "km",
         "<", "cycling \u2264", max_cycle, "km"), ""))) %>%
  mutate(scen_order=case_when(title1 == "1km \u2265 walking" ~ 1,
                              title1 ==   "2km \u2265 walking" ~ 2,
                              title1 ==  "2km \u2265 cycling" ~ 3, 
                              title1 ==   "5km \u2265 cycling" ~ 4, 
                              title1 ==   "10km \u2265 cycling" ~ 5,
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 2 km" ~ 6, 
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 5 km" ~ 7,
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 10 km" ~ 8, 
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 5 km" ~ 9,
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 10 km" ~10))

# Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))


# Load data ---------------------------------------------------------------

finalLocation <- "./output/melbourne-outputs"



output_df_agg_all <- readRDS(paste0(finalLocation,"/output_df_agg.rds")) %>% ## add titles
  left_join(scenariosDF)
output_diseases_change <- readRDS(paste0(finalLocation,"/output_diseases_change.rds")) %>% ## add titles
  left_join(scenariosDF)
output_life_years_change <- readRDS(paste0(finalLocation,"/output_life_years_change.rds")) %>% ## add titles
  left_join(scenariosDF)
PAall<-readRDS(paste0(finalLocation,"/PAall.rds")) %>% ## add titles
  left_join(scenariosDF)
PAallGuide<-readRDS(paste0(finalLocation,"/PAallGuide.rds")) %>% ## add titles
  left_join(scenariosDF)
output_transport_modes<-readRDS(paste0(finalLocation,"/output_transport_modes.rds")) %>% ## add titles
  left_join(scenariosDF)

#Outputs location
graphsLocation <- "./results/paper/graphs"
dir.create(graphsLocation, recursive=TRUE, showWarnings=FALSE)



tablesLocation <- "./results/paper/tables"
dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)
# Function inputs select


age_val<-c('15-19','20-39', '40-64', '65plus', 'all')
sex_val<- c('all', 'female', 'male')
purp_val<- unique(scenariosDF$purpose)




modes_list <- list()
index <- 1

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
  # 
# AGE="all"
# SEX="all"
# PURP="all"
      
## Create output location for each scenario, age and sex combination
      
graphsLocation <- paste0( "./results/paper/graphs/", AGE, SEX)
dir.create(paste0(graphsLocation), recursive=TRUE, showWarnings=FALSE)
      
      
      
tablesLocation <- paste0("./results/paper/tables/", AGE, SEX)
dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)
      # Function inputs select


# Transport graph ----------------------------------------------
### Problem with data, missing prop
modes_list[[index]] <- GraphsMode(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "mode_", PURP,".tiff"),width=12,height=18)
# 



# PA minutes --------------------------------------------------
minutesT <- minutesTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesT, paste0(tablesLocation, "/", paste0(PURP, ".miuntesTable.csv")))

# PA minutes guidelines ---------------------------------------
minutesG <- minutesGuide(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesG, paste0(tablesLocation, "/", paste0(PURP, ".miuntesGTable.csv")))


# Diseases graphs ---------------------------------------------
## Overall percentage change in incidence over life course
diseasesChangeIncidencePerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "disease_change_perc", PURP, ".png"),width=10,height=6)

## Overall percentage change in mortaility over life course
diseasesChangeDeathsPerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "deaths_change_perc", PURP, ".png"),width=10,height=6)

diseasesChangeIncidenceNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "disease_change_num", PURP, ".png"),width=10,height=6)

## Overall percentage change in mortaility over life course
diseasesChangeDeathsNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "deaths_change_num", PURP, ".png"),width=10,height=6)

### Incidence diseases change per simulation year
incidenceDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "life_disease_change", PURP, ".png"),width=10,height=6)

### Mortality diseases change per simulation year
mortalityDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "life_deaths_change", PURP, ".png"),width=10,height=6)
# Diseases table -------------------------------------------------------------

diseases <- diseasesTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
saveRDS(diseases,paste0(tablesLocation, "/", paste0("disease", PURP,".disease.rds")))
# Life years -----------------------------------------------------------------
## Healh Adjusted life years change per simulation year
halyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "halys", PURP, ".png"),width=10,height=6)

## Healh Adjusted life years change per simulation year
lyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "lys", PURP, ".png"),width=10,height=6)
# HALYs table ---------------------------------------------------------------

HALYsLYs <- HALYsTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
saveRDS(HALYsLYs,paste0(tablesLocation, "/", paste0("HALYsLYs", PURP,".disease.rds")))
index <- index + 1

      }
    }
  }


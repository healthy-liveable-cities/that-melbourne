rm (list = ls())


# ---- Get libraries ----
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(srvyr)
library(stringr)
library(doParallel)
library(ParallelLogger)
library(ithimr)

library(devtools)

# devtools::install_github("ITHIM/ITHIM-R")
# 
# install_github('nathanvan/parallelsugar')

# ---- Create directories -----

# Working directory

scenarioLocation      <- "./scenarios"
scenarioTripsLocation <- "./scenarios/scenarioTrips"
finalLocation     <- "output/melbourne-outputs"

# Local path to result folder
local_dir_path <- "C:/home/"

# Local drive-results (large files)

outputLocation       <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-raw")
combinedLocationDisease     <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined/disease")
combinedLocationLifeYears <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined/LifeYears")
combineLocationOutputAgg <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined/OutputAgg")
# combinedLocationMMETS <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined-mmets")
# summarisedLocation    <- paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-summarised")


# Create directories, in case not created yet
dir.create(outputLocation , recursive=TRUE, showWarnings=FALSE)
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationDisease, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationLifeYears, recursive=TRUE, showWarnings=FALSE)
dir.create(combineLocationOutputAgg, recursive=TRUE, showWarnings=FALSE)
# dir.create(summarisedLocation, recursive=TRUE, showWarnings=FALSE)

# ---- Get scenarios names and data -----

maxDistanceWalk <- c(0,1,2)
maxDistanceCycle <- c(0,2,5,10)

tripPurpose <- c("commuting", "all")

tripPurposeFull <- c("Work,Education",
                     "Leisure,Shopping,Work,Education,Other")

tripPurposeDF <- data.frame(purpose=tripPurpose,
                            purpose_full=tripPurposeFull,
                            stringsAsFactors=FALSE)


scenarios_ShortTrips <- crossing(data.frame(max_walk=maxDistanceWalk),
                                 data.frame(max_cycle=maxDistanceCycle),
                                 data.frame(purpose=tripPurpose)) %>%
  filter(max_walk!=max_cycle) %>%
  inner_join(tripPurposeDF) %>%
  mutate(scenario=paste0(purpose,"_",max_walk,"_",max_cycle))%>%
  mutate(scenario_location=paste0(scenarioLocation,"/",scenario,".csv")) %>%
  mutate(trips_location=paste0(scenarioTripsLocation,"/",scenario,".csv")) %>%
  mutate(output_location=paste0(outputLocation,"/",scenario))

# # ---- Generate persons_matched --- BZ-D: Just run once, already saved in sceanrios
# source("Scripts/functions_tripsReplace.R")
# source("Scripts/data_prep/synthetic_pop.R")
# 
# for (i in 1:nrow(scenarios_ShortTrips)){
#   generateMatchedPopulationScenario(
#     output_location="./scenarios",
#     scenario_name=scenarios_ShortTrips[i,]$scenario,
#     in_data="./Data/processed/trips_melbourne.csv",
#     max_walk=scenarios_ShortTrips[i,]$max_walk,
#     max_cycle=scenarios_ShortTrips[i,]$max_cycle,
#     purpose=scenarios_ShortTrips[i,]$purpose_full
#   )
#   cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
# }

# ---- Functions to run model ----
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/run_model.R")
source("Scripts/data_prep/population_prep.R")

# --- Fixed inputs ---

mslt_general="Data/processed/mslt/mslt_df.csv"
death_rate_periodic="Data/processed/mslt/deaths_periodic.csv"
death_rates_projections="Data/processed/mslt/deaths_projections.csv"
population_data="Data/original/abs/population_census.xlsx"
disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"

## Victoria specific

location_deaths_periodic="Victoria"
location_deaths_projections="Victoria"
location_population="Greater Melbourne"


MSLT_DF <- read.csv(mslt_general,as.is=T,fileEncoding="UTF-8-BOM")

death_rate_periodic <- read.csv(death_rate_periodic,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == location_deaths_periodic) %>%
  dplyr::select("sex_age_cat", "mx")
MSLT_DF <- left_join(MSLT_DF, death_rate_periodic)

death_projections <- read.csv(death_rates_projections,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Victoria", assumption == "medium")

population <- GetPopulation(
  population_data=population_data,
  location= location_population)
MSLT_DF <<- left_join(MSLT_DF, population)
MSLT_DF$age <- as.numeric(MSLT_DF$age)


# Age and sex cohorts to model (always run all, results can be viewed by age and sex)
i_age_cohort <<- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
i_sex <<- c("male", "female")

DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")

# From ITHIM-R

## copied from ithimr ithim_load_data
### RRs PA
dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
                            "/dose_response/drpa/extdata")

# load every csv file in the dose response folder
list_of_files <<- list.files(path=dose_response_folder, recursive=TRUE,
                             pattern="\\.csv$", full.names=TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

## DATA FILES FOR MODEL  


DISEASE_SHORT_NAMES <<- read.csv("Data/processed/mslt/disease_names.csv",as.is=T,fileEncoding="UTF-8-BOM")

include <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>% 
  dplyr::filter(physical_activity == 1)

DISEASE_SHORT_NAMES <<- DISEASE_SHORT_NAMES %>%
  dplyr::filter(acronym %in% include$acronym)



# --- Parameters ----

NSAMPLES<<- 1000
PA_DOSE_RESPONSE_QUANTILE <<- T


parameters  <<-  GetParameters(
  MMET_CYCLING=5.8,
  MMET_WALKING=2.5,
  DIABETES_IHD_RR_F= c(2.82, 2.35, 3.38), ### issue when using parameters for uncertainty
  DIABETES_STROKE_RR_F=c(2.28, 1.93, 2.69),
  DIABETES_IHD_RR_M=c(2.16, 1.82, 2.56),
  DIABETES_STROKE_RR_M=c(1.83, 1.60, 2.08))


# ---- Run model ----

 
# print(paste0("iterating through ",nrow(scenarios_ShortTrips)," scenarios at ",Sys.time()))
# number_cores <- max(1,floor(as.integer(detectCores())*0.8))
# cl <- makeCluster(number_cores)
# cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
# seeds<- 1:NSAMPLES
# registerDoParallel(cl)
# start_time = Sys.time()

## non-parallel implementation of outputs
results <- for(seed_current in 965:NSAMPLES){
  for(i in 1:nrow(scenarios_ShortTrips)){
    
    for(p in 1:length(parameters))
      assign(names(parameters)[p],parameters[[p]][[seed_current]],pos=1)
    
    if (file.exists(scenarios_ShortTrips[i,]$scenario_location)){
      print(scenarios_ShortTrips[i,]$scenario_location)
      CalculationModel(output_location=scenarios_ShortTrips[i,]$output_location,
                       persons_matched= read.csv(scenarios_ShortTrips[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM"))
    }
    
  }
}


## Comment out parallel loop
# results <-  foreach::foreach(seed_current=seeds,.export=ls(globalenv())) %:% 
#           
#             foreach::foreach(i=1:nrow(scenarios_ShortTrips),
#                              .combine=rbind,
#                              .verbose=F,
#                              .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr"),
#                              .export=c("calculateMMETSperPerson","CalculationModel","gen_pa_rr",
#                                        "PA_dose_response", "health_burden_2","RunDisease","RunLifeTable")
#   ) %dopar% {
#     for(p in 1:length(parameters))
#      assign(names(parameters)[p],parameters[[p]][[seed_current]],pos=1) 
#     
#     if (file.exists(scenarios_ShortTrips[i,]$scenario_location))
#       CalculationModel(output_location=scenarios_ShortTrips[i,]$output_location,
#                      persons_matched= read.csv(scenarios_ShortTrips[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM"))
#     
#     end_time = Sys.time()
#     end_time - start_time
#     stopCluster(cl)
#     # cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
#   }

### Run one sceanario at the time to see if the nested loop is the problem
# 
# 
# print(paste0("iterating through ",nrow(scenarios_ShortTrips)," scenarios at ",Sys.time()))
# number_cores <- max(1,floor(as.integer(detectCores())*0.8))
# cl <- makeCluster(number_cores)
# cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
# seeds<- 1:NSAMPLES
# registerDoParallel(cl)
# start_time = Sys.time()
# 
# results <-  foreach::foreach(seed_current=seeds,.export=ls(globalenv())) %dopar% {
#     for(p in 1:length(parameters))
#       assign(names(parameters)[p],parameters[[p]][[seed_current]],pos=1) 
#     
#     CalculationModel(output_location=scenarios_ShortTrips[1,]$output_location,
#                      persons_matched= read.csv(scenarios_ShortTrips[1,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM"))
#     
#     end_time = Sys.time()
#     end_time - start_time
#     stopCluster(cl)
#     # cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
#   }

# ---- Summarize results ---------------

# ---- Health outcomes ----
# Combine outputs and save (outputs raw here "C:/dot-hia/output/melbourne-outputs-raw" get combined to here "C:/dot-hia/output/melbourne-outputs-combined")
# Diseases
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/disease/'), ## BZ: changed output_location for outputLocation
                 paste0(combinedLocationDisease,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# Life years
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/life_years/'), ## BZ: changed output_location for outputLocation
                 paste0(combinedLocationLifeYears,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# Over life course
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/output_df_agg/'), ## BZ: changed output_location for outputLocation
                 paste0(combineLocationOutputAgg,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}


# Calculate statistics outputs

output_diseases_change <- CalculateDisease(inputDirectory=paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined/disease"))
output_life_years_change <- CalculateLifeYears(inputDirectory=paste0(local_dir_path, "results/scenarioTripsReplace/melbourne-outputs-combined/LifeYears")) 
## To large, do list and then append list
index <- 1
list_output_agg <- list()
for (i in 1:nrow(scenarios_ShortTrips)) {
list_output_agg[[index]] <- CalculateOutputAgg(paste0(local_dir_path, 
                  "results/scenarioTripsReplace/melbourne-outputs-combined/OutputAgg/", scenarios_ShortTrips[i,]$scenario, ".rds"))
index <- index + 1
}


output_df_agg <- do.call(rbind.data.frame, list_output_agg)
  
# Save results
saveRDS(output_diseases_change,paste0(finalLocation,"/output_diseases_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocation,"/output_life_years_change.rds"))
saveRDS(output_df_agg, paste0(finalLocation,"/output_df_agg.rds"))




# Results below are deterministic, run once only
# ---- Transport -----

print(paste0("summarising transport modes ",nrow(scenarios_ShortTrips)," scenario outputs at ",Sys.time()))
scenarioTrips<-NULL
for (i in 1:nrow(scenarios_ShortTrips)){
  scenarioTripsCurrent<-summariseTransport(scenarios_ShortTrips[i,]$trips_location,
                                           scenarios_ShortTrips[i,]$scenario)
  scenarioTrips<-bind_rows(scenarioTrips,scenarioTripsCurrent) %>%
    dplyr::filter(participant_wt!=0) ## Some weights had 0 value
  cat(paste0("\n combined transport scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n")) }

scenarioTrips <- transportOucomes(scenarioTrips)

saveRDS(scenarioTrips,paste0(finalLocation,"/output_transport_modes.rds"))


# ----- Physical activity -----
PA <- PAOutcomes(scenarioLocation)

saveRDS(PA[["PAall"]],paste0(finalLocation,"/PAall.rds"))
saveRDS(PA[["PAallGuide"]],paste0(finalLocation,"/PAallGuide.rds"))


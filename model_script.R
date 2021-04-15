############################# REPLACE SHORT TRIPS MODEL ######################################################

# Clean Global Environment
rm (list = ls())

# Avoid scientific notation
options(scipen=999)

# Functions for model
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/run_model.R")
source("Scripts/data_prep/population_prep.R")


# Outputs location

# Working directory
scenarioLocation <- "./scenarios"
scenarioTripsLocation <- "./scenarios/scenarioTrips"
finalLocationProb <- "output/probabilistic/melbourne-outputs"
finalLocationDeter <- "output/deterministic/melbourne-outputs"



# Local drive-deterministic (large files)

outputLocationDeter <- "C:/home/results/scenarioTripsReplace/deterministic/melbourne-outputs-raw"
combinedLocationDeter <-  "C:/home/results/scenarioTripsReplace/deterministic/melbourne-outputs-combined"
combinedLocationMMETSDeter <-  "C:/home/results/scenarioTripsReplace/deterministic/melbourne-outputs-combined-mmets"
summarisedLocationDeter <-  "C:/home/results/scenarioTripsReplace/deterministic/melbourne-outputs-summarised"

# Local drive-probabilistic (large files)
outputLocationProb <- "C:/home/results/scenarioTripsReplace/probabilistic/melbourne-outputs-raw"
combinedLocationProb <-  "C:/home/results/scenarioTripsReplace/probabilistic/melbourne-outputs-combined"
combinedLocationMMETSProb <-  "C:/home/results/scenarioTripsReplace/probabilistic/melbourne-outputs-combined-mmets"
summarisedLocationProb <-  "C:/home/results/scenarioTripsReplace/probabilistic/melbourne-outputs-summarised"

# Create directories, in case not created yet

dir.create(finalLocationProb, recursive=TRUE, showWarnings=FALSE)
dir.create(finalLocationDeter, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationDeter, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationProb, recursive=TRUE, showWarnings=FALSE)
dir.create(summarisedLocationDeter, recursive=TRUE, showWarnings=FALSE)
dir.create(summarisedLocationProb, recursive=TRUE, showWarnings=FALSE)


#----- Create scenario files -----
# Applies to both deterministic and probabilistic

## Four sets of files are created: 1) Scenarios; 2) trips file, 3) persons travel and 4) matched population

### 1) Generate scenarios file

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
  mutate(outputLocationProb=paste0(outputLocationProb,"/",scenario)) %>%
  mutate(outputLocationDeter=paste0(outputLocationDeter,"/",scenario))

  


### Generate 2) trips files, 3) persons travel and 4) matched population. One of each per scenario

source("Scripts/functions_tripsReplace.R")
source("Scripts/data_prep/synthetic_pop.R")

print(paste0("iterating through ",nrow(scenarios_ShortTrips)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_ShortTrips)){
  generateMatchedPopulationScenario(
    output_location="./scenarios",
    scenario_name=scenarios_ShortTrips[i,]$scenario,
    in_data="./Data/processed/trips_melbourne.csv",
    max_walk=scenarios_ShortTrips[i,]$max_walk,
    max_cycle=scenarios_ShortTrips[i,]$max_cycle,
    purpose=scenarios_ShortTrips[i,]$purpose_full
  )
  cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# ---- Generate transport and PA outcomes ------

## Same for deterministic and probabilistic models

## Summarise transport and physical activity
### Generates 1) output_transport_modes, 2) PAall, 3) PAallGuides for both deterministic and probabilistic (results are the same, just saves them in the right folder)
print(paste0("summarising transport modes ",nrow(scenarios_ShortTrips)," scenario outputs at ",Sys.time()))
scenarioTrips<-NULL
for (i in 1:nrow(scenarios_ShortTrips)){
  scenarioTripsCurrent<-summariseTransport(scenarios_ShortTrips[i,]$trips_location,
                                           scenarios_ShortTrips[i,]$scenario)
  scenarioTrips<-bind_rows(scenarioTrips,scenarioTripsCurrent) %>%
    dplyr::filter(participant_wt!=0) ## Some weights had 0 value
  cat(paste0("\n combined transport scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}


## Transport summarised outputs
scenario_trips_weighted <- list()
iage <- c(unique(scenarioTrips$age))
isex <- c(unique(scenarioTrips$sex))
iscenario <- c(unique(scenarioTrips$scenario))
iscen <- c(unique(scenarioTrips$scen))
index <- 1
for (a in iage){
  for (s in isex){
    for (sc in iscenario){
      for (scena in iscen) {
        data <- scenarioTrips %>%
          dplyr::filter(age==a, sex==s, scenario==sc, scen==scena)
        mode_share <- data %>%
          srvyr::as_survey_design(weights = participant_wt) %>%
          group_by(mode) %>%
          dplyr::summarize(prop= srvyr::survey_mean(na.rm = T),
                           trips = srvyr::survey_total(na.rm = T),
                           surveyed = srvyr::unweighted(dplyr::n())) %>%
          ungroup() %>%
          dplyr::mutate(age=a,
                        sex=s,
                        scenario=sc,
                        scen=scena) %>%
          dplyr::select(age, sex, scenario, scen, prop, mode)
        scenario_trips_weighted[[index]] <- mode_share
        index <- index +1}}}}

scenarioTrips<- bind_rows(scenario_trips_weighted)

saveRDS(scenarioTrips,paste0(finalLocationProb,"/output_transport_modes.rds"))
saveRDS(scenarioTrips,paste0(finalLocationDeter,"/output_transport_modes.rds"))

## Physical activity summarised outputs
PA_files<-list.files(scenarioLocation,pattern="*.csv",full.names=T)
data<-lapply(PA_files,read.csv,header=T) %>%
  bind_rows() %>%
  dplyr::select(participant_wt,age,sex,ses,mod_total_hr:scen) %>%
  mutate(agegroup= case_when(
    age>=15 & age<=19 ~'15-19',
    age>=20 & age<=39 ~'20-39',
    age>=40 & age<=64 ~'40-64',
    age>=65           ~'65plus')) %>%
  dplyr::select(participant_wt,age=agegroup,sex,ses,mod_total_hr:scen)
dataAll <- bind_rows(
  data,
  data%>%mutate(age='all'),
  data%>%mutate(sex='all'),
  data%>%mutate(age='all',sex='all'))

## Create weighted stats
PA_weighted  <- dataAll %>%
  srvyr::as_survey_design(weights = participant_wt)%>%
  group_by(age, sex, scen) %>%
  dplyr::summarize(walk_base= srvyr::survey_mean(time_base_walking*60),
                   walk_scen = srvyr::survey_mean(time_scen_walking*60),
                   cycle_base= srvyr::survey_mean(time_base_bicycle*60),
                   cycle_scen = srvyr::survey_mean(time_scen_bicycle*60))

saveRDS(PA_weighted, file=paste0(finalLocationProb, "/PAall.rds"))
saveRDS(PA_weighted, file=paste0(finalLocationDeter, "/PAall.rds"))

#### Meets guidelines
PA_weighted  <- dataAll %>% 
  dplyr::mutate(meets_pa_base=ifelse((time_base_walking + walk_rc + time_base_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60 >= 150, 1, 0), 
                meets_pa_scen= ifelse((time_scen_walking + walk_rc + time_scen_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60>= 150, 1, 0))

PA_guide_weighted  <- PA_weighted %>% 
  srvyr::as_survey_design(weights = participant_wt)%>%
  group_by(age, sex, scen) %>%
  dplyr::summarize(meets_base= srvyr::survey_mean(meets_pa_base, na.rm = T), 
                   meets_scen = srvyr::survey_mean(meets_pa_scen, na.rm = T))

saveRDS(PA_guide_weighted, file=paste0(finalLocationProb, "/PAallGuide.rds"))
saveRDS(PA_guide_weighted, file=paste0(finalLocationDeter, "/PAallGuide.rds"))

#----- Run model deterministic -----

# Run model (BZ: problem with matched population, sometimes it says that it cannot find it. )

for (i in 1:nrow(scenarios_ShortTrips)){
CalculationModel(output_location=scenarios_ShortTrips[i,]$outputLocationDeter,
                 persons_matched=read.csv(scenarios_ShortTrips[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM"), 
                 probabilistic = F)
}

# Summarise outputs



# Function to combine outputs
combineOutputs <- function(inputDirectory,outputFile) {
  output_df_files<-list.files(inputDirectory,pattern="*.csv",full.names=T)
  output_df<-do.call(rbind.data.frame, lapply(output_df_files,read.csv,header=T)) %>%
    mutate(run=1)
  saveRDS(output_df, file=outputFile)
}

print(paste0("merging ",nrow(scenarios_ShortTrips)," scenario outputs into single file at ",Sys.time()))
# Combine outputs
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$outputLocationDeter,'/output_df'),
                 paste0(combinedLocationDeter,"/",scenarios_ShortTrips[i,]$scenario,".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# Summarize outputs and save for each scenario (summarisedLocation)
## Saved items include: output_df_aggregate, output_life_expectancy_change, output_life_year_change, output_disease_change

print(paste0("summarising ",nrow(scenarios_ShortTrips)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_ShortTrips)){
  output_df <- readRDS(paste0(combinedLocationDeter,"/",scenarios_ShortTrips[1,]$scenario,".rds"))
  summariseOutputs(scenario_location=paste0(summarisedLocationDeter,"/",scenarios_ShortTrips[i,]$scenario),
                   output_df)
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# combine scenarios into single files 

combineScenarios <- function(summarisedLocation,name) {
  scenario_names<-data.frame(scen=list.files(summarisedLocation)) %>%
    mutate(id=row_number())
  file_locations<-list.files(summarisedLocation,
                             pattern=name,
                             full.names=T,recursive=T)
  output_df<-lapply(file_locations,read.csv,header=T) %>%
    bind_rows(.id="id") %>%
    mutate(id=as.integer(id))
  output_df<-inner_join(scenario_names,output_df,by='id')%>%
    dplyr::select(-id)
  return(output_df)
}

output_df_agg_all <- combineScenarios(summarisedLocationDeter,name="output_df_agg.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_diseases_change <- combineScenarios(summarisedLocationDeter,name="output_diseases_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_expectancy_change <- combineScenarios(summarisedLocationDeter,name="output_life_expectancy_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_years_change <- combineScenarios(summarisedLocationDeter,name="output_life_years_change.csv") %>%
  rename(age=age_group_final,sex=Gender)

# Save outputs for visualisation 

saveRDS(output_df_agg_all,paste0(finalLocationDeter,"/output_df_agg.rds"))
saveRDS(output_diseases_change,paste0(finalLocationDeter,"/output_diseases_change.rds"))
saveRDS(output_life_expectancy_change,paste0(finalLocationDeter,"/output_life_expectancy_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocationDeter,"/output_life_years_change.rds"))



#----- Run model probabilistic

### Run model

suppressPackageStartupMessages(library(doParallel))

print(paste0("iterating through ",nrow(scenarios_ShortTrips)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_ShortTrips)){
  
  number_cores <- max(1,floor(as.integer(detectCores())*0.8))
  cl <- makeCluster(number_cores)
  cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
  persons_matched=read.csv(scenarios_ShortTrips[1,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM")
  seeds<- 1:1000
  registerDoParallel(cl)
  start_time = Sys.time()
  results <- foreach::foreach(seed_current=seeds,
                              .combine=rbind,
                              .verbose=F,
                              .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr"),
                              .export=c("calculateMMETSperPerson","CalculationModel","gen_pa_rr_wrapper",
                                        "GetParamters","GetPopulation","GetStDevRR","health_burden_2",
                                        "RunDisease","RunLifeTable")
  ) %dopar%
    CalculationModel(seed=seed_current,
                     output_location=scenarios_ShortTrips[i,]$outputLocationProb,
                     persons_matched, 
                     probabilistic = T)
  end_time = Sys.time()
  end_time - start_time
  stopCluster(cl)
  cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}


### Save combined outputs (for all scenarios and iterations)

##### Create directories if not created


print(paste0("merging ",nrow()," scenario outputs into single file at ",Sys.time()))

### Combine outputs and save (outputs raw here "C:/dot-hia/output/melbourne-outputs-raw" get combined to here "C:/dot-hia/output/melbourne-outputs-combined")

# Function to combine outputs
combineOutputs <- function(inputDirectory,outputFile) {
  output_df_files<-list.files(inputDirectory,pattern="*.csv",full.names=T)
  output_df<-lapply(output_df_files,read.csv,header=T) %>%
    bind_rows(.id="run") %>%
    mutate(run=as.integer(run))
  saveRDS(output_df, file=outputFile)
}


for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$outputLocationProb,'/output_df'),
                 paste0(combinedLocationProb,"/",scenarios_ShortTrips[i,]$scenario,".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

### Summarize outputs and save for each scenario (summarisedLocation)
#### Saved items include: output_df_aggregate, output_life_expectancy_change, output_life_year_change, output_disease_change

print(paste0("summarising ",nrow(scenarios_ShortTrips)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_ShortTrips)){
  output_df <- readRDS(paste0(combinedLocationProb,"/",scenarios_ShortTrips[i,]$scenario,".rds"))
  summariseOutputs(scenario_location=
                     paste0(summarisedLocationProb,"/",scenarios_ShortTrips[i,]$scenario),
                   output_df)
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# combine scenarios into single files 

combineScenarios <- function(summarisedLocationProb,name) {
  scenario_names<-data.frame(scen=list.files(summarisedLocationProb)) %>%
    mutate(id=row_number())
  file_locations<-list.files(summarisedLocationProb,
                             pattern=name,
                             full.names=T,recursive=T)
  output_df<-lapply(file_locations,read.csv,header=T) %>%
    bind_rows(.id="id") %>%
    mutate(id=as.integer(id))
  output_df<-inner_join(scenario_names,output_df,by='id')%>%
    dplyr::select(-id)
  return(output_df)
}

output_df_agg_all <- combineScenarios(summarisedLocationProb,name="output_df_agg.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_diseases_change <- combineScenarios(summarisedLocationProb,name="output_diseases_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_expectancy_change <- combineScenarios(summarisedLocationProb,name="output_life_expectancy_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_years_change <- combineScenarios(summarisedLocationProb,name="output_life_years_change.csv") %>%
  rename(age=age_group_final,sex=Gender)



saveRDS(output_df_agg_all,paste0(finalLocationProb,"/output_df_agg.rds"))
saveRDS(output_diseases_change,paste0(finalLocationProb,"/output_diseases_change.rds"))
saveRDS(output_life_expectancy_change,paste0(finalLocationProb,"/output_life_expectancy_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocationProb,"/output_life_years_change.rds"))







suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())

### Genate fixed inputs for Melbourne for mslt_code and ithim-r


### Trips data used in mslt_code and ithim-r
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/processed/trips_melbourne.csv", row.names=F, quote=F)

### BZ: not used, default speeds used of 4.8 for walking and 14.5 for cycling. 
### Speed walking and cycling Melbourne 
# source("Scripts/data_prep/trips_prep.R")
# speed_trips_melbourne <- CalculateAgeSexSpeed(
#   in_data="Data/processed/trips_melbourne.csv"
# )
# write.csv(speed_trips_melbourne, "Data/processed/speed_trips_melbourne.csv", row.names=F, quote=F)


### Travel data people used in mslt_code to generate matched population
source("Scripts/data_prep/synthetic_pop.R")
travel_data <- calculateTravelData(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
)

write.csv(travel_data, "Data/processed/travel_data.csv", row.names=F, quote=T)

### PA data people used in mslt_code to generate matched population
persons_pa <- calculatePersonsPA(
  pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv",
  hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"
)
write.csv(persons_pa, "Data/processed/persons_pa.csv", row.names=F, quote=F)

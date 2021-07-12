## Script to check whether any RR is greater for bigger mmet value
## Assumes model_script3.R has already run - giving access to global variables such as DISEASE_INVENTORY

# Load library
library(readr)

# Read file
# Belen, you may need to tweak the path according to your directory structure
X1 <- read_csv("results/scenarioTripsReplace/melbourne-outputs-raw/all_2_0/mmets/1.csv")

# Loop through all diseases for PA
for (dis in DISEASE_INVENTORY %>% filter(physical_activity == 1) %>% dplyr::select(acronym) %>% unlist() %>% as.character()){
  
  # Print number of rows where baseline RR is less than scen1's while baseline's mmet is also less than scen1's
  print(paste(dis, 
              X1 %>% filter(.[[paste0("RR_pa_base_", dis)]] < .[[paste0("RR_pa_scen1_", dis)]] & 
                              scen1_mmet > base_mmet) %>% nrow()))
}

# With parallel loop I was getting non-zeros for some of them:
# [1] "ac 0"
# [1] "ihd 0"
# [1] "neo 552"
# [1] "lc 0"
# [1] "stroke 0"
# [1] "t2d 0"
# [1] "breast 0"
# [1] "colon 0"
# [1] "endo 66"

# With non-parallel loop, I get zeros like this:
# [1] "ac 0"
# [1] "ihd 0"
# [1] "neo 0"
# [1] "lc 0"
# [1] "stroke 0"
# [1] "t2d 0"
# [1] "breast 0"
# [1] "colon 0"
# [1] "endo 0"
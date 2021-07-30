### Derive distributions for RRs


rm (list = ls())

library(dplyr)
library(tidyr)
library(readr)


### Get parameters
source("Scripts/run_model.R")
disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"
DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")


### RRs PA
dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
                            "/dose_response/drpa/extdata")

list_of_files <<- list.files(path=dose_response_folder, recursive=TRUE,
                             pattern="\\.csv$", full.names=TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

### Get parameters

NSAMPLES<<- 2000
PA_DOSE_RESPONSE_QUANTILE <<- T


parameters  <<-  GetParameters(
  MMET_CYCLING=5.8,
  MMET_WALKING=2.5,
  DIABETES_IHD_RR_F= c(2.82, 2.35, 3.38), ### issue when using parameters for uncertainty
  DIABETES_STROKE_RR_F=c(2.28, 1.93, 2.69),
  DIABETES_IHD_RR_M=c(2.16, 1.82, 2.56),
  DIABETES_STROKE_RR_M=c(1.83, 1.60, 2.08))

### Function to calculate quantiles RRs
calculate_distributions <- function(data, relative_risk) {
  
  # 
  # data=coronary_heart_disease_mortality
  # relative_risk="coronary_heart_disease"

  rr_values <- parameters[[paste0("PA_DOSE_RESPONSE_QUANTILE_", relative_risk)]]

  rr_list <- list()
  
  index <- 1
  
  
  for(d in  1:length(rr_values)){
    
    rr_list[[index]]  <- data %>%  
      dplyr::mutate(UQ(sym(paste0("RR_",d))) := qnorm(rr_values[d], RR, (ub-lb)/1.96)) %>% 
      dplyr::mutate(UQ(sym(paste0("Diff_RR",d))) := UQ(sym(paste0("RR_",d))) - 
               lag(UQ(sym(paste0("RR_",d))), 
                   default = first(UQ(sym(paste0("RR_",d)))))) %>%
      dplyr::mutate(name=paste0(relative_risk))
    
  
    index <-  index + 1
    
  }   
return(rr_list=rr_list)
}

### Calculate RRs for all diseases


### Function to plot RRs

library(plyr)
library(ggplot2)

plot_rr <- function(data) {
  
  # data=rr_list
  
  
  relative_risks <- plyr::join_all(data) 
  
  name <- unique(relative_risks$name)
  
  write.csv(relative_risks, file=paste0("./test/",name, ".csv"))      
  
  data_long <-relative_risks %>%
    dplyr::select(dose, RR, lb, ub, starts_with("RR")) %>%
    pivot_longer(cols = RR_1:paste0("RR_", NSAMPLES),
                 names_to = "rr",
                 values_to = "value")
  
  ggplot(data=data_long,
         aes(x=dose, y=value, linetype=rr)) +
    geom_line() +
    theme(legend.position = "none")
  
  ggplot2::ggsave(paste0("./test", "/", name, ".png"))
  
}


## Plot sd

plot_sd <- function(data, name) {
  
  # data=coronary_heart_disease_mortality
  # name="coronary_heart_disease"
  
  
  relative_risks <- data %>%
    mutate(sd=(ub-lb)/1.96,
           name=name) %>%
    dplyr::select(dose, sd, name)
  
  name <- unique(relative_risks$name)


  
  ggplot(data=relative_risks,
         aes(x=dose, y=sd)) +
    geom_line() +
    theme(legend.position = "none")
  
  ggplot2::ggsave(paste0("./test", "/", name, "_sd", ".png"))
  
}
### Do calculations for all diseases

coronary_list <- calculate_distributions(coronary_heart_disease_mortality, "coronary_heart_disease")
plot_coronary <- plot_rr(coronary_list)
plot_sd_coronary <- plot_sd(coronary_heart_disease_mortality, "coronary_heart_disease")

b_cancer_list <- calculate_distributions(breast_cancer_all, "breast_cancer")
plot_b_cancer <- plot_rr(b_cancer_list)
plot_sd_breast <- plot_sd(breast_cancer_all, "breast_cancer")

c_cancer_list <- calculate_distributions(colon_cancer_all, "colon_cancer")
plot_c_cancer <- plot_rr(c_cancer_list)
plot_sd_colon <- plot_sd(colon_cancer_all, "colon_cancer")

e_cancer_list <- calculate_distributions(endometrial_cancer_all, "endometrial_cancer")
plot_e_cancer <- plot_rr(e_cancer_list)
plot_sd_endo <- plot_sd(endometrial_cancer_all, "endometrial_cancer")

l_cancer_list <- calculate_distributions(lung_cancer_all, "lung_cancer")
plot_l_cancer <- plot_rr(l_cancer_list)
plot_sd_lung<- plot_sd(lung_cancer_all, "lung_cancer")

stroke_list<- calculate_distributions(stroke_mortality, "stroke")
plot_stroke <- plot_rr(stroke_list)
plot_sd_stroke <- plot_sd(stroke_mortality, "stroke")

diabetes_list<- calculate_distributions(diabetes_mortality, "diabetes")
plot_diabetes <- plot_rr(diabetes_list)
plot_sd_diabetes <- plot_sd(diabetes_mortality, "diabetes")




### Check examples with problems (change value of q in qnorm (first value) and rr2 for higher dose is lower than rr1)
# Breast cancer 
#dose 0.04487487
rr_1=qnorm(0.90, 0.9997002, (1.0000030-0.9993976)/1.96) #sd=0.0006054
#dose 0.17949950
rr_2=qnorm(0.90, 0.9988014, (1.0000118-0.9975926)/1.96) #sd=0.0024192

rr_1
rr_2
# 
# > rr_1
# [1] 1.000096
# > rr_2
# [1] 1.000383

################## Read PA DR curves ##################
rm(list = ls())
require(ithimr)

# Find global folder in ithimr package
global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
global_path <- paste0(global_path, "/")

# Read disease lookup
DISEASE_INVENTORY <- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))

# Read all DR PA curves
list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

# Set colours for all PA disease
cols <- rainbow(sum(DISEASE_INVENTORY$physical_activity == 1))
# Filter all causes for PA
cause_indices <- which(DISEASE_INVENTORY$physical_activity == 1)

############# sample dose-response curves
# Set sample size
nsamples <- 100
# Initialize PA DR quantile vars using samples
parameters <- ithimr::ithim_setup_parameters(NSAMPLES=nsamples,
                                             PA_DOSE_RESPONSE_QUANTILE=T)

# Shorten range of endometrial and breast cancer to 0.3-0.7
parameters$PA_DOSE_RESPONSE_QUANTILE_endometrial_cancer <- runif(nsamples, 0.3, 0.7)
parameters$PA_DOSE_RESPONSE_QUANTILE_breast_cancer <- runif(nsamples, 0.3, 0.7)

# Create a list to store DR relationship for each cause and quantile
spl_cause <- list()

# Save plots in a file
pdf('test/PA_dose_response_sample.pdf',width=11,height=11)
# Set dimensions of plot window
par(mar=c(5,5,2,1),mfrow=c(3,3))
# Loop through all PA causes
for ( j in 1:length(cause_indices)){
  cause <- as.character(DISEASE_INVENTORY$pa_acronym[cause_indices[j]])
  
  # Select mortality or all (mortality and incidence combined) for all cause
  if (cause %in% c("all_cause", "coronary_heart_disease", "stroke", "diabetes", "total_cancer"))
    doses_vector <- get(paste0(cause, '_mortality')) %>% dplyr::select(dose) %>% unlist()
  else
    doses_vector <- get(paste0(cause, '_all')) %>% dplyr::select(dose) %>% unlist()
  
  # Loop through samples
  for(seed in 1:nsamples){
    # For all parameters quantiles, create a variable
    for(i in 1:length(parameters))
      assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
    # Get PA DR relationship - which returns values according to the current quantile
    return_vector <- ithimr::PA_dose_response(cause = cause,dose = doses_vector)
    
    # Create a df for RR and dose
    temp_df <- data.frame(dose = doses_vector, RR = return_vector)
    # Rename RR by adding sample number to it
    names(temp_df)[2] <- paste0("RR_", seed)
    # Save RR and DF relationship in a list
    spl_cause[[cause]][[seed]] <- temp_df
    
    # Plot dose and RR relationship
    if(seed==1){
      plot(doses_vector, return_vector$rr, type='l', ylim = 0:1, main = cause, 
           ylab ='Relative risk', xlab = 'Dose', frame = F, col = cols[j], 
           xlim = range(doses_vector), cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, lwd = 1)
    }else{
      lines(doses_vector, return_vector$rr, col = cols[j], lwd = 1)
    }
  }
  
  # Reduce list to a single df
  td <- spl_cause[[cause]] %>% purrr::reduce(full_join, by = "dose") %>% as.data.frame()
  # Change wide to long format
  td <- reshape2::melt(td, id.vars="dose")
  
  # Plot individual lines - similar to plot above
  ggplot(td) +
    aes(x = dose, y = value, linetype = variable) + geom_line() +
    labs(x = "dose", y = "RR", title = cause) +
    theme(legend.position = "none")
  # Save cause specific plot in a file
  ggplot2::ggsave(paste0("./test/ithimr-", cause, ".png"))
  
}
# Save pdf in a file
dev.off()
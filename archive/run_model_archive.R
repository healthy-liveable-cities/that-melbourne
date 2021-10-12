## loading copies of the ithm-r functions called

# gen_pa_rr <- function(mmets_pp) {
#   
#   
#   # filtering down to columns with 'mmet' in their name
#   SCEN_SHORT_NAME <- colnames(mmets_pp)[grep("mmet",colnames(mmets_pp))]
#   # removing '_mmet' to find the base scenario and scenario names
#   SCEN_SHORT_NAME <- gsub("_mmet","",SCEN_SHORT_NAME)
#   
#   dose_columns <- match(paste0(SCEN_SHORT_NAME, "_mmet"), 
#                         colnames(mmets_pp))
#   doses_vector <- unlist(data.frame(mmets_pp[, dose_columns]))
#   
#   for (j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 
#                                          1]) {
#     pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
#     pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
#     # Select mortality or all (mortality and incidence combined) for all cause
#     if (pa_dn %in% c("all_cause", "coronary_heart_disease", "stroke", "diabetes", "total_cancer"))
#       dose_vector <- get(paste0(pa_dn, '_mortality')) %>% dplyr::select(dose) %>% unlist()
#     else
#       dose_vector <- get(paste0(pa_dn, '_all')) %>% dplyr::select(dose) %>% unlist()
#     return_vector <- ithimr::PA_dose_response(cause = pa_dn, dose = doses_vector)
#     print(paste("index: ", which(parameters[[paste0("PA_DOSE_RESPONSE_QUANTILE_", pa_dn)]] == get(paste0("PA_DOSE_RESPONSE_QUANTILE_", 
#                                                                                                          pa_dn))), " for ", pa_dn))
#     for (i in 1:length(SCEN_SHORT_NAME)) {
#       scen <- SCEN_SHORT_NAME[i]
#       mmets_pp[[paste("RR_pa", scen, pa_n, sep = "_")]] <- return_vector$rr[(1 + (i - 1) * nrow(mmets_pp)):(i * nrow(mmets_pp))]
#     }
#   }
#   mmets_pp
# }

# PA_dose_response <- function(cause, dose, confidence_intervals = F) {
#   # 
#   # 
#   # # Find global folder in ithimr package
#   # global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
#   # global_path <- paste0(global_path, "/")
#   # 
#   # # Read disease lookup
#   # DISEASE_INVENTORY <- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))
#   # 
#   # # Read all DR PA curves
#   # list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
#   # for (i in 1:length(list_of_files)){
#   #   assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
#   #          readr::read_csv(list_of_files[[i]],col_types = cols()),
#   #          pos = 1)
#   # }
#   
#   # list_of_files <- list.files(path=dose_response_folder, recursive=TRUE,
#   #                             pattern="\\.csv$", full.names=TRUE)
#   # for (i in 1:length(list_of_files)){
#   #   assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
#   #          readr::read_csv(list_of_files[[i]],col_types = cols()),
#   #          pos = 1)}
#   
#   if (sum(is.na(dose)) > 0 || class(dose) != "numeric") {
#     stop("Please provide dose in numeric")
#     
#   }
#   if (!cause %in% c("all_cause", "breast_cancer", "cardiovascular_disease", 
#                     "colon_cancer", "coronary_heart_disease", "diabetes", 
#                     "endometrial_cancer", "heart_failure", "lung_cancer", 
#                     "stroke", "total_cancer")) {
#     stop("Unsupported cause/disease. Please select from \n\n         all_cause \n\n         breast_cancer\n\n         cardiovascular_disease \n\n         colon_cancer \n\n         coronary_heart_disease \n\n         endometrial_cancer \n\n         heart_failure \n\n         lung_cancer \n\n         stroke \n\n         total_cancer")
#   }
#   outcome_type <- ifelse(cause %in% c("lung_cancer", "breast_cancer", 
#                                       "endometrial_cancer", "colon_cancer"), "all", "mortality") 
#   if (cause %in% c("total_cancer", "coronary_heart_disease", 
#                    "breast_cancer", "endometrial_cancer", "colon_cancer")) 
#     dose[dose > 35] <- 35
#   else if (cause == "lung_cancer") 
#     dose[dose > 10] <- 10
#   else if (cause == "stroke") 
#     dose[dose > 32] <- 32
#   else if (cause == "all_cause") 
#     dose[dose > 16.08] <- 16.08
#   fname <- paste(cause, outcome_type, sep = "_")
#   lookup_table <- get(fname)
#   lookup_df <- setDT(lookup_table)
#   rr <- approx(x = lookup_df$dose, y = lookup_df$RR, xout = dose, 
#                yleft = 1, yright = min(lookup_df$RR))$y
#   if (confidence_intervals || PA_DOSE_RESPONSE_QUANTILE == 
#       T) {
#     lb <- approx(x = lookup_df$dose, y = lookup_df$lb, xout = dose, 
#                  yleft = 1, yright = min(lookup_df$lb))$y
#     ub <- approx(x = lookup_df$dose, y = lookup_df$ub, xout = dose, 
#                  yleft = 1, yright = min(lookup_df$ub))$y
#   }
#   if (PA_DOSE_RESPONSE_QUANTILE == T) {
#     rr <- qnorm(get(paste0("PA_DOSE_RESPONSE_QUANTILE_", 
#                            cause)), mean = rr, sd = (ub - lb)/1.96)
#     rr[rr < 0] <- 0
#   }
#   if (confidence_intervals) {
#     return(data.frame(rr = rr, lb = lb, ub = ub))
#   }
#   else {
#     return(data.frame(rr = rr))
#   }
# }

###################################################################################################
#Title: Simulation (data inputs)
#Author: Hailey Park
#Date: September 25, 2023
###################################################################################################

hosp_death_age_stratified <- read.csv("data/clean-data/hosp_death_age_stratified_counts_adj.csv")[,-1]

#Protection before simulation
old_severe_waning_data <- read.csv("results/waning-predictions/main/severe_waning_predictions_monthly.csv")[,-1]
old_nonsevere_waning_data <- read.csv("results/waning-predictions/main/nonsevere_waning_predictions_monthly.csv")[,-1]

#Worsened protection, including worse vaccine-induced immunity
new_severe_waning_data <- read.csv("results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")[,-1] 
new_nonsevere_waning_data <- read.csv("results/waning-predictions/variant-waning/immune-escape-ver1/nonsevere_waning_predictions_monthly.csv")[,-1]

#Worsened protection, including improved vaccine-induced immunity
new_vax_severe_waning_data <- read.csv("results/waning-predictions/variant-waning/S4-immune-escape-ver1/severe_waning_predictions_monthly.csv")[,-1]
new_vax_nonsevere_waning_data <- read.csv("results/waning-predictions/variant-waning/S4-immune-escape-ver1/nonsevere_waning_predictions_monthly.csv")[,-1]

#MAKE SURE YOU ARE SETTING THE CORRECT WANING CURVE FOR CALIBRATION (mean, lower, upper)
old_severe_waning <- old_severe_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(old_severe_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, study, ve_pred))
new_severe_waning <- new_severe_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(new_severe_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, study, ve_pred))
new_vax_severe_waning <- new_vax_severe_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(new_vax_severe_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, study, ve_pred))
old_nonsevere_waning <- old_nonsevere_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(old_nonsevere_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, ve_pred))
new_nonsevere_waning <- new_nonsevere_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(new_nonsevere_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, ve_pred))
new_vax_nonsevere_waning <- new_vax_nonsevere_waning_data %>% filter(estimate == waning) %>%
  rowwise() %>% mutate(new_vax_nonsevere_ve_pred = max(ve_pred, 0))  %>% dplyr::select(-c(estimate, month_input, ve_pred))

waning_data_clean <- merge(merge(merge(merge(merge(new_severe_waning, old_severe_waning, by = c("age_group", "prior_inf", "months"), all.x = TRUE),
                           old_nonsevere_waning, by = c("age_group", "prior_inf", "months"), all.x = TRUE),
                           new_nonsevere_waning, by = c("age_group", "prior_inf", "months"), all.x = TRUE),
                           new_vax_severe_waning,  by = c("age_group", "prior_inf", "months"), all.x = TRUE),
                           new_vax_nonsevere_waning,  by = c("age_group", "prior_inf", "months"), all.x = TRUE)


#MAKE SURE YOU ARE READING IN THE CORRECT CALIBRATION FILE
age_18_49 <- read.csv(paste0("results/calibration/main/immunocompetent/waning-", waning, "/sero-", sero, "/1mil-18-49 years-monthly.csv"))[,-1] 
age_50_64 <-  read.csv(paste0("results/calibration/main/immunocompetent/waning-", waning, "/sero-", sero, "/1mil-50-64 years-monthly.csv"))[,-1]
age_65_74 <-  read.csv(paste0("results/calibration/main/immunocompetent/waning-", waning, "/sero-", sero, "/1mil-65-74 years-monthly.csv"))[,-1]
age_75_plus <-  read.csv(paste0("results/calibration/main/immunocompetent/waning-", waning, "/sero-", sero, "/1mil-75+ years-monthly.csv"))[,-1] 
############################################################################################
#clean age matrices
clean_age_matrix <- function(df){
  df %>% mutate(months_since_last_dose = as.numeric(as.character(as.factor(interval(time_since_last_dose,as.Date('2022-09-01')) %/% months(1))))) %>% 
    dplyr::select(c("individual", "age_group", "prior_inf", "months_since_last_dose_inf", "months_since_last_dose", "num_doses","lambda"))
}

clean_df <- list(age_18_49, age_50_64, age_65_74, age_75_plus) %>%
  lapply(clean_age_matrix) 

############################################################################################
#calculate multiplier adjustments

multiplier_adj <- function(df){
  age_info <- df$age_group[1]
  with_protection <- merge(df, waning_data_clean, by.x = c("age_group", "prior_inf", "months_since_last_dose_inf"), 
                           by.y = c("age_group", "prior_inf", "months"), all.x = TRUE)
  
  perfect_immunity_index <- which(with_protection$months_since_last_dose_inf < with_protection$months_since_last_dose & with_protection$prior_inf == 1 & with_protection$months_since_last_dose_inf %in% c(1:3))
  with_protection[perfect_immunity_index, c("old_severe_ve_pred", "old_nonsevere_ve_pred")] <- 1
  
  multiplier_adjustment <- (1 - mean(with_protection$old_nonsevere_ve_pred))/(1 - mean(with_protection$old_severe_ve_pred))
  return(multiplier_adjustment)
}

mult_adj <- clean_df %>%
  lapply(multiplier_adj)

############################################################################################

if(case == "mean") {
  case_multipliers <- c(200, 79.6, 22.6, 9.6)
} else if (case == "lower") {
  case_multipliers <- c(150, 59.7, 16.95, 7.2)
} else{
  case_multipliers <-c(250, 99.5, 28.25, 12)
}

print(case_multipliers)

nonsevere_infection_multipliers <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                              multiplier = case_multipliers,
                                              multiplier_adjustment = c(mult_adj[[1]], mult_adj[[2]], mult_adj[[3]], mult_adj[[4]])) 





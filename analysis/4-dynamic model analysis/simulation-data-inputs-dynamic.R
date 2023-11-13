###################################################################################################
#Title: Simulation (data inputs)
#Author: Hailey Park
#Date: September 25, 2023
###################################################################################################

hosp_death_age_stratified <- data.table(read.csv("data/clean-data/hosp_death_age_stratified_counts_adj.csv")[,-1]) %>%
  add_row(age_group = "0-17 years", num_hosp = 0, num_death = 0, perc_death = 0.01666344)

avg_incidence_data <- read.csv("data/clean-data/monthly-incidence-estimates.csv")[,-1]

#Adjust the monthly severe incidence estimates to weekly non-severe incidence estimates
average_nonsevere_incidence <- avg_incidence_data %>%
  #adding the 0-17 year age group, assuming that average severe incidence is 5x less than 18-49 year incidence
  add_row(age_group = "0-17 years", avg_inc = 0.00008/5) %>% 
  arrange(age_group) %>%
  #convert monthly incidence to weekly incidence (dividing by 4.345)
  #apply age-specific nonsevere infection multipliers (this is the c(1000,200,79.6,22.6,9.6))
  #apply additional nonsevere case multplier for better calibration (this is the 2.5x multiplier)
  mutate(avg_inc = (avg_inc/4.345) * c(1000,200, 79.6, 22.6, 9.6) * 2.5,
         beta = avg_inc/min(avg_inc))

average_severe_incidence <- avg_incidence_data %>%
  #adding the 0-17 year age group, assuming that average severe incidence is 5x less than 18-49 year incidence
  add_row(age_group = "0-17 years", avg_inc = 0.00008/5) %>% 
  arrange(age_group) %>%
  #convert monthly incidence to weekly incidence (dividing by 4.345)
  mutate(avg_inc = avg_inc/4.345,
         beta = avg_inc/min(avg_inc))

severe_waning_data <- read.csv("results/waning-predictions/dynamic/combined_severe_waning_predictions_weekly.csv")[,-1]
nonsevere_waning_data <- read.csv("results/waning-predictions/dynamic/combined_nonsevere_waning_predictions_weekly.csv")[,-1] ##CHANGE HERE 


#MAKE SURE YOU ARE SETTING THE CORRECT WANING CURVE FOR CALIBRATION 
severe_waning <- severe_waning_data %>% filter(estimate == waning) %>% dplyr::select(-c(study, estimate, month_input, months)) %>%
  rowwise() %>% mutate(severe_ve_pred = max(ve_pred, 0))
nonsevere_waning <- nonsevere_waning_data %>% filter(estimate == waning) %>% dplyr::select(-c(estimate, month_input, months)) %>%
  rowwise() %>% mutate(nonsevere_ve_pred = max(ve_pred, 0))

waning_data_clean <- setDT(merge(severe_waning, nonsevere_waning, by = c("age_group", "prior_inf", "immunocompromised", "weeks")) %>%
                             select(-c("ve_pred.x", "ve_pred.y")))


#MAKE SURE YOU ARE READING IN THE CORRECT CALIBRATION FILE
entire_pop <-  read.csv(paste0("calibration/dynamic/entire_population_calibration_nonsevere2.5x_", waning, ".csv"))[,-1] 

total_infections <- ceiling(sum((merge(entire_pop %>% group_by(age_group) %>% summarise(total = n()),
                                       average_nonsevere_incidence, by = "age_group", all.x = TRUE) %>% 
                                   mutate(total_inf = total * avg_inc))$total_inf)) + ceiling(sum((merge(entire_pop %>% group_by(age_group, immunocompromised) %>% summarise(total = n()),
                                                                                                         average_severe_incidence, by = "age_group", all.x = TRUE) %>% 
                                                                                                     mutate(avg_inc = if_else(immunocompromised %in% c(1, 2), avg_inc * 2.8, avg_inc),
                                                                                                       total_inf = total * avg_inc))$total_inf))
############################################################################################
#clean age matrices
clean_age_matrix <- function(df){
  df %>% dplyr::select(c("individual", "age_group", "prior_inf", "immunocompromised","weeks_since_last_dose_inf", "time_since_last_dose", "num_doses", "lambda")) %>%
    mutate(weeks_since_last_dose = as.numeric(as.character(as.factor(interval(time_since_last_dose, as.Date('2022-09-01')) %/% weeks(1))))) %>%
    dplyr::select(-c("time_since_last_dose"))
}

clean_df <- list(entire_pop) %>%
  lapply(clean_age_matrix) 

############################################################################################
#calculate multiplier adjustments

multiplier_adj <- function(df){
  with_protection <- merge(df, waning_data_clean, by.x = c("age_group", "prior_inf", "immunocompromised","weeks_since_last_dose_inf"), 
                           by.y = c("age_group", "prior_inf","immunocompromised", "weeks"), all.x = TRUE)
  perfect_immunity_index <- which(with_protection$weeks_since_last_dose_inf < with_protection$weeks_since_last_dose & with_protection$prior_inf == 1 & with_protection$weeks_since_last_dose_inf %in% c(1:3))
  with_protection[perfect_immunity_index, c("severe_ve_pred", "nonsevere_ve_pred")] <- 1
  
  group_multiplier_adj <- with_protection %>% group_by(age_group, immunocompromised) %>% summarise(mean_severe_ve = mean(severe_ve_pred),
                                                                                                   mean_nonsevere_ve = mean(nonsevere_ve_pred)) %>%
    mutate(multiplier_adj = (1-mean_severe_ve)/(1-mean_nonsevere_ve))
  
  return(group_multiplier_adj %>% select(age_group, immunocompromised, multiplier_adj))
}

mult_adj <- multiplier_adj(clean_df[[1]])


severe_infection_multipliers <- setDT(merge(merge(data.frame(age_group = rep(c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"), 3),
                                                             immunocompromised =rep(c(0,1,2), each = 5), 
                                                             multiplier = c(1/c(1000, 200, 79.6, 22.6, 9.6), rep(1/(c(1000, 200, 79.6, 22.6, 9.6)/2.8), 2)) * (1/2.5)),
                                                  mult_adj, by = c("age_group", "immunocompromised"), all.x = TRUE),
                                            average_nonsevere_incidence %>% select(age_group, beta),
                                            by = "age_group", all.X = TRUE) )

############################################################################################


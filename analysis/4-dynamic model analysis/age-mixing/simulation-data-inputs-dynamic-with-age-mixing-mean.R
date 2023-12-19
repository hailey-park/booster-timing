###################################################################################################
#Title: Simulation (data inputs)
#Author: Hailey Park
#Date: September 25, 2023
###################################################################################################

hosp_death_age_stratified <- data.table(read.csv("data/hosp_death_age_stratified_counts_adj.csv")[,-1]) %>%
  add_row(age_group = "0-17 years", num_hosp = 0, num_death = 0, perc_death = 0.01666344)

average_nonsevere_incidence <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                          avg_inc = c(.00008/5,.00008, .00016, .00041, .00113)/4.345 * c(1000,200, 79.6, 22.6, 9.6) * 1.5,
                                          pe_avg = c(0.3712155, 0.4280549, 0.4233205, 0.4451702 , 0.4450632)) %>%
  mutate(pe_minus_1 = 1 - pe_avg,
         pe_adj = 0.5548298/pe_minus_1,
         inc_adj = avg_inc/min(avg_inc),
         beta = pe_adj * inc_adj)

average_severe_incidence <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                       avg_severe_inc = c(.00008/5,.00008, .00016, .00041, .00113)/4.345)
severe_waning_data <- read.csv("data/combined_severe_waning_predictions_weekly.csv")[,-1]
nonsevere_waning_data <- read.csv("data/combined_nonsevere_waning_predictions_weekly.csv")[,-1] ##CHANGE HERE 

contact_matrix <- read.csv("data/contact matrix.csv") %>% mutate(across(X0.17.years:X75..years) * 7)

#MAKE SURE YOU ARE SETTING THE CORRECT WANING CURVE FOR CALIBRATION 
severe_waning <- severe_waning_data %>% filter(estimate == 'mean') %>% dplyr::select(-c(study, estimate, month_input, months)) %>%
  rowwise() %>% mutate(severe_ve_pred = max(ve_pred, 0))
nonsevere_waning <- nonsevere_waning_data %>% filter(estimate == 'mean') %>% dplyr::select(-c(estimate, month_input, months)) %>%
  rowwise() %>% mutate(nonsevere_ve_pred = max(ve_pred, 0))

nonsevere_waning_prior_inf_only <- read.csv("data/combined_nonsevere_waning_predictions_weekly_prior_inf_only.csv")[,-1] %>%
  filter(estimate == "mean") %>%
  rename(nonsevere_prior_inf_only_ve_pred = ve_pred) %>%
  dplyr::select(age_group, weeks, immunocompromised, nonsevere_prior_inf_only_ve_pred)

severe_waning_prior_inf_only <- read.csv("data/combined_severe_waning_predictions_weekly_prior_inf_only.csv")[,-1] %>%
  filter(estimate == "mean") %>%
  rename(severe_prior_inf_only_ve_pred = ve_pred) %>%
  dplyr::select(age_group, weeks, immunocompromised, severe_prior_inf_only_ve_pred)

waning_data_clean <- setDT(merge(merge(merge(severe_waning, nonsevere_waning, by = c("age_group", "prior_inf", "immunocompromised", "weeks")) %>%
                             select(-c("ve_pred.x", "ve_pred.y")),
                             nonsevere_waning_prior_inf_only, by = c("weeks", "age_group", "immunocompromised"), all.x = TRUE),
                             severe_waning_prior_inf_only, by = c("weeks", "age_group", "immunocompromised"), all.x = TRUE))


#MAKE SURE YOU ARE READING IN THE CORRECT CALIBRATION FILE
entire_pop <-  read.csv("calibration-results/entire_population_calibration_nonsevere1.5x_mean.csv")[,-1] 

inf_by_age <- merge(merge(entire_pop %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n()),
                          average_severe_incidence, by = "age_group", all.x = TRUE),
                    average_nonsevere_incidence, by = "age_group", all.x = TRUE) %>% 
  mutate(avg_severe_inc = if_else(immunocompromised %in% c(1, 2), avg_severe_inc * 2.8, avg_severe_inc),
         nonsevere_inf = total_pop * avg_inc,
         severe_inf = total_pop * avg_severe_inc,
         total_inf = nonsevere_inf + severe_inf) %>%
  group_by(age_group) %>% summarise(total_inf = ceiling(sum(total_inf)),
                                    total_pop = sum(total_pop))

contact_matrix_adj <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                 contact_matrix_adj = (sum(inf_by_age$total_inf)/10000000)/c(sum((inf_by_age$total_inf/inf_by_age$total_pop) * contact_matrix$X0.17.years),
                                                        sum((inf_by_age$total_inf/inf_by_age$total_pop) * contact_matrix$X18.49.years),
                                                        sum((inf_by_age$total_inf/inf_by_age$total_pop) * contact_matrix$X50.64.years),
                                                        sum((inf_by_age$total_inf/inf_by_age$total_pop) * contact_matrix$X65.74.years),
                                                        sum((inf_by_age$total_inf/inf_by_age$total_pop) * contact_matrix$X75..years)))

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
  
  #Individuals who are unvaccinated and no prior infection history has no protection
  immune_naive_index <- which(with_protection$num_doses=="unvax" & with_protection$prior_inf == 0)
  with_protection[immune_naive_index, c("severe_ve_pred", "nonsevere_ve_pred")] <- 0
  
  #Individuals who are unvaccinated and have prior infection history have prior infection only waning immunity
  prior_inf_only_index <- which(with_protection$num_doses=="unvax" & with_protection$prior_inf == 1)
  with_protection[prior_inf_only_index, c("severe_ve_pred", "nonsevere_ve_pred")] <- with_protection[prior_inf_only_index, c("severe_prior_inf_only_ve_pred", "nonsevere_prior_inf_only_ve_pred")]

  #Individuals with infection <3 months from simulation start has perfect immunity
  perfect_immunity_index <- which(with_protection$weeks_since_last_dose_inf < with_protection$weeks_since_last_dose & with_protection$prior_inf == 1 & with_protection$weeks_since_last_dose_inf %in% c(1:13))
  with_protection[perfect_immunity_index, c("severe_ve_pred", "nonsevere_ve_pred")] <- 1
  
  group_multiplier_adj <- with_protection %>% group_by(age_group, immunocompromised) %>% summarise(mean_severe_ve = mean(severe_ve_pred),
                                                                                                   mean_nonsevere_ve = mean(nonsevere_ve_pred)) %>%
    mutate(multiplier_adj = (1-mean_severe_ve)/(1-mean_nonsevere_ve))
  
  return(group_multiplier_adj %>% select(age_group, immunocompromised, multiplier_adj))
}

mult_adj <- multiplier_adj(clean_df[[1]])


severe_infection_multipliers <- setDT(merge(merge(data.frame(age_group = rep(c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"), 3),
                                                             immunocompromised =rep(c(0,1,2), each = 5), 
                                                             multiplier = c(1/c(1000, 200, 79.6, 22.6, 9.6), rep(1/(c(1000, 200, 79.6, 22.6, 9.6)/2.8), 2)) * (1/1.5)),
                                                  mult_adj, by = c("age_group", "immunocompromised"), all.x = TRUE),
                                            average_nonsevere_incidence %>% select(age_group, beta),
                                            by = "age_group", all.X = TRUE) )

############################################################################################


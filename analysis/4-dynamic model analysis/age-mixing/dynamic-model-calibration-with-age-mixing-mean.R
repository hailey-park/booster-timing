###################################################################################################
#Title: Dynamic Model Calibration (with Age Mixing)
#Author: Hailey Park
#Date: October 3, 2023
###################################################################################################

# rm(list=ls())
# 
# setwd("~/Stanford Research/booster-timing-old")
# 
# #Load libraries
# library(readr)
# library(dplyr)
# library(tidyverse)
# library(ggplot2)
# library(tidyr)
# library(tibble)
# library(reshape2)
# library(lubridate)
# library(scales)
# library(data.table)

#Load data
avg_nonsevere_inc_adj <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                    avg_inc = (c(.00008/5,.00008, .00016, .00041, .00113)/4.345) * c(1000,200, 79.6, 22.6, 9.6) * 1.5) 
avg_severe_inc_adj <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                 avg_severe_inc = (c(.00008/5,.00008, .00016, .00041, .00113)/4.345)) 

pop_by_age <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                         total_pop = (c(2206000, 6410000-2206000, 8317000-6410000, 9063000-8317000, 10000000-9063000))) 

cases_by_week <- read.csv("data/cases_by_week.csv")[,-1]
four_doses_by_week <- read.csv("data/four_doses_by_week.csv")[,-1]
three_doses_by_week <- read.csv("data/three_doses_by_week.csv")[,-1]

contact_matrix <- read.csv("data/contact matrix.csv")

nonsevere_waning <- read.csv("data/combined_nonsevere_waning_predictions_weekly.csv")[,-1] %>%
  filter(estimate == "mean") %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred, 0))

nonsevere_waning_prior_inf_only <- read.csv("data/combined_nonsevere_waning_predictions_weekly_prior_inf_only.csv")[,-1] %>%
  filter(estimate == "mean") %>%
  rename(prior_inf_only_ve_pred = ve_pred) %>%
  dplyr::select(age_group, weeks, immunocompromised, prior_inf_only_ve_pred)

#Clean data
four_doses_by_week$week <- as.character(four_doses_by_week$week)
three_doses_by_week$week <- as.character(three_doses_by_week$week)

contact_matrix <- contact_matrix %>% mutate(across(X0.17.years:X75..years) * 7)

############################################################################
#Create matrix for entire population
#Age-Counts from 2020 Census (https://www.census.gov/library/visualizations/interactive/exploring-age-groups-in-the-2020-census.html)
#Age-specific immunocompromised prevalence from MarketScan 2017 (https://wwwnc.cdc.gov/eid/article/26/8/19-1493-f2)
#Unvaccinated prevalence is from CDPH vaccination board (https://covid19.ca.gov/vaccination-progress-data/#overview)
#For vaccinated prevalence, keeping same proportions of 3-dose vs. 4-dose
#Assuming that severe immunocompromised cases are 5% of total immunocompromised groups

set.seed(488)
age_0_17_cal <- data.frame(individual = c(1:(2206000)),
                           age_group = '0-17 years',
                           num_doses = sample(c('unvax', '3-dose'), 2206000, prob = c(0.59, 0.41), replace = TRUE),
                           prior_inf = rbinom(2206000, 1, 0.8238),
                           immunocompromised = sample(c(0,1,2), 2206000, prob = c(0.9715, 0.0285*0.85, 0.0285*0.15), replace = TRUE))
set.seed(488)
age_18_49_cal <- data.frame(individual = c((2206000 + 1):(6410000)),
                            age_group = '18-49 years',
                            num_doses = sample(c('unvax', '3-dose'), 4204000, prob = c(0.213, 0.787), replace = TRUE),
                            prior_inf = rbinom(4204000, 1, 0.8238),
                            immunocompromised = sample(c(0,1,2), 4204000, prob = c(0.942, 0.058*0.85, 0.058*0.15), replace = TRUE))

set.seed(488)
age_50_64_cal <- data.frame(individual = c((6410000 + 1):(8317000)),
                            age_group = '50-64 years',
                            num_doses = sample(c('unvax','3-dose', '4-dose'), 1907000, prob = c(0.157, 0.6*0.843, 0.4*0.843), replace = TRUE),
                            prior_inf = rbinom(1907000, 1, 0.6579),
                            immunocompromised = sample(c(0,1,2), 1907000, prob = c(0.901, 0.099*0.85, 0.099*0.15), replace = TRUE))

set.seed(488)
age_65_74_cal <- data.frame(individual = c((8317000 + 1):(9063000)),
                            age_group = '65-74 years',
                            num_doses = sample(c('unvax','3-dose', '4-dose'), 746000, prob = c(0.115, 0.6*0.885, 0.4*0.885), replace = TRUE),
                            prior_inf = rbinom(746000, 1, 0.4681),
                            immunocompromised = sample(c(0,1,2), 746000, prob = c(0.861, 0.139*0.85, 0.139*0.15), replace = TRUE))

set.seed(488)
age_75_plus_cal <- data.frame(individual = c((9063000  + 1):(10000000)),
                              age_group = '75+ years',
                              num_doses = sample(c('unvax','3-dose', '4-dose'), 937000, prob = c(0.115, 0.6*0.885, 0.4*0.885), replace = TRUE),
                              prior_inf = rbinom(937000, 1, 0.4681),
                              immunocompromised = sample(c(0,1,2), 937000, prob = c(0.861, 0.139*0.85, 0.139*0.15), replace = TRUE))

entire_population <- rbind(age_0_17_cal, age_18_49_cal, age_50_64_cal, age_65_74_cal, age_75_plus_cal)

############################################################################
#Determining contact matrix sums by age group

inf_by_age <- merge(merge(entire_population %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n()),
                          avg_severe_inc_adj, by = "age_group", all.x = TRUE),
                          avg_nonsevere_inc_adj, by = "age_group", all.x = TRUE) %>% 
  mutate(avg_severe_inc = if_else(immunocompromised %in% c(1, 2), avg_severe_inc * 2.8, avg_severe_inc),
         nonsevere_inf = total_pop * avg_inc,
         severe_inf = total_pop * avg_severe_inc,
         total_inf = nonsevere_inf + severe_inf) %>%
  group_by(age_group) %>% summarise(total_inf = ceiling(sum(total_inf)),
                                    total_pop = sum(total_pop))


contact_mat_0_17 <- sum(inf_by_age$total_inf/inf_by_age$total_pop * contact_matrix$X0.17.years)
contact_mat_18_49 <- sum(inf_by_age$total_inf/inf_by_age$total_pop * contact_matrix$X18.49.years)
contact_mat_50_64 <- sum(inf_by_age$total_inf/inf_by_age$total_pop * contact_matrix$X50.64.years)
contact_mat_65_74 <- sum(inf_by_age$total_inf/inf_by_age$total_pop * contact_matrix$X65.74.years)
contact_mat_75plus <- sum(inf_by_age$total_inf/inf_by_age$total_pop * contact_matrix$X75..years)

############################################################################
#Assigning a 'time-since-last' to each individual and calibrate a lambda

#Calculate time since last vaccine dose or infection
add.weeks= function(date,n) {seq(date, by = paste (n, "weeks"), length = 2)[2]}

time_since_last <- function(df) {
  #Calculate time since last dose and time since last infection
  set.seed(488)
  last_dose_and_inf <- df %>% mutate(time_since_last_dose = ifelse(num_doses == "3-dose", 
                                                                   sample(three_doses_by_week$week,
                                                                          size = sum(num_doses == '3-dose'),
                                                                          prob = three_doses_by_week$perc_doses, 
                                                                          replace = TRUE),
                                                                   ifelse(num_doses == '4-dose',
                                                                          sample(four_doses_by_week$week, 
                                                                                 size = sum(num_doses == '4-dose'),
                                                                                 prob = four_doses_by_week$perc_doses, 
                                                                                 replace = TRUE),
                                                                          '2020-09-01')),
                                     time_since_last_inf = ifelse(prior_inf == 1,
                                                                  sample(as.character(cases_by_week$week), 
                                                                         size = sum(prior_inf == 1),
                                                                         prob = cases_by_week$perc_cases,
                                                                         replace = TRUE),
                                                                  NA))
  #Reinfection for 10% of infected individuals
  set.seed(488)
  reinfection <- last_dose_and_inf %>% filter(prior_inf == 1) %>%  
    sample_frac(.1) %>% 
    mutate(reinf_period = interval(as.Date(time_since_last_inf), as.Date("2022-06-01"),) %/% weeks(1)) %>%
    rowwise() %>% mutate(time_since_last_reinf = ifelse(reinf_period > 0,
                                                        sample(as.character(cases_by_week$week[as.Date(cases_by_week$week) >= add.weeks(as.Date(time_since_last_inf), 3)]),
                                                               size = 1,
                                                               prob = cases_by_week$perc_cases[as.Date(cases_by_week$week) >= add.weeks(as.Date(time_since_last_inf), 3)],
                                                               replace = TRUE),
                                                        NA)) %>% dplyr::select(individual,time_since_last_reinf)                                                          
  
  
  #Merge reinfection data to main df
  merged <- merge(last_dose_and_inf, reinfection, by = c("individual"), all.x = TRUE) %>% 
    mutate(time_since_last_dose_inf = pmax(as.Date(time_since_last_dose), as.Date(time_since_last_inf), as.Date(time_since_last_reinf), na.rm =  TRUE))
  
  return(merged)
}

calibration <- function(df) {
  
  #convert time since last dose/inf into weeks before 9/1/22
  weeks_since <- df %>% mutate(weeks_since_last_dose_inf = as.factor(interval(time_since_last_dose_inf,as.Date('2022-09-01')) %/% weeks(1)),
                               weeks_since_last_inf = as.factor(interval(time_since_last_inf,as.Date('2022-09-01')) %/% weeks(1)),
                               weeks_since_last_reinf = as.factor(interval(time_since_last_reinf,as.Date('2022-09-01')) %/% weeks(1))) 
  
  
  #merge with protective effectiveness
  combined <- merge(merge(merge(weeks_since, nonsevere_waning %>%
                      dplyr::select(-c("months", "month_input")), by.x = c("weeks_since_last_dose_inf", "age_group", "prior_inf", "immunocompromised"), 
                    by.y = c("weeks", "age_group", "prior_inf", "immunocompromised"), all.x = TRUE),
                    avg_nonsevere_inc_adj, by = "age_group", all.x = TRUE),
                    nonsevere_waning_prior_inf_only, by.x = c("weeks_since_last_dose_inf", "age_group", "immunocompromised"),
                    by.y = c("weeks", "age_group", "immunocompromised"), all.x = TRUE)
  
  #Individuals who are unvaccinated and no prior infection history has no protection
  combined$ve_pred[combined$num_doses=="unvax" & combined$prior_inf == 0] <- 0
  
  #Individuals who are unvaccinated and have prior infection history have prior infection only waning immuntiy
  combined$ve_pred[combined$num_doses=="unvax" & combined$prior_inf == 1] <- combined$prior_inf_only_ve_pred[combined$num_doses=="unvax" & combined$prior_inf == 1]
  
  #Individuals with infection <3 months from simulation start has perfect immunity
  combined$ve_pred[combined$time_since_last_inf>="2022-06-01"] <- 1
  combined$ve_pred[combined$time_since_last_reinf>="2022-06-01"] <- 1
  
  #Calibrate risk factor (Lambda)
  pe_list <- combined$ve_pred
  index_0_17 <- which(combined$age_group == "0-17 years")
  index_18_49 <- which(combined$age_group == "18-49 years")
  index_50_64 <- which(combined$age_group == "50-64 years")
  index_65_74 <- which(combined$age_group == "65-74 years")
  index_75plus <- which(combined$age_group == "75+ years")
  
  beta_df <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                        avg_inc = (c(.00008/5,.00008, .00016, .00041, .00113)/4.345) * c(1000,200, 79.6, 22.6, 9.6) * 1.5,
                        avg_pe = c(mean(pe_list[index_0_17]), mean(pe_list[index_18_49]), mean(pe_list[index_50_64]), mean(pe_list[index_65_74]), mean(pe_list[index_75plus]))) %>%
    mutate(pe_minus_1 = 1 - avg_pe,
           inc_adj = avg_inc/min(avg_inc),
           pe_adj = (1 - mean(pe_list[index_65_74]))/pe_minus_1,
           beta = inc_adj * pe_adj)
  
  
  total_inf <- sum(inf_by_age$total_inf)

  
  lambda_calibration <- function(lambda) {
    risk_cal_0_17 <- lambda * beta_df$beta[1] * ((total_inf/10000000)/contact_mat_0_17) * (1-beta_df$avg_pe[1]) * contact_mat_0_17
    risk_cal_18_49 <- lambda * beta_df$beta[2] * ((total_inf/10000000)/contact_mat_18_49)* (1-beta_df$avg_pe[2]) * contact_mat_18_49
    risk_cal_50_64 <- lambda * beta_df$beta[3] * ((total_inf/10000000)/contact_mat_50_64)* (1-beta_df$avg_pe[3]) * contact_mat_50_64
    risk_cal_65_74 <- lambda * beta_df$beta[4] * ((total_inf/10000000)/contact_mat_65_74)* (1-beta_df$avg_pe[4]) * contact_mat_65_74
    risk_cal_75plus <- lambda * beta_df$beta[5] * ((total_inf/10000000)/contact_mat_75plus)* (1-beta_df$avg_pe[5]) * contact_mat_75plus
    
    risk_cal <- c(risk_cal_0_17, risk_cal_18_49, risk_cal_50_64, risk_cal_65_74, risk_cal_75plus)
    return((sum(risk_cal - avg_nonsevere_inc_adj$avg_inc)^2))
  }
  
  lambda <- optim(p = c(0.1), lambda_calibration)
  print(lambda$par)

  return(combined %>% mutate(lambda = lambda$par))
}

#Run calibration
set.seed(488)
calibration_results <- list(entire_population) %>%
  lapply(time_since_last) %>%
  lapply(calibration)

#Set up folder structure to save calibrated population
dir.create("calibration-results")
write.csv(calibration_results[[1]], "calibration-results/entire_population_calibration_nonsevere1.5x_mean.csv")



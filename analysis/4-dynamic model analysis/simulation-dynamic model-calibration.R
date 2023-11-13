###################################################################################################
#Title: Dynamic Model Calibration
#Author: Hailey Park
#Date: October 3, 2023
###################################################################################################

rm(list=ls())

setwd("~/Stanford Research/booster-timing")

#Load libraries
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(tibble)
library(reshape2)
library(lubridate)
library(scales)
library(data.table)

#Load data
avg_incidence_adj <- data.frame(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                avg_inc = (c(.00008/5,.00008, .00016, .00041, .00113)/4.345) * c(1000,200, 79.6, 22.6, 9.6) * 2.5) 
cases_by_week <- read.csv("data/clean-data/cases_by_week.csv")[,-1]
four_doses_by_week <- read.csv("data/clean-data/four_doses_by_week.csv")[,-1]
three_doses_by_week <- read.csv("data/clean-data/three_doses_by_week.csv")[,-1]

nonsevere_waning <- read.csv("results/waning-predictions/dynamic/combined_nonsevere_waning_predictions_weekly.csv")[,-1] %>%
  filter(estimate == "mean")

#Clean data
four_doses_by_week$week <- as.character(four_doses_by_week$week)
three_doses_by_week$week <- as.character(three_doses_by_week$week)

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
  weeks_since <- df %>% mutate(weeks_since_last_dose_inf = as.factor(interval(time_since_last_dose_inf,as.Date('2022-09-01')) %/% weeks(1))) 
  
  
  #merge with protective effectiveness
  combined <- merge(merge(weeks_since, nonsevere_waning %>%
                      dplyr::select(-c("months", "month_input")), by.x = c("weeks_since_last_dose_inf", "age_group", "prior_inf", "immunocompromised"), 
                    by.y = c("weeks", "age_group", "prior_inf", "immunocompromised"), all.x = TRUE),
                    avg_incidence_adj, by = "age_group", all.x = TRUE)
  
  #Individuals with infection <3 months from simulation start has perfect immunity
  combined$ve_pred[combined$time_since_last_inf>="2022-06-01"] <- 1
  combined$ve_pred[combined$time_since_last_reinf>="2022-06-01"] <- 1
  
  #Individuals who are unvaccinated and no prior infection history has no protection
  combined$ve_pred[combined$num_doses=="unvax" & combined$prior_inf == 0] <- 0
  
  
  #Calibrate risk factor (Lambda)
  pe_list <- combined$ve_pred
  avg_inc <- (combined$avg_inc)
  beta <- (combined$avg_inc)/(min(combined$avg_inc))
  index_0_17 <- which(combined$age_group == "0-17 years")
  index_18_49 <- which(combined$age_group == "18-49 years")
  index_50_64 <- which(combined$age_group == "50-64 years")
  index_65_74 <- which(combined$age_group == "65-74 years")
  index_75plus <- which(combined$age_group == "75+ years")
  

  lambda_calibration <- function(lambda) {
    risk_cal_0_17 <- mean(lambda * beta[index_0_17] * (1-pe_list[index_0_17]) * 83381/10000000)
    risk_cal_18_49 <- mean(lambda * beta[index_18_49] * (1-pe_list[index_18_49]) * 83381/10000000) 
    risk_cal_50_64 <- mean(lambda * beta[index_50_64] * (1-pe_list[index_50_64]) * 83381/10000000)
    risk_cal_65_74 <- mean(lambda * beta[index_65_74] * (1-pe_list[index_65_74]) * 83381/10000000)
    risk_cal_75plus <- mean(lambda * beta[index_75plus] * (1-pe_list[index_75plus]) * 83381/10000000) 

    
    risk_cal <- c(risk_cal_0_17, risk_cal_18_49, risk_cal_50_64, risk_cal_65_74, risk_cal_75plus)
    return((sum(risk_cal - avg_incidence_adj$avg_inc)^2))
  }
  
  lambda <- optim(p = c(0.1), lambda_calibration)
  print(lambda$par)

  return(combined %>% mutate(lambda = lambda$par))
}

set.seed(488)
calibration_results <- list(entire_population) %>%
  lapply(time_since_last) %>%
  lapply(calibration)

inspection <- calibration_results[[1]] 


hello <- head(inspection, 100)
write.csv(inspection, "calibration/dynamic/entire_population_calibration_nonsevere2.5x_lower.csv")


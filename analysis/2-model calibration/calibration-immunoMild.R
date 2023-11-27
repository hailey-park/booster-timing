###################################################################################################
#Title: Calibration (Immunocompromised (Mild))
#Author: Hailey Park
#Date: April 18, 2023
###################################################################################################

rm(list=ls())

setwd(here::here())

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
cases_by_month <- read.csv("data/clean-data/cases_by_month.csv")[,-1]
four_doses_by_month <- read.csv("data/clean-data/four_doses_by_month.csv")[,-1]
three_doses_by_month <- read.csv("data/clean-data/three_doses_by_month.csv")[,-1]
avg_incidence_adj <- read.csv("data/clean-data/monthly-incidence-estimates.csv")[,-1] %>%
  mutate(avg_inc = avg_inc * 2.8)

############################################################################
#MAKE SURE LOADING IN CORRECT WANING DATA FROM CORRECT FOLDER
#Choose from "main", "optimistic-ve", "optimistic-waning", "pessimistic-ve", "pessimistic-waning"

waning_data <- read.csv("results/waning-predictions/main/severe_waning_predictions_monthly_immunoMild.csv")[,-1] 

############################################################################

#Clean data
four_doses_by_month$month <- as.character(four_doses_by_month$month)
three_doses_by_month$month <- as.character(three_doses_by_month$month)

#Calculate time since last vaccine dose or infection
add.months= function(date,n) {seq(date, by = paste (n, "months"), length = 2)[2]}

time_since_last <- function(df) {
  #Calculate time since last dose and time since last infection
  set.seed(88)
  last_dose_and_inf <- df %>% mutate(time_since_last_dose = ifelse(num_doses == "3-dose", 
                                                                   sample(three_doses_by_month$month,
                                                                          size = sum(num_doses == '3-dose'),
                                                                          prob = three_doses_by_month$perc_doses, 
                                                                          replace = TRUE),
                                                                   sample(four_doses_by_month$month, 
                                                                          size = sum(num_doses == '4-dose'),
                                                                          prob = four_doses_by_month$perc_doses, 
                                                                          replace = TRUE)),
                                     time_since_last_inf = ifelse(prior_inf == 1,
                                                                  sample(as.character(cases_by_month$month), 
                                                                         size = sum(prior_inf == 1),
                                                                         prob = cases_by_month$perc_cases,
                                                                         replace = TRUE),
                                                                  NA))
  #Reinfection for 10% of infected individuals
  set.seed(88)
  reinfection <- last_dose_and_inf %>% filter(prior_inf == 1) %>%  
    sample_frac(.1) %>% 
    mutate(reinf_period = interval(as.Date(time_since_last_inf), as.Date("2022-06-01"),) %/% months(1)) %>%
    rowwise() %>% mutate(time_since_last_reinf = ifelse(reinf_period > 0,
                                                        sample(as.character(cases_by_month$month[as.Date(cases_by_month$month) >= add.months(as.Date(time_since_last_inf), 3)]),
                                                               size = 1,
                                                               prob = cases_by_month$perc_cases[as.Date(cases_by_month$month) >= add.months(as.Date(time_since_last_inf), 3)],
                                                               replace = TRUE),
                                                        NA)) %>% dplyr::select(individual,time_since_last_reinf)                                                          
  
  
  #Merge reinfection data to main df
  merged <- merge(last_dose_and_inf, reinfection, by = c("individual"), all.x = TRUE) %>% 
    mutate(time_since_last_dose_inf = pmax(as.Date(time_since_last_dose), as.Date(time_since_last_inf), as.Date(time_since_last_reinf), na.rm =  TRUE))
  
  return(merged)
}


calibration <- function(df) {
  
  #convert time since last dose/inf into months before 9/1/22
  months_since <- df %>% mutate(months_since_last_dose_inf = as.factor(interval(time_since_last_dose_inf,as.Date('2022-09-01')) %/% months(1)))
  
  #merge with protective effectiveness
  combined <- merge(merge(months_since, waning_data_clean, by.x = c("months_since_last_dose_inf", "age_group", "prior_inf"), by.y = c("months", "age_group", "prior_inf"), all.x = TRUE),
                    avg_incidence_adj, by = "age_group", all.x = TRUE)
  
  combined$ve_pred[combined$time_since_last_inf>="2022-06-01"] <- 1
  combined$ve_pred[combined$time_since_last_reinf>="2022-06-01"] <- 1
  
  #Calibrate risk factor (Lambda)
  pe_list <- combined$ve_pred
  avg_inc <- (combined$avg_inc)[1]
  
  lambda_calibration <- function(lambda) {
    lambda_cal <- mean(lambda * (1-pe_list))
    return(((lambda_cal - avg_inc)^2))
  }
  
  lambda <- nlm(lambda_calibration, p = c(0.01))
  print(lambda$estimate)
  
  return(combined %>% mutate(lambda = lambda$estimate))
}

save_results <- function(df){
  age_group <- (df$age_group)[1]
  write.csv(df, paste0("results/calibration/main/immunoMild/waning-", waning, "/sero-", sero, "/1mil-",age_group,"-monthly.csv")) 
  
}

##################################################################################################
#Set up folder structure to save calibration results
#NOTE:    Depending on which sensitivity analysis you are running calibration for, change out 'main' folders
#         for a different name ('opt-ve', 'opt-waning', 'pess-ve', 'pess-waning', 'low-sero', 'high-sero', '100-sero').
#         Other sensitivity analyses, such as the five year simulation or delayed vaccine administration use the calibrations
#         from the main analysis, and won't require a separate calibration.

dir.create("results/calibration")
dir.create("results/calibration/main")
dir.create("results/calibration/main/immunoMild")

for (waning in c("upper", "mean", "lower")) {
  dir.create(paste0("results/calibration/main/immunoMild/waning-", waning))
  
  for (sero in c("upper", "mean", "lower")) {
    dir.create(paste0("results/calibration/main/immunoMild/waning-", waning, "/sero-", sero))
  }
}

#Set seroprevalence estimates
seroprev_estimates <- c(0.8238, 0.6579, 0.4681, 0.4681) #Main
#seroprev_estimates <- pmin((c(0.8238, 0.6579, 0.4681, 0.4681) * 1.25), 1) #Higher Seroprevalence
#seroprev_estimates <- (c(0.8238, 0.6579, 0.4681, 0.4681) * .75) #Lower Seroprevalence
#seroprev_estimates <- c(1,1,1,1) #100% Seroprevalence


#Running Calibration on Population of 1,000,000 
for (waning in c("upper", "mean", "lower")) {
  
  waning_data_clean <- waning_data %>% filter(estimate == waning)
  for (sero in c("upper", "mean", "lower")) {
    
    #Create matrices for each age group
    set.seed(88)
    age_18_49_cal <- data.frame(individual = c(1:1000000),
                                age_group = '18-49 years',
                                num_doses = '3-dose',
                                prior_inf = case_when(sero == "upper" ~ rbinom(1000000, 1, min(seroprev_estimates[1] + 0.25, 1)),
                                                      sero == "mean" ~ rbinom(1000000, 1, seroprev_estimates[1]),
                                                      TRUE ~ rbinom(1000000, 1, max(seroprev_estimates[1] - 0.1, 0))))
    
    set.seed(88)
    age_50_64_cal <- data.frame(individual = c(1:1000000),
                                age_group = '50-64 years',
                                num_doses = sample(c('3-dose', '4-dose'), 1000000, prob = c(0.6, 0.4), replace = TRUE),
                                prior_inf = case_when(sero == "upper" ~ rbinom(1000000, 1, min(seroprev_estimates[2] + 0.25, 1)),
                                                      sero == "mean" ~ rbinom(1000000, 1, seroprev_estimates[2]),
                                                      TRUE ~ rbinom(1000000, 1, max(seroprev_estimates[2] - 0.1, 0))))
    
    set.seed(88)
    age_65_74_cal <- data.frame(individual = c(1:1000000),
                                age_group = '65-74 years',
                                num_doses = sample(c('3-dose', '4-dose'), 1000000, prob = c(0.6, 0.4), replace = TRUE),
                                prior_inf = case_when(sero == "upper" ~ rbinom(1000000, 1, min(seroprev_estimates[3] + 0.25, 1)),
                                                      sero == "mean" ~ rbinom(1000000, 1, seroprev_estimates[3]),
                                                      TRUE ~ rbinom(1000000, 1, max(seroprev_estimates[3] - 0.1, 0))))
    
    
    set.seed(88)
    age_75_plus_cal <- data.frame(individual = c(1:1000000),
                                  age_group = '75+ years',
                                  num_doses = as.character(sample(c('3-dose', '4-dose'), 1000000, prob = c(0.6, 0.4), replace = TRUE)),
                                  prior_inf = case_when(sero == "upper" ~ rbinom(1000000, 1, min(seroprev_estimates[4] + 0.25, 1)),
                                                        sero == "mean" ~ rbinom(1000000, 1, seroprev_estimates[4]),
                                                        TRUE ~ rbinom(1000000, 1, max(seroprev_estimates[4] - 0.1, 0))))
    
    set.seed(88)
    calibration_results <- list(age_18_49_cal, age_50_64_cal, age_65_74_cal, age_75_plus_cal) %>%
      lapply(time_since_last) %>%
      lapply(calibration)
    
    calibration_results %>% lapply(save_results)
    
    
  }
}

##################################################################################################
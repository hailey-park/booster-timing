###################################################################################################
#Title: Simulation
#Author: Hailey Park
#Date: March 21, 2023
###################################################################################################

rm(list=ls())

setwd("/mnt/projects/covid_partners/ucsf_lo/Booster Timing")

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
library(parallel)
library(foreach)
library(doParallel)
library(iterators)

#Load data
severe_outcomes <- read.csv("data/severe_outcomes.csv")[,-1]
#case_outcomes <- read.csv("data/case_outcomes.csv")[,-1]
waning_absolute_3dose <- read.csv("data/ve_waning_prediction_severe_agestrat_alldose.csv")[,-1]
#ve_waning_cases <- read.csv("data/ve_waning_absolute_cases.csv")[,-1] #old
hosp_death_age_stratified <- read.csv("data/hosp_death_age_stratified_counts.csv")[,-1]
waning_relative_4dose <- read.csv("data/relative-ve-4dose-lin.csv")


inspection <- merge(severe_outcomes, (severe_outcomes %>% group_by(age_group) %>% summarise(avg_num_severe = mean(num_severe))),
                    by = "age_group", all.x = TRUE) %>% select(age_group, month, num_severe, avg_num_severe, inc_per100000, avg_inc_per100000)



#Clean data
waning_data_clean <- waning_absolute_3dose %>% filter(num_doses == '3-dose') %>% dplyr::select(months, prior_inf, age_group, ve_pred)
age_65_plus_waning <- (waning_data_clean %>% filter(age_group == '65+ years'))[rep(seq_len(nrow(waning_data_clean %>% filter(age_group == '65+ years'))), 2),] %>%
  mutate(age_group = rep(c("65-74 years", "75+ years"), each = 50))
waning_data_clean <- droplevels(rbind((waning_data_clean %>% filter(age_group != '65+ years')), age_65_plus_waning)) %>% arrange(months) %>%
  mutate(months = rep(c(0.5, 1:24), each = 8))
waning_relative_4dose <- rbind((waning_relative_4dose %>% rename(months = Month, relative_ve = ve)),
                               data.frame(months = 13:24,
                                          relative_ve = 0))
severe_clean <- severe_outcomes %>% mutate(month = as.Date(month))
cases_clean <- case_outcomes %>% mutate(month = as.Date(month))
all_outcomes <- merge(cases_clean %>% dplyr::select(-c(num_cases, total_booster_pop, inc_per100000)) %>% rename(avg_inc_cases = avg_inc_per100000),
                      severe_clean %>% dplyr::select(-c(total_booster_pop, inc_per100000)) %>% rename(avg_inc_severe = avg_inc_per100000),
                      by = c("month", "age_group"), all.x = TRUE) %>% mutate(risk_compare = avg_inc_cases/avg_inc_severe)

#Converting bivalent absolute ve_waning data 
absolute_ve <- function(relative_ve, absolute_ve) {
  relative_rr <- 1 - relative_ve
  absolute_rr_lower <- 1 - absolute_ve
  absolute_rr_upper <- relative_rr * absolute_rr_lower
  absolute_ve_upper <- 1 - absolute_rr_upper
  return(absolute_ve_upper)
}

waning_absolute_4dose <- merge(waning_data_clean, waning_relative_4dose, by = "months", all.x = TRUE) %>% mutate(ve_biv = absolute_ve(relative_ve/100, ve_pred)) %>%
  dplyr::select(prior_inf, age_group, months, ve_biv) %>% rename(ve_pred = ve_biv) 


#Read in matrices for each age group
age_18_49 <- read.csv("calibration-results/calibration-1million-18-49 years.csv")[,-1]
age_50_64 <-  read.csv("calibration-results/calibration-1million-50-64 years.csv")[,-1]
age_65_74 <-  read.csv("calibration-results/calibration-1million-65-74 years.csv")[,-1]
age_75_plus <-  read.csv("calibration-results/calibration-1million-75+ years.csv")[,-1]

#clean age matrices
clean_age_matrix <- function(df){
  df %>% dplyr::select(c("individual", "age_group", "prior_inf", "months_since_last_dose_inf", "num_doses","lambda"))
}

clean_df <- list(age_18_49, age_50_64, age_65_74, age_75_plus) %>%
  lapply(clean_age_matrix) 

###########################################################################################
simulation_counts <- data.frame(month = c(1:24), `age_18_49` = NA, `age_50_64` = NA, `age_65_74` = NA,`age_75plus` = NA)

time = 24 
counter <- 1
for (age in c("18-49 years", "50-64 years", "65-74 years", "75+ years")) {
  
  age_grouped <- cases_clean %>% filter(age_group == age) 
  total_pop <- 10000
  outcomes_by_month <- rep(NA, 24)
  avg_incidence <- age_grouped$avg_inc_per100000[1]/100000
  
  for (specific_month in c(1:time)){
    total <- rbinom(1, total_pop,  prob = avg_incidence)
    outcomes_by_month[specific_month] <- total
    total_pop <- total_pop - total
  }
  
  simulation_counts[,1+counter] <- outcomes_by_month
  counter <- counter + 1
}

inspection <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                         num_outcomes_sim = c(sum(simulation_counts$age_18_49),
                                            sum(simulation_counts$age_50_64),
                                            sum(simulation_counts$age_65_74),
                                            sum(simulation_counts$age_75plus)
                                            ))

cumulative_counts <- melt(simulation_counts %>% mutate(age_18_49 = cumsum(age_18_49),
                                                  age_50_64 = cumsum(age_50_64),
                                                  age_65_74 = cumsum(age_65_74),
                                                  age_75plus = cumsum(age_75plus)), id = "month")

ggplot(data = cumulative_counts, aes(x = month, y = value, color = variable)) +
  geom_line() +
  ylab("Total Case Outcomes") +
  xlab("Months") +
  scale_color_discrete(name = "Age Groups",
                       labels = c("18-49 years", "50-64 years", "65-74 years", "75+ years")) +
  ggtitle("Cumulative Total Case Outcomes over 2 Year Simulation Period")



###########################################################################################
#Simulation Base Case Scenario (Bivalent Booster at the Start of the Simulation)

#Function for outcome occurrence based on risk (Risk = Lambda* (1 - PE))
#NOTE: Using 4-dose PE because everyone (including 3-dose individuals) getting bivalent booster
#      at start
severe_outcome <- function(age, inf, time, lambda) {
  pe <- (waning_absolute_4dose %>% filter(age_group == age, prior_inf == inf, months == time))$ve_pred
  risk <- lambda * (1 - pe)
  return(rbinom(1, 1, risk))
}

#Function for averted outcomes
averted_outcome <- function(time) {
  rel_ve <- (waning_relative_4dose %>% filter(months == time))$relative_ve
  return(1 * rel_ve)
}


#Store averted outcomes in new df
averted_age_75_plus <- clean_df[[1]]
averted_age_75_plus[sprintf("month%s",(0:24))] <- NA
averted_age_75_plus['total_deaths'] <- 0
averted_age_75_plus['total_hosps'] <- 0
averted_age_75_plus['perc_death'] <- (hosp_death_age_stratified %>% filter(age_group == '75+ years'))$perc_death

tester <- averted_age_75_plus[1:100,]

#Parallelizing
myCluster <- makeCluster(7, type = 'PSOCK')
registerDoParallel(myCluster)

system.time( {
  output <- foreach(i = 1:nrow(tester), .combine=rbind, .packages = 'dplyr') %dopar% {

      #Individual's info (age_group, num_doses, prior_inf, etc.)
  individual <- tester[i,]
  age <- as.character(individual$age_group)
  doses <- as.character(individual$num_doses)
  inf <- individual$prior_inf
  time_since_last <- 0 #start at 0 because of bivalent booster @ beginning of simulation
  lambda <- individual$lambda
  prob_death <- individual$perc_death
  
  averted_outcomes <- rep(0, 25)
  perfect_immunity_tracker <- 0
  hosp_count <- 0
  death_count <- 0
  months <- c(0.5, 1:24)
  
  for(j in (1:25)) {
    
    month <- months[j]
    
    #Does outcome occur?
    outcome <- severe_outcome(age, inf, month, lambda)
    
    #If severe outcome occurs and the 'perfect immunity' tracker isn't triggered yet, calculate averted 
    # outcomes and check if hosp vs. death. ('perfect immunity' tracker used for when previous hosps occur)
    if(outcome == 1) { #& perfect_immunity_tracker == 0) {
      averted_outcomes[j] <- averted_outcome(month)
      
      #Change prior infection status to 1
      inf <- 1
      
      #is outcome hosp or death?
      death_ind <- rbinom(1,1,prob_death)
      
      if(death_ind == 1) {
        death_count <- death_count + 1
        print(paste0("Individual: ", i, " had a death"))
        break
      }else{
        hosp_count <- hosp_count + 1
        perfect_immunity_tracker <- 3
        time_since_last <- 0
        print(paste0("Individual: ", i, " had a hosp"))
        next
      }
    }
    # #If severe outcome occurs but 'perfect immunity' tracker is triggered, avert outcome bc perfect immunity
    # # and move onto next month
    # else if(outcome == 1 & perfect_immunity_tracker > 0){
    #   averted_outcomes[j] <- 1
    #   perfect_immunity_tracker <- perfect_immunity_tracker - 1
    #   next
    # }
    #No severe outcome occurs (outcome == 0)
    else {time_since_last <- time_since_last + 1}
    
  }
  tester[i, 7:31] <- averted_outcomes
  tester[i, 'total_hosps'] <- hosp_count
  tester[i, 'total_deaths'] <- death_count
  tester[i,,drop = FALSE]
}
  })




months <- c(0.5, 1:24)









i <- 1
j <- 1

for (i in (1:1000000)) {
  print(i)
  
  #Individual's info (age_group, num_doses, prior_inf, etc.)
  individual <- averted_age_75_plus[i,]
  age <- as.character(individual$age_group)
  doses <- as.character(individual$num_doses)
  inf <- individual$prior_inf
  time_since_last <- 0 #start at 0 because of bivalent booster @ beginning of simulation
  lambda <- individual$lambda
  
  averted_outcomes <- rep(0, 25)
  perfect_immunity_tracker <- 0
  
  for(j in (1:25)) {
  
    month <- months[j]
    #Calculate risk of severe outcomes (Avg Inc = Risk Factor * (1 - PE))
    #NOTE: Using 4-dose PE because everyone (including 3-dose individuals) getting bivalent booster at start
    pe <- (waning_absolute_4dose %>% filter(age_group == age, prior_inf == inf, months == month))$ve_pred
    risk <- lambda * (1 - pe)
    outcome <- rbinom(1, 1, risk)
    
    #If severe outcome occurs and the 'perfect immunity' tracker isn't triggered yet, calculate averted 
    # outcomes and check if hosp vs. death. ('perfect immunity' tracker used for when previous hosps occur)
    if(outcome == 1 & perfect_immunity_tracker == 0) {
      rel_ve <- (waning_relative_4dose %>% filter(months == month))$relative_ve
      averted_outcomes[j] <- 1 * rel_ve
      
      #Change prior infection status to 1
      inf <- 1
      
      #is outcome hosp or death?
      prob_death <- (hosp_death_age_stratified %>% filter(age_group == age))$perc_death
      death_ind <- rbinom(1,1,prob_death)
      
      if(death_ind == 1) {
        death_count <- death_count + 1
        print(paste0("Individual: ", i, " had a death"))
        break
      }else{
        hosp_count <- hosp_count + 1
        perfect_immunity_tracker <- 3
        time_since_last <- 0
        print(paste0("Individual: ", i, " had a hosp"))
        next
      }
    }
    #If severe outcome occurs but 'perfect immunity' tracker is triggered, avert outcome bc perfect immunity
    # and move onto next month
    else if(outcome == 1 & perfect_immunity_tracker > 0){
      averted_outcomes[j] <- 1
      perfect_immunity_tracker <- perfect_immunity_tracker - 1
      next
    }
    #No severe outcome occurs (outcome == 0)
    else {time_since_last <- time_since_last + 1}
    
  }
  
  averted_age_75_plus[i, 7:31] <- averted_outcomes
  
}


colSums(averted_age_75_plus[,-(1:6)], na.rm = TRUE)
sum(colSums(averted_age_75_plus[,-(1:6)]))
death_count
hosp_count
death_count + hosp_count





simulation_results <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                 total_severe_outcomes = c(0, 1, 3, 6),
                                 inc_per_100000 = c(0, 10, 30, 60))

simulation_results_time <- data.frame(months = rep(c(0.5, 1:24), 4),
                                      age_group = rep( c("18-49 years", "50-64 years", "65-74 years", "75+ years"), each = 25),
                                      averted_outcomes = c(0,0,53,rep(0,22),0,0,52,46,rep(0,21),142,0,155,46,rep(0,21),71,348,155,186,rep(0,21)))

ggplot(simulation_results_time, aes(x = months, y = averted_outcomes, color = age_group)) +
  geom_line()+
  xlab("Months") +
  ylab("Total Averted Outcomes") +
  ggtitle("Total Averted Outcomes after 1 Bivalent Booster at start of 2 Year Simulation \nfor each age group ")
###########################################################################################
observed_data <- severe_clean %>% rename(num_severe_observed = num_severe) %>% 
  group_by(age_group) %>% summarise(total_severe = sum(num_severe_observed))

merged_data <- merge(calibration_data, observed_data, by = c("month", "age_group"), all.x = TRUE) %>% select(-total_booster_pop) %>%
  group_by(age_group) %>% summarise(num_severe_sim = sum(num_severe_sim),
                                    num_severe_observed = sum(num_severe_observed),
                                    avg_inc_per100000 = mean(avg_inc_per100000))









#fill out age_matrix with outcomes that would have occurred w/o bivalent booster
# for(month in (0:24)) {
#   age_18_49[sprintf("month%s",month)] <- rbinom(10000,1,prob = avg_incidence$avg_inc[1])
#   age_50_64[sprintf("month%s",month)] <- rbinom(10000,1,prob = avg_incidence$avg_inc[2])
#   age_65_74[sprintf("month%s",month)] <- rbinom(10000,1,prob = avg_incidence$avg_inc[3])
#   age_75_plus[sprintf("month%s",month)] <- rbinom(10000,1,prob = avg_incidence$avg_inc[4])
# }
# 
# colSums(age_75_plus[,-(1:4)])












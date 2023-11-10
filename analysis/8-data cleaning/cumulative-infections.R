###################################################################################################
#Title: Cumulative Infection Calculation
#Author: Hailey Park
#Date: April 24, 2023
###################################################################################################

rm(list=ls())

setwd("~/Stanford Research/booster-timing")

#Loading in libraries
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

#Load in data
death_data  <- read.csv("data/raw-data/Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Updated__Bivalent__Booster_Status.csv")

###################################################################################################
#Looking at Cumulative Infections over 2/2022 - 9/2022

cum_infections <- death_data %>%
  filter(outcome == "case",
         vaccination_status == "vaccinated",
         month %in% c("MAR 2022", "APR 2022", "MAY 2022", "JUN 2022", "JUL 2022", "AUG 2022"),
         age_group %in% c("0.5-4", "5-11", "12-17","18-29", "30-49", "50-64", "65-79", "80+")) %>%
  mutate(new_age = case_when(age_group %in% c("18-29", "30-49") ~ "18-49",
                             age_group %in% c("65-79", "80+") ~ "65+",
                             age_group %in% c("0.5-4", "5-11", "12-17") ~ "0-17",
                             TRUE ~ "50-64")) %>%
  group_by(month, mmwr_week, new_age) %>% summarise(total_unvax_outcomes = sum(unvaccinated_with_outcome),
                                                    total_unvax_pop = sum(unvaccinated_population),
                                                    total_vax_outcomes = sum(vaccinated_with_outcome),
                                                    total_vax_pop = sum(vaccinated_population)) %>%
  group_by(month, new_age) %>% summarise(total_unvax_outcomes = sum(total_unvax_outcomes),
                                         total_unvax_pop = min(total_unvax_pop),
                                         total_vax_outcomes = sum(total_vax_outcomes),
                                         total_vax_pop = max(total_vax_pop)) %>%
  mutate(total_outcomes = total_unvax_outcomes + total_vax_outcomes,
         total_pop = total_unvax_pop + total_vax_pop) %>%
  group_by(new_age) %>% summarise(total_outcomes = sum(total_outcomes),
                                  total_pop = mean(total_pop)) %>%
  mutate(inc_perc = total_outcomes/total_pop * 100,
         inc_perc_with_2x = inc_perc * 2) #2x case ascertainment multiplier
  
  



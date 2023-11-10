###################################################################################################
#Title: COVID-19 Hospitalization vs. Death counts
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
hosp_inc_data <- read.csv("data/clean-data/monthly-incidence-estimates.csv")[,-1]

#Estimating monthly incidence of deaths in vaccinated groups (AGE-STRATIFIED)
#NOTE:      Used data from CDC
#           Link: https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status
death_inc_vax <- death_data %>%
  filter(outcome == "death",
         vaccination_status == "vaccinated",
         month %in% c("MAR 2022", "APR 2022", "MAY 2022", "JUN 2022", "JUL 2022", "AUG 2022"),
         age_group %in% c("18-29", "30-49", "50-64", "65-79", "80+")) %>%
  mutate(new_age = if_else(age_group %in% c("18-29", "30-49"), "18-49", age_group)) %>%
  group_by(month, mmwr_week, new_age) %>% summarise(total_vax_outcomes = sum(vaccinated_with_outcome),
                                                    total_vax_pop = sum(vaccinated_population)) %>%
  group_by(month, new_age) %>% summarise(total_vax_outcomes = sum(total_vax_outcomes),
                                         total_vax_pop = max(total_vax_pop)) %>%
  mutate(inc_vax = total_vax_outcomes/total_vax_pop * 1000000) %>%
  group_by(new_age) %>% summarise(avg_inc_vax = mean(inc_vax)) 


hosp_death_age_stratified_counts_adj <- data.frame(age_group = hosp_inc_data$age_group,
                         hosp_inc_vax = hosp_inc_data$avg_inc,
                         death_inc_vax = death_inc_vax$avg_inc_vax/1000000) %>%
  rowwise() %>% mutate(perc_death = death_inc_vax/sum(hosp_inc_vax, death_inc_vax))


write.csv(hosp_death_age_stratified_counts_adj, "data/clean-data/hosp_death_age_stratified_counts_adj_v2.csv")

###################################################################################################


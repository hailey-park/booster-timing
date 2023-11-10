###################################################################################################
#Title: Monthly Incidence Estimates 
#Author: Hailey Park
#Date: April 24, 2023
###################################################################################################

rm(list=ls())

setwd("~/Stanford Research/booster-timing-final")

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
hosp_data <- read.csv("data/raw-data/COVID-19Surveillance_All_Data.csv", skip = 2)
vax_data <- read.csv("data/raw-data/Archive__COVID-19_Vaccination_and_Case_Trends_by_Age_Group__United_States.csv")
death_data  <- read.csv("data/raw-data/Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Updated__Bivalent__Booster_Status.csv")

#Estimating monthly incidence of COVID-19 hospitalizations by age group
#NOTE:    Used data from CDC COVID-NET Surveillance system on COVID-19 Hospitalizations.
#         Link: https://gis.cdc.gov/grasp/COVIDNet/COVID19_3.html
#         Used 2021 US age distribution data from Statista to correct for our age bins
#         Link: https://www.statista.com/statistics/241488/population-of-the-us-by-sex-and-age/


monthly_inc_gen_pop <- hosp_data %>% 
  filter(CATCHMENT == "Entire Network",
         NETWORK == "COVID-NET",
         MMWR.YEAR == 2022,
         AGE.CATEGORY %in% c("18-49 yr", "50-64 yr", "65-74 yr", "75-84 yr", "85+"),
         MMWR.WEEK %in% c(10:35)) %>% 
  mutate(WEEKLY.RATE = as.numeric(WEEKLY.RATE)) %>%
  dplyr::select(AGE.CATEGORY, WEEKLY.RATE) %>% group_by(AGE.CATEGORY) %>% summarise(weekly_rate = mean(WEEKLY.RATE)) %>%
  mutate(monthly_rate = weekly_rate *  4.345) %>% 
  add_row(AGE.CATEGORY = "75 + yr",  #Converting the 75-84 year and 85+ year age groups into single 75+ age group
          weekly_rate = (30.157692 * 0.738) + (56.653846 * 0.262), 
          monthly_rate = (131.03517 * 0.738) + (246.16096 * 0.262)) %>%
  filter(!AGE.CATEGORY %in% c("75-84 yr", "85+"))


#Estimating monthly incidence of deaths in unvaccinated groups is to the vaccinated groups (AGE-STRATIFIED)
#NOTE:      Used data from CDC
#           Link: https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status
inc_unvax_vs_vax <- death_data %>%
  filter(outcome == "death",
         vaccination_status == "vaccinated",
         month %in% c("MAR 2022", "APR 2022", "MAY 2022", "JUN 2022", "JUL 2022", "AUG 2022"),
         age_group %in% c("18-29", "30-49", "50-64", "65-79", "80+")) %>%
  mutate(new_age = if_else(age_group %in% c("18-29", "30-49"), "18-49", if_else(age_group %in% c("65-79", "80+"), "65+", age_group))) %>%
  group_by(month, mmwr_week, new_age) %>% summarise(total_unvax_outcomes = sum(unvaccinated_with_outcome),
                                         total_unvax_pop = sum(unvaccinated_population),
                                         total_vax_outcomes = sum(vaccinated_with_outcome),
                                         total_vax_pop = sum(vaccinated_population)) %>%
  group_by(month, new_age) %>% summarise(total_unvax_outcomes = sum(total_unvax_outcomes),
                                                    total_unvax_pop = min(total_unvax_pop),
                                                    total_vax_outcomes = sum(total_vax_outcomes),
                                                    total_vax_pop = max(total_vax_pop)) %>%
  mutate(inc_unvax = total_unvax_outcomes/total_unvax_pop * 1000000,
         inc_vax = total_vax_outcomes/total_vax_pop * 1000000) %>%
  group_by(new_age) %>% summarise(avg_inc_unvax = mean(inc_unvax),
                                  avg_inc_vax = mean(inc_vax)) %>%
  mutate(X_times_higher = avg_inc_unvax/avg_inc_vax) %>%
  rename(age_group = new_age)

#Estimating total vaccinated and unvaccinated counts by age group 
#NOTE:      Used data from CDC COVID Data Tracker to get proportions of fully vaccinated by age group over time
#           Link: https://data.cdc.gov/Vaccinations/Archive-COVID-19-Vaccination-and-Case-Trends-by-Ag/gxj9-t96f
#           Used 2021 age distribution data from Statista, which can be used to calculate total unvaccinated/vaccinated by age
#           Link: https://www.statista.com/statistics/241488/population-of-the-us-by-sex-and-age/

prop_vax_by_age <- vax_data %>%
  mutate(Date = substring(Date.Administered, 1, 10)) %>%
  filter(Date == "08/31/2022",
         AgeGroupVacc %in% c("18 - 24 Years", "25 - 49 Years", "50 - 64 Years", "65+ Years")) %>%
  dplyr::select(AgeGroupVacc, Series_Complete_Pop_pct_agegroup) %>%
  add_row(AgeGroupVacc = "18 - 49 Years", Series_Complete_Pop_pct_agegroup = (0.652 * 0.217) + (0.707 * 0.783)) %>% #Converting 18-24 year and 25-49 year age groups into single 18-49 year age group
  filter(!AgeGroupVacc %in% c("18 - 24 Years", "25 - 49 Years")) 

total_unvax_vax <- prop_vax_by_age %>%
  mutate(total_pop = case_when(AgeGroupVacc == "18 - 49 Years" ~ 138820000,
                               AgeGroupVacc == "50 - 64 Years" ~ 63710000,
                               TRUE ~ 55860000),
         total_vax_pop = Series_Complete_Pop_pct_agegroup * total_pop,
         total_unvax_pop = total_pop - total_vax_pop) %>%
  add_row(AgeGroupVacc = c("65 - 74 Years", "75+ Years"),  #Converting 65+ age group into 65-74 year and 75+ year age groups
          Series_Complete_Pop_pct_agegroup = c(0.922000, 0.922000),
          total_pop = c(55860000 * 0.60275, 55860000 * 0.39725),
          total_vax_pop = c(51502920 * 0.60275, 51502920 * 0.39725),
          total_unvax_pop = c(4357080 * 0.60275, 4357080 * 0.39725)) %>%
  filter(AgeGroupVacc != "65+ Years")


#Adjusting age-stratified monthly incidence estimates for the vaccinated population ONLY
monthly_inc_vax_pop <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                      avg_inc_everyone = c(15.2, 27.9, 57.9,161.2),
                      total_unvax = c(42331077, 11212960, 2626230, 1730850),
                      total_vax = c(96488923, 52497040, 31043385, 20459535),
                      X_times_higher = c(4.03,5.33,6.51,6.51)) %>% mutate(total_pop = total_unvax + total_vax,
                                                        left_side = (((avg_inc_everyone/100000) * total_pop)/total_vax) * X_times_higher,
                                                        unvax_cases = left_side/((X_times_higher/total_vax) + (1/total_unvax)),
                                                        vax_cases = (unvax_cases/total_unvax) * (total_vax/X_times_higher),
                                                        adj_inc_vax = ceiling((vax_cases/total_vax) * 100000)/100000)

write.csv(monthly_inc_vax_pop %>% dplyr::select(age_group, adj_inc_vax) %>% rename(avg_inc = adj_inc_vax), "data/clean-data/monthly-incidence-estimates.csv")
###################################################################################################

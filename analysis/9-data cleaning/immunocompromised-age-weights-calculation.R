###################################################################################################
#Title: Immunocompromised Age Weights Estimates 
#Author: Hailey Park
#Date: April 24, 2023
###################################################################################################

rm(list=ls())

setwd(here::here())

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
monthly_inc <- read.csv("data/clean-data/monthly-incidence-estimates.csv")[,-1]

#Pull data from literature

#Percent Immunocompromised by Age Group
#I used estimates from 2016-2017 in Figure 2; https://wwwnc.cdc.gov/eid/article/26/8/19-1493-f2
perc_immuno_by_age <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                 perc_immuno = c(5.8, 9.9, 13.9, 13.9))

#Percent Age Group in Total Population
#From https://www.statista.com/statistics/241488/population-of-the-us-by-sex-and-age/
perc_age_pop <- data.frame(age_group = c("18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                 perc_age = c(41.8, 19.2, 10.1, 6.7))

#Create age-weights
age_weights_immuno_pop <- merge(perc_age_pop, perc_immuno_by_age, by = "age_group") %>%
  mutate(perc_age_and_immuno_in_total_pop = perc_age * perc_immuno, #this is the percent that is immunocompromised and in age group among total population
         perc_age_among_immuno_pop = perc_age_and_immuno_in_total_pop/sum(perc_age_and_immuno_in_total_pop)) #this is the percent age group among the immunocompromised population

write.csv(age_weights_immuno_pop, "data/clean-data/age-weights-immunocompromised.csv")
###################################################################################################

###################################################################################################
#Title: Waning Curves for Variant Scenario Analyses
#Author: Hailey Park
#Date: November 1, 2023
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


#Read in waning prediction data (can switch in immunoMild or immunoSevere too)
severe_waning_data <- read.csv("results/waning-predictions/main/severe_waning_predictions_monthly.csv")[,-1]
nonsevere_waning_data <- read.csv("results/waning-predictions/main/nonsevere_waning_predictions_monthly.csv")[,-1]

#Creating the waning curves under a novel variant
#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%
#       3 sets of waning curves are created
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected under novel variant period)

upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

downgraded_immunity <- severe_waning_data %>% 
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data <- rbind(downgraded_immunity, upgraded_hybrid_immunity)

write.csv(combined_waning_data, "results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")

#Creating the waning curves under a novel variant and updated vaccines (specific to Variant Scenario 4 only)
#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%
#       3 sets of waning curves are created
#         prior_inf == 0: These are the upgraded vaccine-induced immunity waning curves (vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected under novel variant period)

upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

upgraded_vaccine_immunity <- severe_waning_data %>% filter(prior_inf == 0) 

downgraded_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data <- rbind(downgraded_immunity, upgraded_hybrid_immunity, upgraded_vaccine_immunity)

write.csv(combined_waning_data, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/severe_waning_predictions_monthly.csv")


###################################################################################################
#Check waning curves
#read in data
plot_data <- read.csv("results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")[,-1]

plot_data %>% 
  filter(estimate == "mean") %>%
  mutate(prior_inf=as.factor(prior_inf)) %>% 
  ggplot(aes(months, ve_pred, group=interaction(age_group, prior_inf), color=age_group, shape=prior_inf)) + 
  geom_line() + geom_point() +
  ylim(0,1)

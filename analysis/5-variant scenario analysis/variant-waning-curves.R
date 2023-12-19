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
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (previously vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original waning curve)

upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

downgraded_immunity <- severe_waning_data %>% 
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data <- rbind(downgraded_immunity, upgraded_hybrid_immunity)

write.csv(combined_waning_data, "results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")

#Creating additional waning curves under a novel variant and updated vaccines (specific to Variant Scenario 4 only)
#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%
#       3 sets of waning curves are created
#         prior_inf == 0: These are the upgraded vaccine-induced immunity waning curves (vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original waning curves)

upgraded_immunity <- severe_waning_data %>%
  mutate(prior_inf = case_when(prior_inf == 0 ~ 2, #restored vaccine-induced immunity
                               prior_inf == 1 ~ 4)) #restored hybrid immunity

partially_upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.05),
         prior_inf = 3) #partially-restored hybrid immunity (if infected before simulation start and receives updated booster)

downgraded_immunity <- severe_waning_data %>%
  rowwise() %>% 
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
#mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2


combined_waning_data <- rbind(downgraded_immunity, upgraded_immunity, partially_upgraded_hybrid_immunity)




#Creating additional waning curves under a novel variant and updated vaccines (specific to Variant Scenario 4 only)
#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%
#       3 sets of waning curves are created
#         prior_inf == 0: These are the upgraded vaccine-induced immunity waning curves (vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original waning curves)

upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 1)

downgraded_hybrid_immunity <- as.data.frame(combined_waning_data) %>% filter(prior_inf %in% c(4,3)) %>%
  rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
#mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

vaccine_immunity <- as.data.frame(combined_waning_data) %>% filter(prior_inf %in% c(0,2)) 

partially_upgraded_hybrid_immunity <- severe_waning_data %>% filter(prior_inf == 1) %>%
  rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.05),
         prior_inf = 5) #partially-restored hybrid immunity (if infected during novel variant period 1 and receives updated booster)

combined_waning_data <- rbind(downgraded_hybrid_immunity, upgraded_hybrid_immunity, vaccine_immunity, partially_upgraded_hybrid_immunity)

write.csv(combined_waning_data, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/severe_waning_predictions_monthly.csv")


###################################################################################################
#Check waning curves
#read in data
plot_data <- read.csv("results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")[,-1]

plot_data <- as.data.frame(combined_waning_data) %>%
  mutate(immunity = case_when(prior_inf == 0 ~ "Downgraded vaccine-induced immunity",
                              prior_inf == 4 ~ "Downgraded hybrid immunity \n(if infected during novel variant period 1)",
                              prior_inf == 3 ~ "Downgraded hybrid immunity \n(if infected before simulation start and received \nupdated booster during novel variant period 1)",
                              prior_inf == 2 ~ "Restored vaccine-induced immunity \n(if received updated vaccine)",
                              prior_inf == 1 ~ "Restored hybrid immunity \n(if infected during current novel variant 2)",
                              prior_inf == 5 ~ "Partially-restored hybrid immunity \n(if infected during novel variant 1 \nand receives updated booster"))
  
  
  as.data.frame(combined_waning_data) %>%
  mutate(immunity = case_when(prior_inf == 0 ~ "Downgraded vaccine-induced immunity",
                              prior_inf == 1 ~ "Downgraded hybrid immunity \n(if infected before simulation start)",
                              prior_inf == 3 ~ "Partially-restored hybrid immunity \n(if infected before simulation start \nand receives updated booster)",
                              prior_inf == 2 ~ "Restored vaccine-induced immunity \n(if received updated vaccine)",
                              prior_inf == 4 ~ "Restored hybrid immunity \n(if infected during current novel variant 1)"))
  
  
plot_data %>% 
  filter(estimate == "mean", age_group == "65-74 years") %>%
  mutate(immunity=as.factor(immunity)) %>% 
  ggplot(aes(months, ve_pred, group=immunity, color=immunity)) + 
  geom_line() + geom_point() +
  ylim(0,1) +
  ggtitle("Novel Variant #2 Waning Curves") +
  theme(legend.key.size = unit(2.5, 'lines'))




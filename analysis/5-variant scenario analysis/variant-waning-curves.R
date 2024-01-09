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

###################################################################################################
#Creating the waning curves under a novel variant (Scenario 1 + 2 only)

#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%
#       3 sets of waning curves are created
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (previously vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original waning curve)

upgraded_hybrid_immunity_severe <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

upgraded_hybrid_immunity_nonsevere <- nonsevere_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

downgraded_immunity_severe <- severe_waning_data %>% 
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

downgraded_immunity_nonsevere <- nonsevere_waning_data %>% 
  mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
#mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data_severe <- rbind(downgraded_immunity_severe, upgraded_hybrid_immunity_severe)
combined_waning_data_nonsevere <- rbind(downgraded_immunity_nonsevere, upgraded_hybrid_immunity_nonsevere)

write.csv(combined_waning_data_severe, "results/waning-predictions/variant-waning/immune-escape-ver1/severe_waning_predictions_monthly.csv")
write.csv(combined_waning_data_nonsevere, "results/waning-predictions/variant-waning/immune-escape-ver1/nonsevere_waning_predictions_monthly.csv")
###################################################################################################
#Creating the waning curves under an annual novel variant (Scenario 3 only)

#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%


#Annual Novel Variant #1
#       3 sets of waning curves are created
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (previously vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original waning curve)

upgraded_hybrid_immunity_severe <- severe_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

upgraded_hybrid_immunity_nonsevere <- nonsevere_waning_data %>% filter(prior_inf == 1) %>%
  mutate(prior_inf = 2)

downgraded_immunity_severe <- severe_waning_data %>% rowwise() %>% 
  #mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

downgraded_immunity_nonsevere <- nonsevere_waning_data %>% rowwise() %>% 
  #mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
 mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data_severe <- rbind(downgraded_immunity_severe, upgraded_hybrid_immunity_severe)
combined_waning_data_nonsevere <- rbind(downgraded_immunity_nonsevere, upgraded_hybrid_immunity_nonsevere)

# write.csv(combined_waning_data_severe, "results/waning-predictions/variant-waning/S3-immune-escape-ver1/novel-var-1/severe_waning_predictions_monthly.csv")
# write.csv(combined_waning_data_nonsevere, "results/waning-predictions/variant-waning/S3-immune-escape-ver1/novel-var-1/nonsevere_waning_predictions_monthly.csv")
# 

#Annual Novel Variant #2
#       4 sets of waning curves are created
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (previously vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period 2)
#         prior_inf == 2: These are the downgraded hybrid immunity waning curves (if individual gets infected with novel variant 1 and enters novel variant period 2)
#         prior_inf == 3: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant 2- protection is restored to original waning curve)

upgraded_hybrid_immunity_severe <- as.data.frame(combined_waning_data_severe) %>% filter(prior_inf == 2) %>%
  mutate(prior_inf = 3)

upgraded_hybrid_immunity_nonsevere <- as.data.frame(combined_waning_data_nonsevere) %>% filter(prior_inf == 2) %>%
  mutate(prior_inf = 3)

downgraded_immunity_severe <- as.data.frame(combined_waning_data_severe) %>% rowwise() %>% 
  #mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

downgraded_immunity_nonsevere <- as.data.frame(combined_waning_data_nonsevere) %>% rowwise() %>% 
  #mutate(ve_pred = max(0, ve_pred - 0.1)) #Immune Evasion Version 1
mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

combined_waning_data_severe <- rbind(downgraded_immunity_severe, upgraded_hybrid_immunity_severe)
combined_waning_data_nonsevere <- rbind(downgraded_immunity_nonsevere, upgraded_hybrid_immunity_nonsevere)

write.csv(combined_waning_data_severe, "results/waning-predictions/variant-waning/S3-immune-escape-ver2/novel-var-2/severe_waning_predictions_monthly.csv")
write.csv(combined_waning_data_nonsevere, "results/waning-predictions/variant-waning/S3-immune-escape-ver2/novel-var-2/nonsevere_waning_predictions_monthly.csv")

###################################################################################################
#Creating the waning curves under an annual novel variant and updated vaccines (Scenario 4 only)

#NOTE:  Variants are modeled under two different immune evasion scenarios: 
#         i) absolute protection is reduced by 10% with circulation of the novel variant
#         ii) absolute protection is reduced by 10%, and rate of waning increases by 5%


#Annual Novel Variant #1
#       5 sets of waning curves are created
#         prior_inf == 0: These are the downgraded vaccine-induced immunity waning curves (previously vaccinated individual enters novel variant period)
#         prior_inf == 1: These are the downgraded hybrid immunity waning curves (previously infected individuals enters novel variant period)
#         prior_inf == 2: These are the upgraded vaccine-induced immunity waning curves (if previously vaccinated individual gets updated booster - protection is restored to original vaccine-induced waning curve)
#         prior_inf == 3: These are the partially upgraded hybrid immunity waning curves (if previously infected individual gets updated booster - protection is partially restored)
#         prior_inf == 4: These are the upgraded hybrid immunity waning curves (if individual gets infected with novel variant - protection is restored to original hybrid immunity waning curve)

lower_pe_severe <- severe_waning_data %>% rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.1, 0)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2


lower_pe_nonsevere <- nonsevere_waning_data %>% rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.1, 0)) #Immune Evasion Version 1
  #mutate(ve_pred = max(0, (ve_pred - months * 0.002) - 0.1)) #Immune Evasion Version 2

updated_vax_hybrid_immunity_severe <- severe_waning_data %>% filter(prior_inf == 1) %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.05, 0), #Immune Evasion Version 1
        #ve_pred = max(0, (ve_pred - months * 0.00104) - 0.05), #Immune Evasion Version 2
         prior_inf = 3)

updated_vax_hybrid_immunity_nonsevere <- nonsevere_waning_data %>% filter(prior_inf == 1) %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.05, 0), #Immune Evasion Version 1
    #ve_pred = max(0, (ve_pred - months * 0.00104) - 0.05), #Immune Evasion Version 2
    prior_inf = 3)

restored_immunity_severe <- severe_waning_data %>%
  mutate(prior_inf = if_else(prior_inf == 0, 2, 4))

restored_immunity_nonsevere <- nonsevere_waning_data %>%
  mutate(prior_inf = if_else(prior_inf == 0, 2, 4))

severe_combined <- as.data.frame(rbind(lower_pe_severe, updated_vax_hybrid_immunity_severe, restored_immunity_severe))
nonsevere_combined <- as.data.frame(rbind(lower_pe_nonsevere, updated_vax_hybrid_immunity_nonsevere, restored_immunity_nonsevere))

write.csv(severe_combined, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/novel-var-1/severe_waning_predictions_monthly.csv")
write.csv(nonsevere_combined, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/novel-var-1/nonsevere_waning_predictions_monthly.csv")

#Plot curves
plot_data <- as.data.frame(severe_combined)

plot_data %>%
  filter(estimate == "mean", age_group == "50-64 years") %>%
  mutate(prior_inf=as.factor(prior_inf)) %>%
  ggplot(aes(months, ve_pred, group=prior_inf, color=prior_inf)) +
  geom_line() + geom_point() +
  ylim(0,1)

#Annual Novel Variant #2
#       6 sets of waning curves are created
#         prior_inf == 2: These are the downgraded vaccine-induced immunity waning curves (if previously vaccinated individual enter novel variant period 2)
#         prior_inf == 3: These are the downgraded hybrid immunity waning curves (if individual infected before simulation start and received updated booster; enters novel variant period #2)
#         prior_inf == 4: These are the downgraded hybrid immunity waning curves (if individual gets infected with novel variant 1 enters novel variant period #2)
#         prior_inf == 5: These are the upgraded vaccine-induced immunity waning curves (if previously vaccinated individual gets updated booster - protection is restored to original vaccine-induced waning curve)
#         prior_inf == 6: These are the partially upgraded hybrid immunity waning curves (if individual infected during novel variant period #1 gets updated booster - protection is partially restored)
#         prior_inf == 7: These are the upgraded hybrid immunity waning curves (if individual gets infected with current novel variant - protection is restored to original hybrid immunity waning curve)


vaccine_immunity_severe <- severe_combined %>% filter(prior_inf %in% c(0, 2)) %>%
  mutate(prior_inf = if_else(prior_inf == 0, 2, 5))

vaccine_immunity_nonsevere <- nonsevere_combined %>% filter(prior_inf %in% c(0, 2)) %>%
  mutate(prior_inf = if_else(prior_inf == 0, 2, 5))

restored_hybrid_immunity_severe <- severe_combined %>% filter(prior_inf %in% c(3,4)) %>%
  mutate(prior_inf = if_else(prior_inf == 3, 6, 7))

restored_hybrid_immunity_nonsevere <- nonsevere_combined %>% filter(prior_inf %in% c(3,4)) %>%
  mutate(prior_inf = if_else(prior_inf == 3, 6, 7))

downgraded_hybrid_severe <- severe_combined %>% filter(prior_inf %in% c(3,4)) %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.1, 0))
    #ve_pred = max(0, (ve_pred - months * 0.002) - 0.1))

downgraded_hybrid_nonsevere <- nonsevere_combined %>% filter(prior_inf %in% c(3,4)) %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.1, 0))
    #ve_pred = max(0, (ve_pred - months * 0.002) - 0.1))

var2_severe_combined <- rbind(vaccine_immunity_severe, restored_hybrid_immunity_severe, downgraded_hybrid_severe)
var2_nonsevere_combined <- rbind(vaccine_immunity_nonsevere, restored_hybrid_immunity_nonsevere, downgraded_hybrid_nonsevere)

write.csv(var2_severe_combined, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/novel-var-2/severe_waning_predictions_monthly.csv")
write.csv(var2_nonsevere_combined, "results/waning-predictions/variant-waning/S4-immune-escape-ver1/novel-var-2/nonsevere_waning_predictions_monthly.csv")

#Plots
plot_data <- as.data.frame(var2_nonsevere_combined)

plot_data %>%
  filter(estimate == "mean", age_group == "50-64 years") %>%
  mutate(prior_inf=as.factor(prior_inf)) %>%
  ggplot(aes(months, ve_pred, group=prior_inf, color=prior_inf)) +
  geom_line() + geom_point() +
  ylim(0,1)

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




###################################################################################################
#Title: Sensitivity Analysis Figures
#Author: Hailey Park
#Date: November 22, 2023
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

###################################################################################################
#Plotting sensitivity analyses
oneBooster_original <- read.csv("results/simulation-results/main/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_high_inc <- read.csv("results/simulation-results/sensitivity/high-incidence/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_low_inc <- read.csv("results/simulation-results/sensitivity/low-incidence/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_100_sero <- read.csv("results/simulation-results/sensitivity/100-sero/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_opt_ve <- read.csv("results/simulation-results/sensitivity/optimistic-ve/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_opt_wan <- read.csv("results/simulation-results/sensitivity/optimistic-waning/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_pes_ve <- read.csv("results/simulation-results/sensitivity/pessimistic-ve/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_pes_wan <- read.csv("results/simulation-results/sensitivity/pessimistic-waning/immuno-mild/1Booster-summarised.csv")[,-1]

annualBooster_original <- read.csv("results/simulation-results/main/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_high_inc <- read.csv("results/simulation-results/sensitivity/high-incidence/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_low_inc <- read.csv("results/simulation-results/sensitivity/low-incidence/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_100_sero <- read.csv("results/simulation-results/sensitivity/100-sero/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_opt_ve <- read.csv("results/simulation-results/sensitivity/optimistic-ve/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_opt_wan <- read.csv("results/simulation-results/sensitivity/optimistic-waning/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_pes_ve <- read.csv("results/simulation-results/sensitivity/pessimistic-ve/immuno-mild/annualBooster-summarised.csv")[,-1]
annualBooster_pes_wan <- read.csv("results/simulation-results/sensitivity/pessimistic-waning/immuno-mild/annualBooster-summarised.csv")[,-1]


biannualBooster_original <- read.csv("results/simulation-results/main/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_high_inc <- read.csv("results/simulation-results/sensitivity/high-incidence/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_low_inc <- read.csv("results/simulation-results/sensitivity/low-incidence/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_100_sero <- read.csv("results/simulation-results/sensitivity/100-sero/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_opt_ve <- read.csv("results/simulation-results/sensitivity/optimistic-ve/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_opt_wan <- read.csv("results/simulation-results/sensitivity/optimistic-waning/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_pes_ve <- read.csv("results/simulation-results/sensitivity/pessimistic-ve/immuno-mild/biannualBooster-summarised.csv")[,-1]
biannualBooster_pes_wan <- read.csv("results/simulation-results/sensitivity/pessimistic-waning/immuno-mild/biannualBooster-summarised.csv")[,-1]


sensitivity<- data.frame(oneBooster_mean = c(colSums(oneBooster_original)[1],
                                             colSums(oneBooster_high_inc)[1],
                                             colSums(oneBooster_low_inc)[1],
                                             colSums(oneBooster_opt_ve)[1],
                                             colSums(oneBooster_opt_wan)[1],
                                             colSums(oneBooster_pes_ve)[1],
                                             colSums(oneBooster_pes_wan)[1],
                                             colSums(oneBooster_100_sero)[1]),
                         oneBooster_max = c(colSums(oneBooster_original)[2],
                                            colSums(oneBooster_high_inc)[2],
                                            colSums(oneBooster_low_inc)[2],
                                            colSums(oneBooster_opt_ve)[2],
                                            colSums(oneBooster_opt_wan)[2],
                                            colSums(oneBooster_pes_ve)[2],
                                            colSums(oneBooster_pes_wan)[2],
                                            colSums(oneBooster_100_sero)[2]),
                         oneBooster_min = c(colSums(oneBooster_original)[3],
                                            colSums(oneBooster_high_inc)[3],
                                            colSums(oneBooster_low_inc)[3],
                                            colSums(oneBooster_opt_ve)[3],
                                            colSums(oneBooster_opt_wan)[3],
                                            colSums(oneBooster_pes_ve)[3],
                                            colSums(oneBooster_pes_wan)[3],
                                            colSums(oneBooster_100_sero)[3]),
                         annualBooster_mean = c(colSums(annualBooster_original)[1],
                                                colSums(annualBooster_high_inc)[1],
                                                colSums(annualBooster_low_inc)[1],
                                                colSums(annualBooster_opt_ve)[1],
                                                colSums(annualBooster_opt_wan)[1],
                                                colSums(annualBooster_pes_ve)[1],
                                                colSums(annualBooster_pes_wan)[1],
                                                colSums(annualBooster_100_sero)[1]),
                         annualBooster_max = c(colSums(annualBooster_original)[2],
                                               colSums(annualBooster_high_inc)[2],
                                               colSums(annualBooster_low_inc)[2],
                                               colSums(annualBooster_opt_ve)[2],
                                               colSums(annualBooster_opt_wan)[2],
                                               colSums(annualBooster_pes_ve)[2],
                                               colSums(annualBooster_pes_wan)[2],
                                               colSums(annualBooster_100_sero)[2]),
                         annualBooster_min = c(colSums(annualBooster_original)[3],
                                               colSums(annualBooster_high_inc)[3],
                                               colSums(annualBooster_low_inc)[3],
                                               colSums(annualBooster_opt_ve)[3],
                                               colSums(annualBooster_opt_wan)[3],
                                               colSums(annualBooster_pes_ve)[3],
                                               colSums(annualBooster_pes_wan)[3],
                                               colSums(annualBooster_100_sero)[3]),
                         biannualBooster_mean = c(colSums(biannualBooster_original)[1],
                                                  colSums(biannualBooster_high_inc)[1],
                                                  colSums(biannualBooster_low_inc)[1],
                                                  colSums(biannualBooster_opt_ve)[1],
                                                  colSums(biannualBooster_opt_wan)[1],
                                                  colSums(biannualBooster_pes_ve)[1],
                                                  colSums(biannualBooster_pes_wan)[1],
                                                  colSums(biannualBooster_100_sero)[1]),
                         biannualBooster_max = c(colSums(biannualBooster_original)[2],
                                                 colSums(biannualBooster_high_inc)[2],
                                                 colSums(biannualBooster_low_inc)[2],
                                                 colSums(biannualBooster_opt_ve)[2],
                                                 colSums(biannualBooster_opt_wan)[2],
                                                 colSums(biannualBooster_pes_ve)[2],
                                                 colSums(biannualBooster_pes_wan)[2],
                                                 colSums(biannualBooster_100_sero)[2]),
                         biannualBooster_min = c(colSums(biannualBooster_original)[3],
                                                 colSums(biannualBooster_high_inc)[3],
                                                 colSums(biannualBooster_low_inc)[3],
                                                 colSums(biannualBooster_opt_ve)[3],
                                                 colSums(biannualBooster_opt_wan)[3],
                                                 colSums(biannualBooster_pes_ve)[3],
                                                 colSums(biannualBooster_pes_wan)[3],
                                                 colSums(biannualBooster_100_sero)[3])) %>%
  mutate_at(vars(-group_cols()), list(~(./(2*1000000))*100000)) %>%
  mutate(sensitivity_group = c("Main", "High Incidence" ,"Low Incidence" ,"Optimistic VE", "Optimistic Waning", "Pessimistic VE", "Pessimistic Waning", "100% Seroprevalence"))

plot_data <- (melt(sensitivity, id= "sensitivity_group") %>%
                mutate(sensitivity_group = factor(sensitivity_group, levels = c("Main", "High Incidence" ,"Low Incidence" ,"Optimistic VE", "Optimistic Waning", "Pessimistic VE", "Pessimistic Waning", "100% Seroprevalence")),
                       estimate = case_when(variable %in% c("oneBooster_mean", "annualBooster_mean", "biannualBooster_mean") ~ "mean",
                                            variable %in% c("oneBooster_min", "annualBooster_min", "biannualBooster_min") ~ "min",
                                            TRUE ~ "max"),
                       vax_intervention = case_when(variable %in% c("oneBooster_mean", "oneBooster_min", "oneBooster_max") ~ "1 Booster",
                                                    variable %in% c("annualBooster_mean", "annualBooster_min", "annualBooster_max") ~ "Annual Booster",
                                                    TRUE ~ "Biannual Booster")) %>% dplyr::select(-variable)) 

hello <- pivot_wider(data = plot_data, 
                     id_cols = c("sensitivity_group", "vax_intervention"), 
                     names_from = estimate, 
                     values_from = value)

ggplot(hello, aes(x = vax_intervention, y = mean, color = vax_intervention)) + 
  facet_grid(. ~ sensitivity_group, switch = "both") + 
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.1) +
  ylab("Annual Risk of Severe COVID-19 \n (cases per 100,000)") +
  xlab("Sensitivity Analysis")+
  labs(color = "Intervention")+
  scale_color_discrete(labels = c("1 Booster", "Annual Booster", "Semiannual Booster")) +
  theme(
    text = element_text(size=10),
    axis.text.x=element_blank()) +
  #ggtitle("Scenario 1 \nWaning Version 2")+
  ylim(0, 0.030*100000)

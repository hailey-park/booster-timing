###################################################################################################
#Title: Dynamic Model Analysis Figures
#Author: Hailey Park
#Date: November 22, 2023
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

###################################################################################################
#Plotting dynamic analyses
oneBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
oneBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
oneBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
oneBooster_65_strat_mean <- oneBooster_18_strat_mean
oneBooster_75_strat_mean <- oneBooster_18_strat_mean
oneBooster_65_strat_min <- oneBooster_18_strat_min
oneBooster_75_strat_min <- oneBooster_18_strat_min
oneBooster_65_strat_max <- oneBooster_18_strat_max
oneBooster_75_strat_max <- oneBooster_18_strat_max

annualBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_65_strat_mean <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_65_strat_min <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_65_strat_max <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_75_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_75_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
annualBooster_75_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 

biannualBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_65_strat_mean <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_65_strat_min <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_65_strat_max <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_75_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_75_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 
biannualBooster_75_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(age_group == "75+ years", immunocompromised == 0) 

sensitivity<- data.frame(oneBooster_mean = c(sum(oneBooster_18_strat_mean[, (4:107)]),
                                             sum(oneBooster_65_strat_mean[, (4:107)]),
                                             sum(oneBooster_75_strat_mean[, (4:107)])),
                         oneBooster_min = c(sum(oneBooster_18_strat_min[, (1:104)]),
                                            sum(oneBooster_65_strat_min[, (1:104)]),
                                            sum(oneBooster_75_strat_min[, (1:104)])),
                         oneBooster_max = c(sum(oneBooster_18_strat_max[, (1:104)]),
                                            sum(oneBooster_65_strat_max[, (1:104)]),
                                            sum(oneBooster_75_strat_max[, (1:104)])),
                         annualBooster_mean = c(sum(annualBooster_18_strat_mean[, (4:107)]),
                                                sum(annualBooster_65_strat_mean[, (4:107)]),
                                                sum(annualBooster_75_strat_mean[, (4:107)])),
                         annualBooster_min = c(sum(annualBooster_18_strat_min[, (1:104)]),
                                               sum(annualBooster_65_strat_min[, (1:104)]),
                                               sum(annualBooster_75_strat_min[, (1:104)])),
                         annualBooster_max = c(sum(annualBooster_18_strat_max[, (1:104)]),
                                               sum(annualBooster_65_strat_max[, (1:104)]),
                                               sum(annualBooster_75_strat_max[, (1:104)])),
                         biannualBooster_mean = c(sum(biannualBooster_18_strat_mean[, (4:107)]),
                                                  sum(biannualBooster_65_strat_mean[, (4:107)]),
                                                  sum(biannualBooster_75_strat_mean[, (4:107)])),
                         biannualBooster_min = c(sum(biannualBooster_18_strat_min[, (1:104)]),
                                                 sum(biannualBooster_65_strat_min[, (1:104)]),
                                                 sum(biannualBooster_75_strat_min[, (1:104)])),
                         biannualBooster_max = c(sum(biannualBooster_18_strat_max[, (1:104)]),
                                                 sum(biannualBooster_65_strat_max[, (1:104)]),
                                                 sum(biannualBooster_75_strat_max[, (1:104)]))) %>%
  mutate_at(vars(-group_cols()), list(~(./(2*499082))*100000)) %>%
  mutate(pop_strat_group = c("18+ Strategy" ,"65+ Strategy", "75+ Strategy" ))

plot_data <- (melt(sensitivity, id= "pop_strat_group") %>%
                mutate(pop_strat_group = factor(pop_strat_group, levels = c("18+ Strategy" ,"65+ Strategy", "75+ Strategy")),
                       estimate = case_when(variable %in% c("oneBooster_mean", "annualBooster_mean", "biannualBooster_mean") ~ "mean",
                                            variable %in% c("oneBooster_min", "annualBooster_min", "biannualBooster_min") ~ "min",
                                            TRUE ~ "max"),
                       vax_intervention = case_when(variable %in% c("oneBooster_mean", "oneBooster_min", "oneBooster_max") ~ "1 Booster",
                                                    variable %in% c("annualBooster_mean", "annualBooster_min", "annualBooster_max") ~ "Annual Booster",
                                                    TRUE ~ "Biannual Booster")) %>% dplyr::select(-variable)) 

hello <- pivot_wider(data = plot_data, 
                     id_cols = c("pop_strat_group", "vax_intervention"), 
                     names_from = estimate, 
                     values_from = value) #%>%
#filter(pop_strat_group == "18+ Strategy")

ggplot(hello, aes(x = vax_intervention, y = mean, color = vax_intervention)) + 
  facet_grid(. ~ pop_strat_group, switch = "both") + 
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.1) +
  ylab("Annual Risk of Severe COVID-19 \n (cases per 100,000)") +
  xlab("Population Strategy")+
  labs(color = "Intervention")+
  scale_color_discrete(labels = c("1 Booster", "Annual Booster", "Semiannual Booster")) +
  theme(
    #panel.grid.major = element_line(color = "black"),
    #panel.grid.minor = element_line(color = "black"),
    #panel.background = element_blank(), 
    #axis.line = element_line(colour = "black"),
    #panel.background = element_blank(), 
    text = element_text(size=10),
    axis.text.x=element_blank())+
  #ggtitle("Scenario 1 \nWaning Version 2")+
  ylim(0, 0.015*100000)
#ylim(0, 0.005*100000)
###################################################################################################
#Plotting dynamic analyses (immunocompromised)
oneBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
oneBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
oneBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/1-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
oneBooster_65_strat_mean <- oneBooster_18_strat_mean
oneBooster_75_strat_mean <- oneBooster_18_strat_mean
oneBooster_65_strat_min <- oneBooster_18_strat_min
oneBooster_75_strat_min <- oneBooster_18_strat_min
oneBooster_65_strat_max <- oneBooster_18_strat_max
oneBooster_75_strat_max <- oneBooster_18_strat_max

annualBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_65_strat_mean <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_65_strat_min <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_65_strat_max <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_75_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_75_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
annualBooster_75_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Annual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 

biannualBooster_18_strat_mean <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_18_strat_min <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_18_strat_max <- read.csv("results/simulation-results/dynamic/18+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_65_strat_mean <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_65_strat_min <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_65_strat_max <- read.csv("results/simulation-results/dynamic/65+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_75_strat_mean <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/mean/combined.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_75_strat_min <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/min.csv")[,-1] %>% 
  filter(immunocompromised == 1) 
biannualBooster_75_strat_max <- read.csv("results/simulation-results/dynamic/75+ strategy/optimistic/Biannual-Booster/max.csv")[,-1] %>% 
  filter(immunocompromised == 1) 

sum(oneBooster_18_strat_mean$total_pop)
sensitivity<- data.frame(oneBooster_mean = c(sum(colSums(oneBooster_18_strat_mean[, (4:107)])),
                                             sum(colSums(oneBooster_65_strat_mean[, (4:107)])),
                                             sum(colSums(oneBooster_75_strat_mean[, (4:107)]))),
                         oneBooster_min = c(sum(colSums(oneBooster_18_strat_min[, (1:104)])),
                                            sum(colSums(oneBooster_65_strat_min[, (1:104)])),
                                            sum(colSums(oneBooster_75_strat_min[, (1:104)]))),
                         oneBooster_max = c(sum(colSums(oneBooster_18_strat_max[, (1:104)])),
                                            sum(colSums(oneBooster_65_strat_max[, (1:104)])),
                                            sum(colSums(oneBooster_75_strat_max[, (1:104)]))),
                         annualBooster_mean = c(sum(colSums(annualBooster_18_strat_mean[, (4:107)])),
                                                sum(colSums(annualBooster_65_strat_mean[, (4:107)])),
                                                sum(colSums(annualBooster_75_strat_mean[, (4:107)]))),
                         annualBooster_min = c(sum(colSums(annualBooster_18_strat_min[, (1:104)])),
                                               sum(colSums(annualBooster_65_strat_min[, (1:104)])),
                                               sum(colSums(annualBooster_75_strat_min[, (1:104)]))),
                         annualBooster_max = c(sum(colSums(annualBooster_18_strat_max[, (1:104)])),
                                               sum(colSums(annualBooster_65_strat_max[, (1:104)])),
                                               sum(colSums(annualBooster_75_strat_max[, (1:104)]))),
                         biannualBooster_mean = c(sum(colSums(biannualBooster_18_strat_mean[, (4:107)])),
                                                  sum(colSums(biannualBooster_65_strat_mean[, (4:107)])),
                                                  sum(colSums(biannualBooster_75_strat_mean[, (4:107)]))),
                         biannualBooster_min = c(sum(colSums(biannualBooster_18_strat_min[, (1:104)])),
                                                 sum(colSums(biannualBooster_65_strat_min[, (1:104)])),
                                                 sum(colSums(biannualBooster_75_strat_min[, (1:104)]))),
                         biannualBooster_max = c(sum(colSums(biannualBooster_18_strat_max[, (1:104)])),
                                                 sum(colSums(biannualBooster_65_strat_max[, (1:104)])),
                                                 sum(colSums(biannualBooster_75_strat_max[, (1:104)])))) %>%
  mutate_at(vars(-group_cols()), list(~(./(2*331517))*100000)) %>%
  mutate(pop_strat_group = c("18+ Strategy" ,"65+ Strategy", "75+ Strategy" ))

plot_data <- (melt(sensitivity, id= "pop_strat_group") %>%
                mutate(pop_strat_group = factor(pop_strat_group, levels = c("18+ Strategy" ,"65+ Strategy", "75+ Strategy")),
                       estimate = case_when(variable %in% c("oneBooster_mean", "annualBooster_mean", "biannualBooster_mean") ~ "mean",
                                            variable %in% c("oneBooster_min", "annualBooster_min", "biannualBooster_min") ~ "min",
                                            TRUE ~ "max"),
                       vax_intervention = case_when(variable %in% c("oneBooster_mean", "oneBooster_min", "oneBooster_max") ~ "1 Booster",
                                                    variable %in% c("annualBooster_mean", "annualBooster_min", "annualBooster_max") ~ "Annual Booster",
                                                    TRUE ~ "Biannual Booster")) %>% dplyr::select(-variable)) 

hello <- pivot_wider(data = plot_data, 
                     id_cols = c("pop_strat_group", "vax_intervention"), 
                     names_from = estimate, 
                     values_from = value) #%>%
#filter(pop_strat_group == "18+ Strategy")

ggplot(hello, aes(x = vax_intervention, y = mean, color = vax_intervention)) + 
  facet_grid(. ~ pop_strat_group, switch = "both") + 
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.1) +
  ylab("Annual Risk of Severe COVID-19 \n (cases per 100,000)") +
  xlab("Population Strategy")+
  labs(color = "Intervention")+
  scale_color_discrete(labels = c("1 Booster", "Annual Booster", "Semiannual Booster")) +
  theme(
    #panel.grid.major = element_line(color = "black"),
    #panel.grid.minor = element_line(color = "black"),
    #panel.background = element_blank(), 
    #axis.line = element_line(colour = "black"),
    #panel.background = element_blank(), 
    text = element_text(size=10),
    axis.text.x=element_blank())+
  #ggtitle("Scenario 1 \nWaning Version 2")+
  ylim(0, 0.015*100000)
#ylim(0, 0.005*100000)
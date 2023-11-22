###################################################################################################
#Title: Variant Scenario Analysis Figures
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
#Plotting explanation of variant analyses (Figure 2, Panel A)
variant_analyses <- data.frame(months = c(-6:24),
                               Original = rep("old", 31),
                               S1 = c(rep("old", 6), rep("variant1", 25)),
                               S3 = c(rep("old", 18), rep("variant1", 13)),
                               S4 = c(rep("old",6), rep("variant1", 12), rep("variant2", 13)),
                               S4 = c(rep("old",6), rep("variant1", 12), rep("variant2", 13)))

plot_data <- melt(variant_analyses, id = "months") %>%
  mutate(updated_vax = if_else(variable == "S4" & value %in% c("variant1", "variant2"),
                               "yes", "no"),
         variable = factor(variable, levels = c("S4", "S3", "S2" ,"S1","Original")))

ggplot(plot_data, aes(months)) + 
  geom_line(aes(y=variable, color = value), key_glyph = "rect", size = 1.5) + 
  geom_point(data = plot_data %>% filter(variable == "S4",
                                         updated_vax == "yes",
                                         months %in% c(1, 13)), aes(y = variable, shape = updated_vax, color = "Updated Vaccine"), size = 5) +
  geom_vline(aes(xintercept = -0.5), color = "darkblue", key_glyph = "rect", size = 1, linetype = "dashed") +
  xlab("Time (months)") + 
  ylab("Variant Scenario") +
  labs(color = "Legend")+
  scale_color_manual(values = c("tomato",
                                "orange",
                                "dodgerblue",
                                "forestgreen"),
                     labels = c("Current Variant", "Updated Vaccine","New Variant 1", "New Variant 2"))+
  scale_x_continuous(breaks=c(-4.5, -0.5,3.8,8,12,16,20,24),
                     labels = c(-4, 0,4,8,12,16,20,24)) +
  guides(shape = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12)) 
###################################################################################################
#Plotting variant analyses
oneBooster_original <- read.csv("results/simulation-results/main/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_S1 <- read.csv("results/simulation-results/variantAnalysis/S1/ver1/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_S2 <- read.csv("results/simulation-results/variantAnalysis/S2/ver1/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_S3 <- read.csv("results/simulation-results/variantAnalysis/S3/ver1/immuno-mild/1Booster-summarised.csv")[,-1]
oneBooster_S4 <- read.csv("results/simulation-results/variantAnalysis/S4/ver1/immuno-mild/1Booster-summarised.csv")[,-1]

annualBooster_original <- read.csv("results/simulation-results/main/immuno-mild/annualBooster-18-49 years-summarised.csv")[,-1]
annualBooster_S1 <- read.csv("results/simulation-results/variantAnalysis/S1/ver1/annualBooster-18-49 years-summarised.csv")[,-1]
annualBooster_S2 <- read.csv("results/simulation-results/variantAnalysis/S2/ver1/annualBooster-18-49 years-summarised.csv")[,-1]
annualBooster_S3 <- read.csv("results/simulation-results/variantAnalysis/S3/ver1/annualBooster-18-49 years-summarised.csv")[,-1]
annualBooster_S4 <- read.csv("results/simulation-results/variantAnalysis/S4/ver1/annualBooster-18-49 years-summarised.csv")[,-1]

biannualBooster_original <- read.csv("simulation-results/final-results/95ui/immuno-mild/biannualBooster-18-49 years-summarised.csv")[,-1]
biannualBooster_S1 <- read.csv("results/simulation-results/variantAnalysis/S1/ver1/biannualBooster-18-49 years-summarised.csv")[,-1]
biannualBooster_S2 <- read.csv("results/simulation-results/variantAnalysis/S2/ver1/biannualBooster-18-49 years-summarised.csv")[,-1]
biannualBooster_S3 <- read.csv("results/simulation-results/variantAnalysis/S3/ver1/biannualBooster-18-49 years-summarised.csv")[,-1]
biannualBooster_S4 <- read.csv("results/simulation-results/variantAnalysis/S4/ver1/biannualBooster-18-49 years-summarised.csv")[,-1]

variant_sensitivity<- data.frame(oneBooster_mean = c(colSums(oneBooster_original)[1],
                                                     colSums(oneBooster_S1)[1],
                                                     colSums(oneBooster_S2)[1],
                                                     colSums(oneBooster_S3)[1],
                                                     colSums(oneBooster_S4)[1]),
                                 oneBooster_max = c(colSums(oneBooster_original)[2],
                                                    colSums(oneBooster_S1)[2],
                                                    colSums(oneBooster_S2)[2],
                                                    colSums(oneBooster_S3)[2],
                                                    colSums(oneBooster_S4)[2]),
                                 oneBooster_min = c(colSums(oneBooster_original)[3],
                                                    colSums(oneBooster_S1)[3],
                                                    colSums(oneBooster_S2)[3],
                                                    colSums(oneBooster_S3)[3],
                                                    colSums(oneBooster_S4)[3]),
                                 annualBooster_mean = c(colSums(annualBooster_original)[1],
                                                        colSums(annualBooster_S1)[1],
                                                        colSums(annualBooster_S2)[1],
                                                        colSums(annualBooster_S3)[1],
                                                        colSums(annualBooster_S4)[1]),
                                 annualBooster_max = c(colSums(annualBooster_original)[2],
                                                       colSums(annualBooster_S1)[2],
                                                       colSums(annualBooster_S2)[2],
                                                       colSums(annualBooster_S3)[2],
                                                       colSums(annualBooster_S4)[2]),
                                 annualBooster_min = c(colSums(annualBooster_original)[3],
                                                       colSums(annualBooster_S1)[3],
                                                       colSums(annualBooster_S2)[3],
                                                       colSums(annualBooster_S3)[3],
                                                       colSums(annualBooster_S4)[3]),
                                 biannualBooster_mean = c(colSums(biannualBooster_original)[1],
                                                          colSums(biannualBooster_S1)[1],
                                                          colSums(biannualBooster_S2)[1],
                                                          colSums(biannualBooster_S3)[1],
                                                          colSums(biannualBooster_S4)[1]),
                                 biannualBooster_max = c(colSums(biannualBooster_original)[2],
                                                         colSums(biannualBooster_S1)[2],
                                                         colSums(biannualBooster_S2)[2],
                                                         colSums(biannualBooster_S3)[2],
                                                         colSums(biannualBooster_S4)[2]),
                                 biannualBooster_min = c(colSums(biannualBooster_original)[3],
                                                         colSums(biannualBooster_S1)[3],
                                                         colSums(biannualBooster_S2)[3],
                                                         colSums(biannualBooster_S3)[3],
                                                         colSums(biannualBooster_S4)[3])) %>%
  mutate_at(vars(-group_cols()), list(~(./(2*1000000))*100000)) %>%
  mutate(variant_group = c("Main", "S1" ,"S2" ,"S3", "S4"))


plot_data <- (melt(variant_sensitivity, id= "variant_group") %>%
                mutate(variant_group = factor(variant_group, levels = c("Main", "S1" ,"S2" ,"S3", "S4")),
                       estimate = case_when(variable %in% c("oneBooster_mean", "annualBooster_mean", "biannualBooster_mean") ~ "mean",
                                            variable %in% c("oneBooster_min", "annualBooster_min", "biannualBooster_min") ~ "min",
                                            TRUE ~ "max"),
                       vax_intervention = case_when(variable %in% c("oneBooster_mean", "oneBooster_min", "oneBooster_max") ~ "1 Booster",
                                                    variable %in% c("annualBooster_mean", "annualBooster_min", "annualBooster_max") ~ "Annual Booster",
                                                    TRUE ~ "Biannual Booster")) %>% dplyr::select(-variable)) 

hello <- pivot_wider(data = plot_data, 
                     id_cols = c("variant_group", "vax_intervention"), 
                     names_from = estimate, 
                     values_from = value)

ggplot(hello, aes(x = vax_intervention, y = mean, color = vax_intervention)) + 
  facet_grid(. ~ variant_group, switch = "both") + 
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.1) +
  ylab("Annual Risk of Severe COVID-19 \n (cases per 100,000)") +
  xlab("Variant Scenario")+
  labs(color = "Intervention")+
  scale_color_discrete(labels = c("1 Booster", "Annual Booster", "Semiannual Booster")) +
  theme(
    #panel.grid.major = element_line(color = "black"),
    #panel.grid.minor = element_line(color = "black"),
    #panel.background = element_blank(), 
    #axis.line = element_line(colour = "black"),
    #panel.background = element_blank(), 
    text = element_text(size=10),
    axis.text.x=element_blank()) +
  #ggtitle("Scenario 1 \nWaning Version 2")+
  ylim(0, 0.035*100000)
#ylim(0, 0.04*100000)

###################################################################################################
#Title: Plots for Waning Predictions
#Author: Hailey Park
#Date: August 27, 2023
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


#Load in dataset (can switch out with immunoMild and immunoSevere waning curves here)
severe_waning_data <- read.csv("results/waning-predictions/main/severe_waning_predictions_monthly.csv")[,-1]
nonsevere_waning_data <- read.csv("results/waning-predictions/main/nonsevere_waning_predictions_monthly.csv")[,-1]

severe_waning_data_mean <- severe_waning_data %>% filter(estimate == "mean")
severe_waning_data_upper <- severe_waning_data %>% filter(estimate == "upper")
severe_waning_data_lower <- severe_waning_data %>% filter(estimate == "lower")

nonsevere_waning_data_mean <- nonsevere_waning_data %>% filter(estimate == "mean")
nonsevere_waning_data_upper <- nonsevere_waning_data %>% filter(estimate == "upper")
nonsevere_waning_data_lower <- nonsevere_waning_data %>% filter(estimate == "lower")

###################################################################################################
#Plots with 95% UI by risk group

#combine datasets
combined_severe <- rbind(severe_waning_data_mean %>% mutate(estimate = 'Mean'),
                  severe_waning_data_upper %>% mutate(estimate = 'Upper 95% UI'),
                  severe_waning_data_lower %>% mutate(estimate = 'Lower 95% UI')) %>%
  mutate(group = if_else(prior_inf == 1, paste0("Prior Infection; ", estimate),
                         paste0("No Prior Infection; ", estimate)))
combined_nonsevere <- rbind(nonsevere_waning_data_mean %>% mutate(estimate = 'Mean'),
                         nonsevere_waning_data_upper %>% mutate(estimate = 'Upper 95% UI'),
                         nonsevere_waning_data_lower %>% mutate(estimate = 'Lower 95% UI')) %>%
  mutate(group = if_else(prior_inf == 1, paste0("Prior Infection; ", estimate),
                         paste0("No Prior Infection; ", estimate)))


#Loop through each age group and create plots
plot_list = list()
age_groups <- c("18-49 years", "50-64 years", "65-74 years", "75+ years")

for (i in 1:4) {
  age <- age_groups[i]
  age_data_severe <- combined_severe %>% filter(age_group == age) 
  age_data_nonsevere <- combined_nonsevere %>% filter(age_group == age) 
  
  
  p <- ggplot(age_data_severe, aes(x = months, y = ve_pred * 100, color = group)) +
    geom_line(linewidth = .75) +
    ggtitle(paste0("Protective Effectiveness Waning against Severe COVID-19\nAge: ", age, "\nImmune Status: Immunocompetent")) +
    ylab("Protective Effectiveness (%)")+
    xlab("Time (months)") +
    labs(color='Group') +
    ylim(0,100) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size=12))+
    scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
    scale_color_manual(values = c(
      "lightskyblue1",
      "dodgerblue1",
      "royalblue3",
      "tomato",
      "red",
      "red4"))
  
  plot_list[[i]] <- p
  

  p_nonsevere <- ggplot(age_data_nonsevere, aes(x = months, y = ve_pred * 100, color = group)) +
    geom_line(linewidth = .75) +
    ggtitle(paste0("Protective Effectiveness Waning against Nonsevere COVID-19\nAge: ", age, "\nImmune Status: Immunocompetent")) +
    ylab("Protective Effectiveness (%)")+
    xlab("Time (months)") +
    labs(color='Group') +
    ylim(0,100) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size=12))+
    scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
    scale_color_manual(values = c(
      "lightskyblue1",
      "dodgerblue1",
      "royalblue3",
      "tomato",
      "red",
      "red4"))
  
  plot_list[[i+4]] <- p_nonsevere
}

pdf("figures/appendix/waning-plots-immunocompetent.pdf")
for (i in 1:8) {
  print(plot_list[[i]])
}
dev.off()


  
  
  
  

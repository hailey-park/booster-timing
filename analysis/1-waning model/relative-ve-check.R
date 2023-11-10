###################################################################################################
#Title: Relative VE Comparison
#Author: Hailey Park
#Date: November 1, 2023
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

#Relative VE Function
relative_ve_fn <- function(baseline, upper) {
  baseline_rr <- 1 - baseline
  upper_rr <- 1- upper
  relative_rr <- upper_rr / baseline_rr
  relative_ve <- 1 - relative_rr
  return(relative_ve)
}

#Read in waning prediction data (absolute VE)
waning_data <- read.csv("results/waning-predictions/main/severe_waning_predictions_monthly.csv")[,-1]

#Read in waning relative VE literature estimates
#NOTE: These estimates are from Lin et al. (New England Journal of Medicine, 2023)
#      DOI: 10.1056/NEJMc2215471; Figure S2 Panel B
#      Link: https://www.nejm.org/doi/full/10.1056/NEJMc2215471
relative_ve_data_lit <- data.frame(months = rep(c(0.5, 1:5), 2),
                                   relative_ve = c(.6471, .4747, .4495, .4242, .3951, .3698, .771, .4241, .3872,.3424,.3016 ,.2529),
                                   prior_inf = rep(0:1, each = 6)) %>%
  mutate(group = if_else(prior_inf == 1, "Literature Estimates; Prior Infection", "Literature Estimates; No Prior Infection")) %>%
  dplyr::select(-prior_inf)


#We are checking if the relative VE under our waning curve predictions are similar to published estimates.
#We are checking the relative VE given that the average time since last immune event (dose; infection) is around 7-8 months.

waning_data <- waning_data %>%
  mutate(group = if_else(prior_inf == 1, "Prior Infection; Mean", "No Prior Infection; Mean"))

adj_relative_waning <- waning_data %>% filter(age_group == "50-64 years", estimate == 'mean') %>%
  arrange(months) %>%
  mutate(months_baseline = lead(months, n = 16, default = 24)) %>% 
  dplyr::select(group, months, months_baseline, ve_pred)

waning_data <- waning_data %>% filter(age_group == "50-64 years", estimate == 'mean') %>%
  arrange(months) %>%
  rename(ve_pred_baseline = ve_pred) %>% dplyr::select(group, months, ve_pred_baseline)

adj_waning_clean <- merge(adj_relative_waning, waning_data, by.x = c("months_baseline", "group"),
                          by.y = c("months", "group"), all.x = TRUE) %>% 
  mutate(months = if_else(months < 1, 0.5, round(months)),
         relative_ve = relative_ve_fn(ve_pred_baseline, ve_pred),
         group = if_else(group == "Prior Infection; Mean", "Waning Predictions; Prior Infection", "Waning Predictions; No Prior Infection")) %>%
  dplyr::select(months, relative_ve, group) 

combined_rel <- rbind(adj_waning_clean, relative_ve_data_lit)

ggplot(combined_rel, aes(months, relative_ve*100, color = factor(group))) +
  geom_line(size = .75) +
  ylab("Relative Protective Effectiveness (%)") +
  xlab("Time (months)") +
  labs(color='Group') +
  ylim(0,100) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12))+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
  ggtitle("Relative VE Comparison")


###################################################################################################

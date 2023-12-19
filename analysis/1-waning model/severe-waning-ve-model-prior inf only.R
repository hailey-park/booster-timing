###################################################################################################
#Title: Severe Waning VE Model
#Author: Hailey Park
#Date: March 25, 2023
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
library(lme4)
library(merTools)
library(sjstats)

#Load in raw datasets
waning_data <- read.csv("data/raw-data/severe waning data absolute prior inf only 121123 data.csv") 
weights <- read.csv("data/raw-data/severe waning data absolute prior inf only 121123 weights.csv") 

#Reformat data to long and clean data
long_data_mean <- melt(waning_data %>% filter(X95UI == "mean")) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100, 
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         study = as.factor(References),
         estimate = "mean") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months)) 

long_data_lower <- melt(waning_data %>% filter(X95UI == "lower")) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100,
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         study = as.factor(References),
         estimate = "lower") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months))

long_data_upper <- melt(waning_data %>% filter(X95UI == "upper")) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100, 
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         study = as.factor(References),
         estimate = "upper") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months))

long_data_weights <- melt(weights) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         study = as.factor(References),
         weight = value) %>%
  dplyr::select(c(months, prior_inf, num_doses, study, weight))


#Merge weights to raw waning data
merged_data <- merged_data <- merge(rbind(long_data_mean, long_data_lower, long_data_upper),
                                    long_data_weights, by = c("months", "prior_inf", "num_doses", "study"), all.x = TRUE) %>%
  mutate(estimate = factor(estimate, levels = c("lower", "mean", "upper")))

  
#Fit model
severe_model <- lmer(ve_input ~ month_input  +  estimate + (month_input|study),
                      weights = weight,
                      data = merged_data)

summary(severe_model)


#Prediction for waning model (weekly timescale; for dynamic model)
new_data_weekly <- data.frame(estimate = rep(c("lower", "mean", "upper"), each = 1, times = 1),
                       study = "NA")


prediction_data_weekly <- new_data_weekly[rep(seq_len(nrow(new_data_weekly)), 104), ]
prediction_data_weekly$month_input <- rep(log(c(1:104)/4.345), each = 3)
prediction_data_weekly$months <- rep(floor(c(1:104)/4.345), each = 3)
prediction_data_weekly$weeks <- rep((c(1:104)), each = 3)

preds_weekly <- predict(severe_model, newdata = prediction_data_weekly, allow.new.levels = TRUE)
prediction_data_weekly$ve_pred <- 1 - exp(preds_weekly)

#Adjust waning data by age group
hybrid_immunity_waning_data <- read.csv("results/waning-predictions/dynamic/combined_severe_waning_predictions_weekly.csv")[,-1] %>%
  filter(immunocompromised == 0, estimate == "mean", prior_inf == 1) %>%
  dplyr::select(age_group, weeks, ve_pred)

ve_diff_by_age <- merge(hybrid_immunity_waning_data, hybrid_immunity_waning_data %>% filter(age_group == "50-64 years"), 
                     by = "weeks", all.x = TRUE) %>%
  mutate(ve_diff = ve_pred.x - ve_pred.y) %>%
  group_by(age_group.x) %>% summarise(ve_diff_avg = mean(ve_diff))

immunocompetent_combined <- rbind(prediction_data_weekly %>% rowwise() %>% mutate(ve_pred = max(ve_pred - 0.002212477, 0), age_group = "0-17 years"),
                                  prediction_data_weekly %>% rowwise() %>% mutate(ve_pred = max(ve_pred - 0.002212477, 0), age_group = "18-49 years"),
                                  prediction_data_weekly %>% rowwise() %>% mutate(age_group = "50-64 years"),
                                  prediction_data_weekly %>% rowwise() %>% mutate(ve_pred = max(ve_pred + 0.004720923, 0), age_group = "65-74 years"),
                                  prediction_data_weekly %>% rowwise() %>% mutate(ve_pred = max(ve_pred + 0.004720923, 0), age_group = "75+ years")) %>%
  mutate(immunocompromised = 0)


#Combine immunocompetent waning data with all immunocompromised groups (Mild; Moderate/Severe)
#NOTE: Immunocompromised (Mild) has waning shifted down 13%
#      Immunocompromised (Moderate/Severe) has waning shifted down 13% and waning rate increased by 12%
combined <- rbind(immunocompetent_combined,
                  immunocompetent_combined %>% rowwise() %>% mutate(ve_pred = max(ve_pred - 0.13, 0),
                                                                    immunocompromised = 1),
                  immunocompetent_combined %>% rowwise() %>% mutate(ve_pred = max((ve_pred - weeks * 0.00115) - 0.13, 0),
                                                                    immunocompromised = 2))

write.csv(combined, "results/waning-predictions/dynamic/combined_severe_waning_predictions_weekly_prior_inf_only.csv")


plot_data <- as.data.frame(combined)

plot_data %>% 
  filter(estimate == "mean", age_group == "50-64 years") %>%
  #mutate(prior_inf=as.factor(prior_inf)) %>% 
  ggplot(aes(weeks, ve_pred, group = immunocompromised, color=immunocompromised)) + 
  geom_line() + geom_point() +
  ylim(0,1) +
  ggtitle("Fitted Prior Infection Only Waning Immunity Curves\nOutcome: Severe Infections") +
  geom_point(data = long_data_mean %>% mutate(estimate = study), aes(x = months, y = ve, group = estimate))


###################################################################################################

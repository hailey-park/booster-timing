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
waning_data <- read.csv("data/raw-data/severe waning data absolute 090423 final mean.csv") 
weights <- read.csv("data/raw-data/severe waning data absolute 090423 final weights.csv") 
waning_data_95ui <- read.csv("data/raw-data/severe waning data absolute 090423 final 95ui.csv")

#Reformat data to long and clean data
long_data_mean <- melt(waning_data) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100, 
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         age_group = Age, 
         study = as.factor(References),
         estimate = "mean") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, age_group, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months)) 

long_data_lower <- melt(waning_data_95ui %>% filter(X95UI == "lower")) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100,
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         age_group = Age, 
         study = as.factor(References),
         estimate = "lower") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, age_group, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months))

long_data_upper <- melt(waning_data_95ui %>% filter(X95UI == "upper")) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         ve =  value/100, 
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         age_group = Age, 
         study = as.factor(References),
         estimate = "upper") %>%
  dplyr::select(c(months, prior_inf, ve, num_doses, age_group, study, estimate)) %>%
  mutate(ve_input = log(1 - ve),  #log of 1-VE
         month_input = log(months))

long_data_weights <- melt(weights) %>% filter(!is.na(value)) %>%
  mutate(months = as.numeric(substr(variable,6,8)),  #months
         prior_inf = ifelse(Prior.Infection == "Yes", 1, 0),
         num_doses = factor(Vaccine.Status, levels = c("Unvaccinated", "2-dose", "3-dose"), ordered = TRUE),
         age_group = Age, 
         study = as.factor(References),
         weight = value) %>%
  dplyr::select(c(months, prior_inf, num_doses, age_group, study, weight))


#Merge weights to raw waning data
merged_data <- merged_data <- merge(rbind(long_data_mean, long_data_lower, long_data_upper),
                                    long_data_weights, by = c("months", "prior_inf", "num_doses", "age_group", "study"), all.x = TRUE) %>%
  mutate(estimate = factor(estimate, levels = c("lower", "mean", "upper")))

  
#Fit model
severe_model <- lmer(ve_input ~ month_input  + factor(age_group)  + prior_inf + estimate + (month_input|study),
                      weights = weight,
                      data = merged_data %>% filter(num_doses == '3-dose'))

summary(severe_model)


#Prediction for waning model (monthly timescale)
new_data_monthly <- data.frame(age_group = rep(c("18-49 years", "50-64 years", "65+ years"), each = 6),
                       estimate = rep(c("lower", "mean", "upper"), each = 2, times = 3),
                       prior_inf = rep(c(0,1), each = 1, times = 9),
                       study = "NA")
prediction_data_monthly <- new_data_monthly[rep(seq_len(nrow(new_data_monthly)), 25), ]
prediction_data_monthly$month_input <- rep(log(c(0.5, 1:24)), each = 18)
prediction_data_monthly$months <- rep((c(0.5, 1:24)), each = 18)

preds_monthly <- predict(severe_model, newdata = prediction_data_monthly, allow.new.levels = TRUE)
prediction_data_monthly$ve_pred <- 1 - exp(preds_monthly)

#Prediction for waning model (weekly timescale; for dynamic model)
# new_data_weekly <- data.frame(age_group = rep(c("18-49 years", "50-64 years", "65+ years"), each = 6),
#                        estimate = rep(c("lower", "mean", "upper"), each = 2, times = 3),
#                        prior_inf = rep(c(0,1), each = 1, times = 9),
#                        study = "NA")
# prediction_data_weekly <- new_data_weekly[rep(seq_len(nrow(new_data_weekly)), 104), ]
# prediction_data_weekly$month_input <- rep(log(c(1:104)/4.345), each = 18)
# prediction_data_weekly$months <- rep(floor(c(1:104)/4.345), each = 18)
# prediction_data_weekly$weeks <- rep((c(1:104)), each = 18)
# 
# preds_weekly <- predict(severe_model, newdata = prediction_data_weekly, allow.new.levels = TRUE)
# prediction_data_weekly$ve_pred <- 1 - exp(preds_weekly)

#Split the 65+ age group into a 65-74 and 75+ age group
below65 <- prediction_data_monthly %>% filter(age_group != "65+ years")
above65_74 <- prediction_data_monthly %>% filter(age_group == "65+ years") %>%
  mutate(age_group = "65-74 years")
above75 <- prediction_data_monthly %>% filter(age_group == "65+ years") %>%
  mutate(age_group = "75+ years")

prediction_data_immunocompetent <- rbind(below65, above65_74, above75)

#Create waning data for immunocompromised groups (Mild; Moderate/Severe)
#NOTE: Immunocompromised (Mild) has waning shifted down 13%
#      Immunocompromised (Moderate/Severe) has waning shifted down 13% and waning rate increased by 12%
prediction_data_immuno_mild <- prediction_data_immunocompetent %>%
  rowwise() %>%
  mutate(ve_pred = max(ve_pred - 0.13, 0))

prediction_data_immuno_severe <- prediction_data_immunocompetent %>%
  rowwise() %>%
  mutate(ve_pred = max((ve_pred - months * 0.005) - 0.13, 0))

#Write to csv
write.csv(prediction_data_immunocompetent, "results/waning-predictions/main/severe_waning_predictions_monthly.csv")
write.csv(prediction_data_immuno_mild, "results/waning-predictions/main/severe_waning_predictions_monthly_immunoMild.csv")
write.csv(prediction_data_immuno_severe, "results/waning-predictions/main/severe_waning_predictions_monthly_immunoSevere.csv")


#Plot curves
plot_data <- prediction_data_immunocompetent

plot_data %>% 
  filter(estimate == "mean") %>%
  mutate(prior_inf=as.factor(prior_inf)) %>% 
  ggplot(aes(months, ve_pred, group=interaction(age_group, prior_inf), color=age_group, shape=prior_inf)) + 
  geom_line() + geom_point() +
  ylim(0,1)


###################################################################################################
#Curves for sensitivity analyses

#OPTIMISTIC VACCINE EFFECTIVENESS
#Waning curves shifted up 10%
prediction_data_immunocompetent_opt_ve <- prediction_data_immunocompetent %>% rowwise() %>%
  mutate(ve_pred = min(1, ve_pred + 0.1))

prediction_data_immuno_mild_opt_ve <- prediction_data_immuno_mild %>% rowwise() %>%
  mutate(ve_pred = min(1, ve_pred + 0.1))

prediction_data_immuno_severe_opt_ve <- prediction_data_immuno_severe %>% rowwise() %>%
  mutate(ve_pred = min(1, ve_pred + 0.1))

write.csv(prediction_data_immunocompetent_opt_ve, "results/waning-predictions/optimistic-ve/severe_waning_predictions_monthly.csv")
write.csv(prediction_data_immuno_mild_opt_ve, "results/waning-predictions/optimistic-ve/severe_waning_predictions_monthly_immunoMild.csv")
write.csv(prediction_data_immuno_severe_opt_ve, "results/waning-predictions/optimistic-ve/severe_waning_predictions_monthly_immunoSevere.csv")

#PESSIMISTIC VACCINE EFFECTIVENESS
#Waning curves shifted down 10%
prediction_data_immunocompetent_pess_ve <- prediction_data_immunocompetent %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.1))

prediction_data_immuno_mild_pess_ve <- prediction_data_immuno_mild %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.1))

prediction_data_immuno_severe_pess_ve <- prediction_data_immuno_severe %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - 0.1))

write.csv(prediction_data_immunocompetent_pess_ve, "results/waning-predictions/pessimistic-ve/severe_waning_predictions_monthly.csv")
write.csv(prediction_data_immuno_mild_pess_ve, "results/waning-predictions/pessimistic-ve/severe_waning_predictions_monthly_immunoMild.csv")
write.csv(prediction_data_immuno_severe_pess_ve, "results/waning-predictions/pessimistic-ve/severe_waning_predictions_monthly_immunoSevere.csv")

#OPTIMISTIC WANING
#Rate of waning reduced by 10%
prediction_data_immunocompetent_opt_wan <- prediction_data_immunocompetent %>% 
  group_by(age_group, prior_inf, estimate) %>%
  mutate(maxByGroup = max(ve_pred),
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred + (months * 0.00416) > maxByGroup,
                           maxByGroup,
                           ve_pred + (months * 0.00416))) %>%
  group_by(age_group, prior_inf, estimate) %>%
  mutate(min_month = months[which(ve_pred == min(ve_pred))[1]],
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred > minByGroup & months > min_month, minByGroup, ve_pred)) %>%
  dplyr::select(-c("maxByGroup", "minByGroup", "min_month"))


prediction_data_immuno_mild_opt_wan <- prediction_data_immuno_mild %>% 
  group_by(age_group, prior_inf, estimate) %>%
  mutate(maxByGroup = max(ve_pred),
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred + (months * 0.00416) > maxByGroup,
                           maxByGroup,
                           ve_pred + (months * 0.00416))) %>%
  group_by(age_group, prior_inf, estimate) %>%
  mutate(min_month = months[which(ve_pred == min(ve_pred))[1]],
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred > minByGroup & months > min_month, minByGroup, ve_pred)) %>%
  dplyr::select(-c("maxByGroup", "minByGroup", "min_month"))

prediction_data_immuno_severe_opt_wan <- prediction_data_immuno_severe %>% 
  group_by(age_group, prior_inf, estimate) %>%
  mutate(maxByGroup = max(ve_pred),
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred + (months * 0.00416) > maxByGroup,
                           maxByGroup,
                           ve_pred + (months * 0.00416))) %>%
  group_by(age_group, prior_inf, estimate) %>%
  mutate(min_month = months[which(ve_pred == min(ve_pred))[1]],
         minByGroup = min(ve_pred)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ve_pred = if_else(ve_pred > minByGroup & months > min_month, minByGroup, ve_pred)) %>%
  dplyr::select(-c("maxByGroup", "minByGroup", "min_month"))


write.csv(prediction_data_immunocompetent_opt_wan, "results/waning-predictions/optimistic-waning/severe_waning_predictions_monthly.csv")
write.csv(prediction_data_immuno_mild_opt_wan, "results/waning-predictions/optimistic-waning/severe_waning_predictions_monthly_immunoMild.csv")
write.csv(prediction_data_immuno_severe_opt_wan, "results/waning-predictions/optimistic-waning/severe_waning_predictions_monthly_immunoSevere.csv")

#PESSIMISTIC WANING
#Rate of waning increased by 10%
prediction_data_immunocompetent_pess_wan <- prediction_data_immunocompetent %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - (months * 0.00416)))

prediction_data_immuno_mild_pess_wan <- prediction_data_immuno_mild %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - (months * 0.00416)))

prediction_data_immuno_severe_pess_wan <- prediction_data_immuno_severe %>% rowwise() %>%
  mutate(ve_pred = max(0, ve_pred - (months * 0.00416)))

write.csv(prediction_data_immunocompetent_pess_wan, "results/waning-predictions/pessimistic-waning/severe_waning_predictions_monthly.csv")
write.csv(prediction_data_immuno_mild_pess_wan, "results/waning-predictions/pessimistic-waning/severe_waning_predictions_monthly_immunoMild.csv")
write.csv(prediction_data_immuno_severe_pess_wan, "results/waning-predictions/pessimistic-waning/severe_waning_predictions_monthly_immunoSevere.csv")

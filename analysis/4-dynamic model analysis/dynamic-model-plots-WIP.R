###################################################################################################
#Title: Dynamic Model results processing and plots
#Author: Hailey Park
#Date: September 1, 2023
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
library(here)
library(data.table)

###################################################################################################

tp <- list.files(path, pattern = "*.csv", full.names = TRUE)
all_files <- lapply(tp, read.delim)

###################################################################################################
results <- read.csv("simulation-results/final-results/dynamic/65+ strategy/realistic/entire_pop_simulation_1Booster_weekly.csv")[,-1]
entire_pop_results <- data.frame(
    mean_severe = colSums(results[, (4:107)]),
    mean_nonsevere = colSums(results[, (108:211)]),
    weeks = c(1:104))

total_pop_vaccinated <- sum(results$total_pop)

plot_data <- melt(entire_pop_results, id = "weeks")
  
plot <- ggplot(plot_data %>% filter(variable == "mean_severe"), aes(weeks)) + 
    geom_line(aes(y=value/total_pop_vaccinated * 100000, color = variable), size = 1.5) + 
    xlab("Time (weeks)") + 
    ylab("weekly COVID-19 Incidence (per 100,000)") +
    labs(color='Infection Type') +
    scale_color_discrete(labels = c("Severe Infections"))+
                      # values = c("#00BFC4"))+#, "Nonsevere Infections")) +
    guides(fill = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size=12))+
  ylim(0, 130) +
    ggtitle(paste0("Semi-annual Booster"))
 
plot

sum((results %>% filter(age_group == "75+ years"))$total_pop)

results <- read.csv("simulation-results/final-results/dynamic/75+ strategy/entire_pop_simulation_1Booster_weekly.csv")[,-1]

plot_data <- melt(results %>% mutate(across(week1:nonsevere_week104, ~ .x/total_pop * 100000)) %>%
  select(age_group, immunocompromised, week1:week104), id = c("age_group", "immunocompromised")) %>%
  #select(age_group, immunocompromised, nonsevere_week1:nonsevere_week104), id = c("age_group", "immunocompromised")) %>%
  mutate(weeks = as.numeric(substring(variable, 5)))

plot <- ggplot(plot_data %>% filter(age_group == "75+ years"), aes(weeks)) + 
  geom_line(aes(y=value, color = interaction(age_group, immunocompromised)), size = 1.5) + 
  xlab("Time (weeks)") + 
  ylab("Weekly COVID-19 Incidence (per 100,000)") +
  labs(color='Infection Type') +
 # scale_color_discrete(labels = c("Severe Infections")) +#, "Nonsevere Infections")) +
  guides(fill = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12))+
  #ylim(0, 1500) +
  ylim(0, 470) +
  ggtitle(paste0("Semi-annual Booster"))

plot


results <- read.csv("simulation-results/final-results/dynamic/65+ strategy/optimistic/Annual-Booster/mean/combined.csv")[,-1]
results_min <- read.csv("simulation-results/final-results/dynamic/65+ strategy/optimistic/Annual-Booster/min.csv")[,-1]
results_max <- read.csv("simulation-results/final-results/dynamic/65+ strategy/optimistic/Annual-Booster/max.csv")[,-1]

annual_risk <- results %>% filter(immunocompromised == 0) %>%
  mutate(total_cases = ceiling(select(., week1:week104) %>% rowSums(na.rm = TRUE))) %>%
  mutate(annual_risk = total_cases/(2*total_pop) * 100000) %>%
  dplyr::select(age_group, total_cases, annual_risk)

annual_risk_min <- results_min %>% filter(immunocompromised == 0) %>%
  mutate(total_cases = ceiling(select(., week1:week104) %>% rowSums(na.rm = TRUE))) %>%
  mutate(annual_risk = total_cases/(2*total_pop) * 100000) %>%
  dplyr::select(age_group, total_cases, annual_risk)

annual_risk_max <- results_max %>% filter(immunocompromised == 0) %>%
  mutate(total_cases = ceiling(select(., week1:week104) %>% rowSums(na.rm = TRUE))) %>%
  mutate(annual_risk = total_cases/(2*total_pop) * 100000) %>%
  dplyr::select(age_group, total_cases, annual_risk)




annual_risk_immunocompromised <- results %>% filter(immunocompromised != 0) %>%
  group_by(immunocompromised) %>% summarise(total_pop = sum(total_pop),
                                            across(week1:week104, sum)) %>%
  mutate(total_cases = select(., week1:week104) %>% rowSums(na.rm = TRUE))%>%
  mutate(annual_risk = total_cases/(2*total_pop) * 100000) %>%
  dplyr::select(immunocompromised, total_cases, annual_risk)

age_18_49_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                              total_severe_covid = c(4335, 4000, 3469)) %>%
  mutate(absolute_annual_risk = total_severe_covid/(2*60088),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(60088, 3)/(max(total_severe_covid) - total_severe_covid)))

########################################

strategy <- "18+ strategy"
uptake <- "realistic"
for(vax_intervention in c("1-Booster","Annual-Booster", "Biannual-Booster")){
  run_1 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/1.csv"))[,-1] 
  run_2 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/2.csv"))[,-1] 
  run_3 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/3.csv"))[,-1] 
  run_4 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/4.csv"))[,-1] 
  run_5 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/5.csv"))[,-1] 
  run_6 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/6.csv"))[,-1] 
  run_7 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/7.csv"))[,-1] 
  run_8 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/8.csv"))[,-1] 
  run_9 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/9.csv"))[,-1] 
  run_10 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/10.csv"))[,-1] 
  run_11 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/11.csv"))[,-1] 
  run_12 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/12.csv"))[,-1] 
  run_13 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/13.csv"))[,-1] 
  run_14 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/14.csv"))[,-1] 
  run_15<- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/15.csv"))[,-1] 
  run_16 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/16.csv"))[,-1] 
  run_17 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/17.csv"))[,-1] 
  run_18 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/18.csv"))[,-1] 
  run_19 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/19.csv"))[,-1] 
  run_20 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/20.csv"))[,-1] 
  run_21 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/21.csv"))[,-1] 
  run_22 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/22.csv"))[,-1] 
  run_23 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/23.csv"))[,-1] 
  run_24 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/24.csv"))[,-1] 
  run_25 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/25.csv"))[,-1] 
  
  combined <- rbind(run_1, run_2, run_3, run_4, run_5, run_6, run_7, run_8, run_9, run_10, run_11, run_12, run_13, run_14, run_15, run_16, run_17, run_18, run_19, run_20, run_21, run_22, run_23, run_24, run_25) %>% 
    group_by(age_group, immunocompromised, total_pop) %>% summarise(across(week1:week104, mean)) 
  
  write.csv(combined, paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/combined.csv"))
  
}


strategy <- "75+ strategy"
uptake <- "realistic"
for(vax_intervention in c("1-Booster")){
  vax_intervention <- "1-Booster"
  run_1 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/1.csv"))[,-1] 
  run_2 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/2.csv"))[,-1] 
  run_3 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/3.csv"))[,-1] 
  run_4 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/4.csv"))[,-1] 
  run_5 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/5.csv"))[,-1] 
  run_6 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/6.csv"))[,-1] 
  run_7 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/7.csv"))[,-1] 
  run_8 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/8.csv"))[,-1] 
  run_9 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/9.csv"))[,-1] 
  run_10 <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/10.csv"))[,-1] 
  
  
  combined <- rbind(run_1, run_2, run_3, run_4, run_5, run_6, run_7, run_8, run_9, run_10) %>% 
    group_by(age_group, immunocompromised, total_pop) %>% summarise(across(week1:week104, mean)) 
  
  write.csv(combined, paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/combined.csv"))
  
}



%>%
  mutate(week1 = week1/total_pop * 100000,
         nonsevere_week1 = nonsevere_week1/total_pop * 100000, 
         run = 1) %>%
  select(age_group, immunocompromised, week1, nonsevere_week1)

run_2 <- read.csv("simulation-results/final-results/dynamic/calibration-no booster/entire_pop_simulation_noBooster_weekly_2.csv")[,-1] %>%
  mutate(week1 = week1/total_pop * 100000,
         nonsevere_week1 = nonsevere_week1/total_pop * 100000, 
         run = 2) %>%
  select(age_group, immunocompromised, week1, nonsevere_week1)

run_3 <- read.csv("simulation-results/final-results/dynamic/calibration-no booster/entire_pop_simulation_noBooster_weekly_3.csv")[,-1] %>%
  mutate(week1 = week1/total_pop * 100000,
         nonsevere_week1 = nonsevere_week1/total_pop * 100000, 
         run = 3) %>%
  select(age_group, immunocompromised, week1, nonsevere_week1)

run_4 <- read.csv("simulation-results/final-results/dynamic/calibration-no booster/entire_pop_simulation_noBooster_weekly_4.csv")[,-1] %>%
  mutate(week1 = week1/total_pop * 100000,
         nonsevere_week1 = nonsevere_week1/total_pop * 100000, 
         run = 4) %>%
  select(age_group, immunocompromised, week1, nonsevere_week1)

run_5 <- read.csv("simulation-results/final-results/dynamic/calibration-no booster/entire_pop_simulation_noBooster_weekly_5.csv")[,-1] %>%
  mutate(week1 = week1/total_pop * 100000,
         nonsevere_week1 = nonsevere_week1/total_pop * 100000, 
         run = 5) %>%
  select(age_group, immunocompromised, week1, nonsevere_week1)
  
combined_runs <- rbind(run_1, run_2, run_3, run_4, run_5) %>% group_by(age_group, immunocompromised) %>%
  summarise(avg_severe_inc = mean(week1),
            avg_nonsevere_inc = mean(nonsevere_week1))


###########################################################################################
strategy <- "18+ strategy"
uptake <- "realistic"
for(vax_intervention in c("1-Booster")){
  mean <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/mean/combined.csv")) 
  lower <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/lower/combined.csv")) 
  upper <- read.csv(paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/upper/combined.csv")) 
  
  max <- pmax(mean[,5:108], lower[,5:108], upper[,5:108])
  min <- pmin(mean[,5:108], lower[,5:108], upper[,5:108])
  
  max$age_group <- mean$age_group
  max$immunocompromised <- mean$immunocompromised
  max$total_pop <- mean$total_pop
  
  min$age_group <- mean$age_group
  min$immunocompromised <- mean$immunocompromised
  min$total_pop <- mean$total_pop
  
  write.csv(max, paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/max.csv"))
  write.csv(min, paste0("simulation-results/final-results/dynamic/", strategy, "/", uptake, "/", vax_intervention, "/min.csv"))
  
}




###########################################################################################
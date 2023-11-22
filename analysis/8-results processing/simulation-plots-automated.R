###################################################################################################
#Title: Plots of Simulation Results
#Author: Hailey Park
#Date: April 17, 2023
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
#Set up dfs
age_18_49_95ui <- data.frame(
  age_group = "18-49 years")

age_50_64_95ui <- data.frame(
  age_group = "50-64 years")

age_65_74_95ui <- data.frame(
  age_group = "65-74 years")

age_75plus_95ui <- data.frame(
  age_group = "75+ years")

immuno_mild_95ui <- read.csv("simulation-results/final-results/sensitivity/five-year-sim/immuno-mild/1Booster-summarised.csv")[,-1] %>%
  mutate(age_group = "Immunocompromised \n(Mild)")
immuno_mild_95ui$months <- c(0:60)

immuno_severe_95ui <- read.csv("simulation-results/final-results/sensitivity/five-year-sim/immuno-severe/1Booster-summarised.csv")[,-1] %>%
  mutate(age_group = "Immunocompromised \n(Moderate/Severe)")
immuno_severe_95ui$months <- c(0:60)


immuno_mild <- colSums(immuno_mild_95ui[, c("mean", "max", "min")])
immuno_severe <- colSums(immuno_severe_95ui[, c("mean", "max", "min")])
###################################################################################################
severe_95ui <- function(df){
  age_group <- (df$age_group)[1]
  age_group <- "75+ years"
  intervention <- "biannualBooster"
  mean <- read.csv(paste0("simulation-results/final-results/main_2/immunocompetent/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_1 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_2 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_3 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_4 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_5 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_6 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_7 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_8 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_9 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_10 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_11 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_12 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_13 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_14<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_15<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_16 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_18 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_19<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_20 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_21 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_22 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_23 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_24 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_25 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_26 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_27 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  
  
  df_95ui <- data.frame(
    mean = colMeans(mean)[1:25],
    ui_1 = colMeans(ui_1)[1:25],
    ui_2 = colMeans(ui_2)[1:25],
    ui_3 = colMeans(ui_3)[1:25],
    ui_4 = colMeans(ui_4)[1:25],
    ui_5 = colMeans(ui_5)[1:25],
    ui_6 = colMeans(ui_6)[1:25],
    ui_7 = colMeans(ui_7)[1:25],
    ui_8 = colMeans(ui_8)[1:25],
    ui_9 = colMeans(ui_9)[1:25],
    ui_10 = colMeans(ui_10)[1:25],
    ui_11 = colMeans(ui_11)[1:25],
    ui_12 = colMeans(ui_12)[1:25],
    ui_13 = colMeans(ui_13)[1:25],
    ui_14 = colMeans(ui_14)[1:25],
    ui_15 = colMeans(ui_15)[1:25],
    ui_16 = colMeans(ui_16)[1:25],
    ui_18 = colMeans(ui_18)[1:25],
    ui_19 = colMeans(ui_19)[1:25],
    ui_20 = colMeans(ui_20)[1:25],
    ui_21 = colMeans(ui_21)[1:25],
    ui_22 = colMeans(ui_22)[1:25],
    ui_23 = colMeans(ui_23)[1:25],
    ui_24 = colMeans(ui_24)[1:25],
    ui_25 = colMeans(ui_25)[1:25],
    ui_26 = colMeans(ui_26)[1:25],
    ui_27 = colMeans(ui_27)[1:25],
    age_group = age_group)
  
  # max <- colSums(df_95ui[, names(sort(colSums(df_95ui[,1:27]), T)[1])])
  # min <- df_95ui[, names(sort(colSums(df_95ui[,1:27]), F)[1])]
  # 
  # return(data.frame(age_group = age_group,
  #                   mean = df_95ui$mean,
  #                   max = max,
  #                   min = min))
    age_group = age_group) %>%
    rowwise() %>% mutate(max = max(across(mean:ui_27)),
                         min = min(across(mean:ui_27)))
  
  return(df_95ui)
}

nonsevere_95ui <- function(df){
  age_group <- (df$age_group)[1]
  intervention <- "1Booster"
  mean <- read.csv(paste0("simulation-results/final-results/main_2/immunocompetent/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_1 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_2 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_3 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_4 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_5 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_6 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_7 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_8 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_9 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-lower/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_10 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_11 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_12 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_13 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_14<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_15<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_16 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_18 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-mean/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_19<- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_20 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_21 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-upper/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_22 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_23 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_24 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-lower/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_25 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-upper/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_26 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-mean/", intervention,"-", age_group, "-average.csv"))[,-1]
  ui_27 <- read.csv(paste0("simulation-results/final-results/95ui/immunocompetent/waning-upper/sero-mean/case-lower/", intervention,"-", age_group, "-average.csv"))[,-1]
  
  
  df_95ui <- data.frame(
    mean = colMeans(mean)[26:50],
    ui_1 = colMeans(ui_1)[26:50],
    ui_2 = colMeans(ui_2)[26:50],
    ui_3 = colMeans(ui_3)[26:50],
    ui_4 = colMeans(ui_4)[26:50],
    ui_5 = colMeans(ui_5)[26:50],
    ui_6 = colMeans(ui_6)[26:50],
    ui_7 = colMeans(ui_7)[26:50],
    ui_8 = colMeans(ui_8)[26:50],
    ui_9 = colMeans(ui_9)[26:50],
    ui_10 = colMeans(ui_10)[26:50],
    ui_11 = colMeans(ui_11)[26:50],
    ui_12 = colMeans(ui_12)[26:50],
    ui_13 = colMeans(ui_13)[26:50],
    ui_14 = colMeans(ui_14)[26:50],
    ui_15 = colMeans(ui_15)[26:50],
    ui_16 = colMeans(ui_16)[26:50],
    ui_18 = colMeans(ui_18)[26:50],
    ui_19 = colMeans(ui_19)[26:50],
    ui_20 = colMeans(ui_20)[26:50],
    ui_21 = colMeans(ui_21)[26:50],
    ui_22 = colMeans(ui_22)[26:50],
    ui_23 = colMeans(ui_23)[26:50],
    ui_24 = colMeans(ui_24)[26:50],
    ui_25 = colMeans(ui_25)[26:50],
    ui_26 = colMeans(ui_26)[26:50],
    ui_27 = colMeans(ui_27)[26:50],
    age_group = age_group) %>%
    rowwise() %>% mutate(max = max(across(ui_1:ui_27)),
                         min = min(across(ui_1:ui_27)))
  
  return(df_95ui)
}
##################################################################################################
#Getting 95% UI

age_group_95ui <- list(age_18_49_95ui, age_50_64_95ui, age_65_74_95ui, age_75plus_95ui) %>%
  lapply(severe_95ui) 
##################################################################################################
#Plotting 95% UI

combined <- rbind(age_group_95ui[[1]], age_group_95ui[[2]], age_group_95ui[[3]], age_group_95ui[[4]]) 

combined$months <- rep((0:24), 4)

all_combined <- rbind(combined %>% select(mean, max, min, age_group, months),
                      immuno_mild_95ui, immuno_severe_95ui)

plot_data <- all_combined #%>% mutate(mean = mean/10, max = max/10, min = min/10)

ggplot(plot_data, aes(months)) + 
  geom_line(aes(y=mean/10, color = age_group), size = 1) + 
  geom_ribbon(aes(ymin=min/10, ymax=max/10, fill = age_group), alpha=0.4) +
  xlab("Time (months)") + 
  ylab("Severe COVID-19 Incidence (per 100,000)") +
  labs(color='Age Group') +
  guides(fill = "none") +
  #ylim(0,3100)+
  ylim(0,150) +
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12)) #+
  #ggtitle("Outcome: Severe\nScenario: No Booster")


##################################################################################################
#Tables
age_18_49 <- colSums(age_group_95ui[[1]][, c("mean", "max", "min")])
age_50_64 <- colSums(age_group_95ui[[2]][, c("mean", "max", "min")])
age_65_74 <- colSums(age_group_95ui[[3]][, c("mean", "max", "min")])
age_75plus <- colSums(age_group_95ui[[4]][, c("mean", "max", "min")])

immuno_mild <- colSums(immuno_mild_95ui[, c("mean", "max", "min")])
immuno_severe <- colSums(immuno_severe_95ui[, c("mean", "max", "min")])

inspection <- colSums((immuno_severe_95ui %>% select(-age_group))[, ])
inspection

age_18_49_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                              #total_severe_covid = c(2391,2032,1745)) %>% #mean
                              #total_severe_covid = c(1944,1661,1434)) %>% #lower
                              total_severe_covid = c(2555,2176,1864)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(2*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))


age_50_64_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                              #total_severe_covid = c(4254,3644,3115)) %>% #mean
                              #total_severe_covid = c(3784,3253,2810)) %>% #lower
                              total_severe_covid = c(5412,4591,3883)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(2*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))

age_65_74_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                              #total_severe_covid = c(10449,8874,7599)) %>% #mean
                              #total_severe_covid = c(9939,8491,7254)) %>% #lower
                              total_severe_covid = c(12315,10402,8850)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(2*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))

age_75plus_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                               #total_severe_covid = c(27791,23783,20494)) %>% #mean
                               #total_severe_covid = c(26620,22861,19775)) %>% #lower
                               total_severe_covid = c(32924,27951,23793)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(2*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))

immuno_mild_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                               #total_severe_covid = c(61048,52973, 49610)) %>% #mean
                               #total_severe_covid = c(55863,47071,43215)) %>% #lower
                               total_severe_covid = c(68706,62105,59526)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(5*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))

immuno_severe_table <- data.frame(intervention = c("1 Booster (1 dose)", "Annual Booster (2 doses)", "Biannual Booster (4 doses)"),
                               #total_severe_covid = c(70131,53646,48090)) %>% #mean
                               #total_severe_covid = c(62661,47650,42397)) %>% #lower
                               total_severe_covid = c(81091,62105,56065)) %>% #upper
  mutate(absolute_annual_risk = total_severe_covid/(5*1000000),
         ARR = max(absolute_annual_risk) - absolute_annual_risk,
         RRR = ARR/max(absolute_annual_risk),
         NNT = ceiling(rep(1000000, 3)/(max(total_severe_covid) - total_severe_covid)))

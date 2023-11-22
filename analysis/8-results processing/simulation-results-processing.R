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
#Processing Simulation Results for All Risk Groups
#NOTE:   For each analysis, we have to determine the max and min across the 27 UI versions. 
#
for(age_group in c("18-49 years", "50-64 years", "65-74 years", "75+ years")) {
  
  for(vax_interventions in c("1Booster", "annualBooster", "biannualBooster")){
    
    for(immuno in c("immunocompetent", "immuno-mild", "immuno-severe")) {
      
      #Depending on which analysis you are processing results for, double check file paths
      main <- "main"
      sensitivity <- "sensitivity/high-incidence"
      variant <- "variantAnalysis/S1/ver1"
      
      #CAN CHOOSE BETWEEN 'main', 'sensitivity', 'variant'
      mean <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-mean/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_1 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-upper/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_2 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-upper/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_3 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-upper/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_4 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-lower/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_5 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-lower/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_6 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-lower/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_7 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-mean/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_8 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-mean/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_9 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-lower/sero-mean/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_10 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-upper/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_11 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-upper/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_12 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-upper/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_13 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-lower/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_14<- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-lower/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_15<- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-lower/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_16 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-mean/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      #ui_17 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-mean/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_18 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-mean/sero-mean/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_19<- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-upper/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_20 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-upper/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_21 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-upper/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_22 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-lower/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_23 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-lower/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_24 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-lower/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_25 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-mean/case-upper/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_26 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-mean/case-mean/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      ui_27 <- read.csv(paste0("results/simulation-results/", main, "/", immuno, "/waning-upper/sero-mean/case-lower/", vax_interventions,"-", age_group,"-average.csv"))[,-1]
      
      all_ui <- data.frame(
        mean_severe = colMeans(mean)[1:25],
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
        age_group = "75+ years") %>%
        rowwise() %>% mutate(max = max(across(mean_severe:ui_27)), 
                             min = min(across(mean_severe:ui_27)))
      to_save <- all_ui %>% dplyr::select(mean_severe, max, min)
      to_save$months <- c(0:24)
      
      write.csv(to_save, paste0("results/simulation-results/", main, "/", immuno, "/", vax_interventions,"-", age_group,"-summarised.csv"))
      
    }
  }
}

###################################################################################################
#Processing Simulation Results for Immunocompromised Risk Groups
#NOTE:   For each analysis, we have to determine the max and min across the 27 UI versions. For the 
#        immunocompromised groups (mild + moderate/severe), we are using age-weighted results.
#

for(vax_intervention in c("1Booster", "annualBooster", "biannualBooster")){
  
  for(immuno in c("immuno-mild", "immuno-severe")) {
    
    #Depending on which analysis you are processing results for, double check file paths
    main <- "main"
    sensitivity <- "sensitivity/high-incidence"
    variant <- "variantAnalysis/S1/ver1"
    
    age_18_49 <- read.csv(paste0("results/simulation-results/", main, "/", immuno,"/", vax_intervention, "-18-49 years-summarised.csv"))[,-1]
    
    age_50_64 <- read.csv(paste0("results/simulation-results/", main, "/", immuno,"/", vax_intervention, "-50-64 years-summarised.csv"))[,-1]
    
    age_65_74 <- read.csv(paste0("results/simulation-results/", main, "/", immuno,"/", vax_intervention, "-65-74 years-summarised.csv"))[,-1]
    
    age_75plus <- read.csv(paste0("results/simulation-results/", main, "/", immuno,"/", vax_intervention, "-75+ years-summarised.csv"))[,-1]
    
    #These are age-weights for the immunocompromised group (look at data cleaning folder for calculation of age-weights)
    weighted_immuno_outcomes_mean <- 0.365805431 * age_18_49$mean_severe + 0.283233968 * age_50_64$mean_severe + 0.155565424 * age_65_74$mean_severe + 0.195395177 * age_75plus$mean_severe
    weighted_immuno_outcomes_max <- 0.365805431 * age_18_49$max + 0.283233968 * age_50_64$max + 0.155565424 * age_65_74$max + 0.195395177 * age_75plus$max
    weighted_immuno_outcomes_min <- 0.365805431 * age_18_49$min + 0.283233968 * age_50_64$min + 0.155565424 * age_65_74$min + 0.195395177 * age_75plus$min
    
    combined <- data.frame(mean = weighted_immuno_outcomes_mean,
                           max = weighted_immuno_outcomes_max,
                           min = weighted_immuno_outcomes_min)
    
    write.csv(combined, paste0("results/simulation-results/", main, "/", immuno,"/", vax_intervention,"-summarised.csv"))
  }
}

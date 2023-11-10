###################################################################################################
#Title: Simulation with Non-Severe Infection
#Author: Hailey Park
#Date: September 25, 2023
###################################################################################################

rm(list=ls())
gc()

setwd("~/Stanford Research/booster-timing-final") 

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
library(foreach)
library(doParallel)
library(here)
library(data.table)

# Set up the number of cores used for parallelization.
# Use detectCores() to find out how many cores are available.
num_cores <- detectCores() - 1
registerDoParallel(num_cores)


comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}


noBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:25, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- noBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("simulation-results/final-results/95ui/immuno-severe/waning-", waning, "/sero-", sero, "/case-", case, "/noBooster-", age_info, "-average.csv"))
  
}


oneBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  
  sim_df <- foreach (i = 1:25, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- oneBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
    }

  write.csv(sim_df, paste0("simulation-results/variant-immunocompetent-95ui-rerunning/S3/ver2/waning-", waning, "/sero-", sero, "/case-", case, "/1Booster-", age_info, "-average.csv"))

}

annualBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:25, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- annualBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("simulation-results/variant-immunocompetent-95ui-rerunning/S3/ver2/waning-", waning, "/sero-", sero, "/case-", case, "/annualBooster-", age_info, "-average.csv"))
  
}

biannualBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:25, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- biannualBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("simulation-results/variant-immunocompetent-95ui-rerunning/S3/ver2/waning-", waning, "/sero-", sero, "/case-", case, "/biannualBooster-", age_info, "-average.csv"))
  
}


for(waning in c("upper", "mean", "lower")){
  
  for(sero in c("upper", "mean", "lower")){
    
    for(case in c("upper", "mean", "lower")){
      
      print(paste0("Waning Curve: ", waning))
      print(paste0("Seroprevalence: ", sero))
      print(paste0("Case Multipliers: ", case))
      
      source(here::here("simulation-data-inputs-95ui-immunocompetent.R"))
      source(here::here("intervention-functions-immunocompetent.R"))

      set.seed(88)
      clean_df %>% lapply(average_10_sims_no)
      
      set.seed(88)
      clean_df %>% lapply(average_10_sims_one)

      set.seed(88)
      clean_df %>% lapply(average_10_sims_annual)

      set.seed(88)
      clean_df %>% lapply(average_10_sims_biannual)
      
      
    }
  }
}


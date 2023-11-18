###################################################################################################
#Title: Simulation
#Author: Hailey Park
#Date: September 25, 2023
###################################################################################################

rm(list=ls())
gc()

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
library(foreach)
library(doParallel)
library(here)
library(data.table)

# Set up the number of cores used for parallelization.
# Use detectCores() to find out how many cores are available.
num_cores <- detectCores() - 1
registerDoParallel(num_cores)


#Custom combine function
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}

#Functions for running multiple runs of each booster intervention in parallel
#NOTE: You can change the number of runs by changing out the 'num_sims'.
#      In our study, we ran 25 sims for the mean estimates, and 10 sims for each
#      uncertainty combination.

num_sims <- 25

noBoosterSim_Parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:num_sims, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- noBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero, "/case-", case, "/noBooster-", age_info, "-average.csv"))
}


oneBoosterSim_Parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:num_sims, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- oneBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero, "/case-", case, "/1Booster-", age_info, "-average.csv"))
}

annualBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:num_sims, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- annualBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero, "/case-", case, "/annualBooster-", age_info, "-average.csv"))
  
}

biannualBoosterSim_parallel <- function(df){
  
  age_info <- df$age_group[1]
  
  sim_df <- foreach (i = 1:num_sims, .combine='comb', .multicombine=TRUE) %dopar% {
    results <- biannualBoosterSimulation(df)
    list(results[[1]], results[[2]], results[[3]])
  }
  
  write.csv(sim_df, paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero, "/case-", case, "/biannualBooster-", age_info, "-average.csv"))
  
}

#Set up folder structure to save simulation results
dir.create("results/simulation-results")
dir.create("results/simulation-results/variantAnalysis")
dir.create("results/simulation-results/variantAnalysis/S1")
dir.create("results/simulation-results/variantAnalysis/S1/ver1")

for (immune_status in c("immunocompetent", "immunoMild", "immunoSevere")) {
  dir.create(paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status))
  
  for (waning in c("upper", "mean", "lower")) {
    dir.create(paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning))
    
    for (sero in c("upper", "mean", "lower")) {
      dir.create(paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero))
      
      for (case in c("upper", "mean", "lower")) {
        dir.create(paste0("results/simulation-results/variantAnalysis/S1/ver1/", immune_status, "/waning-", waning, "/sero-", sero, "/case-", case))
      }
    }
  }
}

#Run Booster Interventions

# NOTE:   To quantify uncertainty in the study findings, we generated uncertainty intervals (UI) for our model
#         estimate. These intervals account for the uncertainty in protective effectiveness and waning over 
#         time, in addition to the uncertainty in baseline age-specific seroprevalence estimates and age-specific
#         non-severe infection multipliers. For each model parameter, we created ‘upper’, ‘mean’, and ‘lower’ 
#         bound versions (Table A5) and ran simulations under each combination of model parameter bounds. Each
#         combination of model parameters and immune status requires a different calibration, hence why we 
#         loop through each combination and re-call scripts. 
#         
#         

for (immune_status in c("immunocompetent", "immunoMild", "immunoSevere")) {
  
  for (waning in c("upper", "mean", "lower")){
    
    for (sero in c("upper", "mean", "lower")){
      
      for (case in c("upper", "mean", "lower")){
        
        print(paste0("Immuno Status: ", immune_status))
        print(paste0("Waning Curve: ", waning))
        print(paste0("Seroprevalence: ", sero))
        print(paste0("Case Multipliers: ", case))
        
        #Can choose 'immune evasion ver1' or 'immune evasion ver2'
        source(here::here(paste0("analysis/5-variant scenario analysis/S1/immune evasion ver1/simulation-data-inputs-95ui-", immune_status, ".R")))
        source(here::here(paste0("analysis/5-variant scenario analysis/S1/intervention-functions-variantAnalysis-S1-", immune_status, ".R")))
        
        set.seed(88)
        clean_df %>% lapply(noBoosterSim_Parallel)
        
        set.seed(88)
        clean_df %>% lapply(oneBoosterSim_Parallel)
        
        set.seed(88)
        clean_df %>% lapply(annualBoosterSim_parallel)
        
        set.seed(88)
        clean_df %>% lapply(biannualBoosterSim_parallel)
        
        
      }
    }
  }
}

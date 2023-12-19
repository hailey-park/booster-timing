###################################################################################################
#Title: Simulation with Non-Severe Infection (Dynamic Model)
#Author: Hailey Park
#Date: September 1, 2023
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

#Set number of sims
num_sims <- 10

#Set up folder structure to save simulation results
dir.create("results/simulation-results/dynamic")

for (pop_strat in c("18+", "65+", "75+")) {
  dir.create(paste0("results/simulation-results/dynamic/", pop_strat, " strategy"))
  for (vax_assign in c("realistic", "optimistic")) {
    dir.create(paste0("results/simulation-results/dynamic/", pop_strat, " strategy/", vax_assign))
    for (intervention in c("1-Booster", "Annual-Booster", "Biannual-Booster")) {
      dir.create(paste0("results/simulation-results/dynamic/", pop_strat, " strategy/", vax_assign, "/", intervention))
      for (waning in c("upper", "mean", "lower")) {
        dir.create(paste0("results/simulation-results/dynamic/", pop_strat, " strategy/", vax_assign, "/", intervention, "/waning-", waning))
      }
    }
  }
}


#Iterate through each population targeting strategy (18+, 65+, 75+)
for(pop_strat in c("18+", "65+", "75+")) {
  
  #Iterate through each waning estimate
  for(waning in c("lower", "mean", "upper")) {
    
    #Read in files 
    source(here::here("analysis/4-dynamic model analysis/simulation-data-inputs-dynamic.R"))
    source(here::here(paste0("analysis/4-dynamic model analysis/intervention-functions-dynamic-", pop_strat, ".R")))
    
    #Select either realistic or optimistic vaccine uptake scenario
    set.seed(88)
    assign_vax <- clean_df %>% 
      lapply(realistic_vax_assignment) 
    # lapply(optimistic_vax_assignment)
    
    #Run one-time booster program
    set.seed(88)
    sim_df_1B <- foreach (i = 1:num_sims, .packages = c("dplyr", "data.table")) %dopar% {
      results <- oneBoosterSimulation(assign_vax[[1]])
      list(results)
    }
    
    for (i in c(1:num_sims)){
      write.csv(sim_df_1B[[i]][[1]], paste0("results/simulation-results/dynamic/", pop_strat, " strategy/realistic/1-Booster/", waning, "/", i, ".csv"))
    }
    
    #Run annual booster program
    set.seed(88)
    sim_df_aB <- foreach (i = 1:num_sims, .packages = c("dplyr", "data.table")) %dopar% {
      results <- annualBoosterSimulation(assign_vax[[1]])
      list(results)
    }
    
    for (i in c(1:num_sims)){
      write.csv(sim_df_aB[[i]][[1]], paste0("results/simulation-results/dynamic/", pop_strat, " strategy/realistic/Annual-Booster/", waning, "/", i, ".csv"))
    }
    
    #Run semi-annual booster program
    set.seed(88)
    sim_df_bB <- foreach (i = 1:num_sims, .packages = c("dplyr", "data.table")) %dopar% {
      results <- biannualBoosterSimulation(assign_vax[[1]])
      list(results)
    }
    
    
    for (i in c(1:num_sims)){
      write.csv(sim_df_bB[[i]][[1]], paste0("results/simulation-results/dynamic/", pop_strat, " strategy/realistic/Biannual-Booster/", waning, "/", i, ".csv"))
    }
    
  }
}


###################################################################################################

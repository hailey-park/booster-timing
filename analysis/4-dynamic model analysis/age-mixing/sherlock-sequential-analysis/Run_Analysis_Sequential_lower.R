###################################################################################################
#Title: Simulation with Non-Severe Infection (Dynamic Model)
#Author: Hailey Park
#Date: September 1, 2023
###################################################################################################

rm(list=ls())
gc()

#Load libraries
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(here)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
job_selector <- (as.numeric(args[1])-1)

###################################################################################################
#75+ Population Strategy; Optimistic Uptake

if (((job_selector==0)|(job_selector==1))|(job_selector==2)) {
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-75+.R"))

  set.seed(88)
  inspection <- clean_df %>% 
    lapply(optimistic_vax_assignment)
}

if (job_selector==0){
  #Running 1 Booster (will only do for 75+ pop strat since this is same across all population strategies)
  set.seed(88)
  sim_df_1B <- for (i in 1:25) {
    results <- oneBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/optimistic/1-Booster/", i, ".csv"))
  }
} else if (job_selector==1){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/optimistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==2){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/optimistic/Biannual-Booster/", i, ".csv"))
  }
}

###################################################################################################
#65+ Population Strategy; Optimistic Uptake

if ((job_selector==3)|(job_selector==4)){
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-65+.R"))

  set.seed(88)
  inspection <- clean_df %>% 
    lapply(optimistic_vax_assignment)
}

if (job_selector==3){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/65+ strategy/optimistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==4){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/65+ strategy/optimistic/Biannual-Booster/", i, ".csv"))
  }
}

###################################################################################################
#18+ Population Strategy; Optimistic Uptake

if ((job_selector==5)|(job_selector==6)){
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-18+.R"))

  set.seed(88)
  inspection <- clean_df %>% 
    lapply(optimistic_vax_assignment)
}

if (job_selector==5){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/18+ strategy/optimistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==6){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/18+ strategy/optimistic/Biannual-Booster/", i, ".csv"))
  }
}
###################################################################################################
#75+ Population Strategy; Realistic Uptake

if (((job_selector==7)|(job_selector==8))|(job_selector==9)) {
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-75+.R"))
  
  set.seed(88)
  inspection <- clean_df %>% 
    lapply(realistic_vax_assignment)
}

if (job_selector==7){
  #Running 1 Booster (will only do for 75+ pop strat since this is same across all population strategies)
  set.seed(88)
  sim_df_1B <- for (i in 1:25) {
    results <- oneBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/realistic/1-Booster/", i, ".csv"))
  }
} else if (job_selector==8){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/realistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==9){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/75+ strategy/realistic/Biannual-Booster/", i, ".csv"))
  }
}

###################################################################################################
#65+ Population Strategy; Realistic Uptake

if ((job_selector==10)|(job_selector==11)){
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-65+.R"))
  
  set.seed(88)
  inspection <- clean_df %>% 
    lapply(realistic_vax_assignment)
}

if (job_selector==10){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/65+ strategy/realistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==11){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/65+ strategy/realistic/Biannual-Booster/", i, ".csv"))
  }
}

###################################################################################################
#18+ Population Strategy; Realistic Uptake

if ((job_selector==12)|(job_selector==13)){
  source(here::here("simulation-data-inputs-dynamic-with-age-mixing-lower.R"))
  source(here::here("intervention-functions-dynamic with mixing-18+.R"))
  
  set.seed(88)
  inspection <- clean_df %>% 
    lapply(realistic_vax_assignment)
}

if (job_selector==12){
  #Running Annual Booster
  set.seed(88)
  sim_df_aB <- for (i in 1:25) {
    results <- annualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/18+ strategy/realistic/Annual-Booster/", i, ".csv"))
  }
} else if (job_selector==13){
  #Running Biannual Booster
  set.seed(88)
  sim_df_bB <- for (i in 1:25) {
    results <- biannualBoosterSimulation(inspection[[1]])
    write.csv(results, paste0("simulation-results/lower/18+ strategy/realistic/Biannual-Booster/", i, ".csv"))
  }
}
###################################################################################################

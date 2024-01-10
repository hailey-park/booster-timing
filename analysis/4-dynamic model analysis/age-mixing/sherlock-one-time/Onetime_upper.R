rm(list=ls())
gc()

#Load libraries
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(here)
library(data.table)

#Run calibration script (this should produce a calibrated population file ~1.4 Gb inside the folder 'calibration-results')
source(here::here("dynamic-model-calibration-with-age-mixing-upper.R"))


#Set up folder structure to save simulation results
dir.create("simulation-results")
dir.create("simulation-results/upper")

for (pop_strat in c("75+ strategy", "65+ strategy", "18+ strategy")) {
  dir.create(paste0("simulation-results/upper/", pop_strat))
  
  for (uptake in c("realistic", "optimistic")) {
    dir.create(paste0("simulation-results/upper/", pop_strat, "/", uptake))
    
    for (intervention in c("1-Booster", "Annual-Booster", "Biannual-Booster")) {
      dir.create(paste0("simulation-results/upper/", pop_strat, "/", uptake, "/", intervention))
      
    }
  }
}
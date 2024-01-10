###################################################################################################
#Title: Interventions - Variant Analysis S4 (New variant and updated vaccine annually)
#Author: Hailey Park
#Date: Septemeber 25, 2023
###################################################################################################


#Function for outcome occurrence based on risk (Risk = Lambda* (1 - PE))
outcome_occurrence <- function(age, inf, time, lambda, perfect_immunity_counter, death_marker, new_variant_marker) {
  
  severe_pe <- rep(1, length(age))
  nonsevere_pe <- rep(1, length(age))
  
  #Creating a df of individuals eligible for infection to merge with waning_data_clean to get protection at specific time point
  index_individuals_eligible <- which(perfect_immunity_counter == 0 & death_marker == 0)
  df_individuals_eligible <- data.table(index_individual = index_individuals_eligible,
                                        age_group = age[index_individuals_eligible],
                                        prior_inf = inf[index_individuals_eligible],
                                        months = time[index_individuals_eligible],
                                        new_variant = new_variant_marker[index_individuals_eligible])
  
  df_protection <- (df_individuals_eligible[waning_data_clean, 
                                            on=c("age_group", "prior_inf", "months"), 
                                            nomatch = NULL]) %>% arrange(index_individual)
  
  #For individuals with new variant introduced (new variant marker), use the new protection estimates
  
  index_old_variant <- which(new_variant_marker == 0)
  index_new_variant1 <- which(new_variant_marker == 1)
  index_new_variant2 <- which(new_variant_marker == 2)
  
  severe_pe[intersect(index_individuals_eligible, index_old_variant)] <- (df_protection %>% filter(new_variant == 0))$old_severe_ve_pred
  severe_pe[intersect(index_individuals_eligible, index_new_variant1)] <- (df_protection %>% filter(new_variant == 1))$var1_severe_ve_pred
  severe_pe[intersect(index_individuals_eligible, index_new_variant2)] <- (df_protection %>% filter(new_variant == 2))$var2_severe_ve_pred
  
  nonsevere_pe[intersect(index_individuals_eligible, index_old_variant)] <- (df_protection %>% filter(new_variant == 0))$old_nonsevere_ve_pred
  nonsevere_pe[intersect(index_individuals_eligible, index_new_variant1)] <- (df_protection %>% filter(new_variant == 1))$var1_nonsevere_ve_pred
  nonsevere_pe[intersect(index_individuals_eligible, index_new_variant2)] <- (df_protection %>% filter(new_variant == 2))$var2_nonsevere_ve_pred
  
  
  print(paste0("New Variant Marker: ", new_variant_marker[546]))
  print(paste0("Severe PE: ", severe_pe[546]))
  print(paste0("Nonsevere PE: ", nonsevere_pe[546]))
  
  severe_risk <- lambda * (1 - severe_pe)
  
  nonsevere_multiplier <- (nonsevere_infection_multipliers %>% filter(age_group == age[1]))$multiplier / 2.8 #CHANGE HERE
  nonsevere_multiplier_adj <- (nonsevere_infection_multipliers %>% filter(age_group == age[1]))$multiplier_adjustment
  nonsevere_risk <- lambda * (1 - nonsevere_pe) * nonsevere_multiplier/nonsevere_multiplier_adj
  return(list(rbinom(length(severe_risk), 1, severe_risk), rbinom(length(nonsevere_risk), 1, nonsevere_risk),
              severe_pe))
}

set.seed(88)

###########################################################################################

oneBoosterSimulation <- function(df){
  
  #Store averted outcomes in new df
  averted <- df %>% arrange(individual)
  averted[sprintf("month%s",(0:24))] <- NA
  averted[sprintf("nonsevere_month%s",(0:24))] <- NA
  averted['total_deaths'] <- 0
  averted['total_hosps'] <- 0
  age_info <- averted$age_group[1]
  averted['perc_death'] <- (hosp_death_age_stratified %>% filter(age_group == age_info))$perc_death
  averted['vaccine_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave_2'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  
  input<- averted
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  time_since_last <- input$months_since_last_dose_inf
  time_since_last_dose <- input$months_since_last_dose
  lambda <- input$lambda
  prob_death <- input$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity months
  index_recent_infection <- which(inf == 1 & time_since_last < 3 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 4 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  new_variant_marker <- rep(0,nrow(input)) #New variant is introduced in waves, where new variant has worsened waning protection
  updated_vax_marker <- rep(0, nrow(input)) #If someone gets vaccine during new variant, then they need to get improved vaccine protection
  months <- c(1:24)
  vaccine_wave <- input$vaccine_wave
  variant_wave <- input$variant_wave
  variant_wave_2 <- input$variant_wave_2
  average_pe <- rep(0, length(months))
  
  #Iterate through each time step
  for (i in (1:25)) {
    print(i)
    
    #Staggering first variant introduction over 3-month window 
    if(i %in% c(2:4)){
      variant_wave_index <- which(variant_wave == i - 1)
      new_variant_marker[variant_wave_index] <- 1
      inf[intersect(variant_wave_index, which(inf == 0 & updated_vax_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(variant_wave_index, which(inf == 1 & updated_vax_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
      
    } 
    
    #Staggering second variant introduction over 3-month window 
    if(i %in% c(14:16)){
      variant_wave_index <- which(variant_wave_2 == i - 13)
      new_variant_marker[variant_wave_index] <- 2
    } 
    
    #Staggering vaccination over 3-month window 
    if(i %in% c(2:4)){
      vaccine_wave_index <- which(vaccine_wave == i - 1)
      time_since_last[vaccine_wave_index] <- 1
      updated_vax_marker[vaccine_wave_index] <- 1
      inf[intersect(vaccine_wave_index, which(inf == 0 & new_variant_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(vaccine_wave_index, which(inf == 1 & new_variant_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
    } 
    
    time_since_last[time_since_last >= 24] <- 24     #Assuming that >24 month waning is same as 24 month waning pe
    
    month <- months[time_since_last]
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, month, lambda, perfect_immunity_counter, death_marker, new_variant_marker)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    severe_pe <- outcomes[[3]]
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs,
    #change their prior infection status to 1/4/7 depending on period, time since last to 1, perfect immunity counter to 3
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    
    inf[intersect(index_outcome, which(new_variant_marker == 0))] <- 1 
    inf[intersect(index_outcome, which(new_variant_marker == 1))] <- 4
    inf[intersect(index_outcome, which(new_variant_marker == 2))] <- 7
    
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 3
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[1])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Add data to dataframe
    input[, i + 7] <- severe_outcomes
    input[, i + 32] <- nonsevere_outcomes
    
    average_pe[i] <- mean(severe_pe)
    
    
  }
  input$total_hosps <- hosp_count
  input$total_deaths <- death_count
  input[i,,drop = FALSE]
  
  return(list(colSums(input[, (8:57)]), 
              sum(colSums((input %>% filter(prior_inf == 1))[, (8:32)])), 
              sum(colSums((input %>% filter(prior_inf == 0))[, (8:32)])),
              average_pe))
}



annualBoosterSimulation <- function(df){
  
  #Store averted outcomes in new df
  averted <- df %>% arrange(individual)
  averted[sprintf("month%s",(0:24))] <- NA
  averted[sprintf("nonsevere_month%s",(0:24))] <- NA
  averted['total_deaths'] <- 0
  averted['total_hosps'] <- 0
  age_info <- averted$age_group[1]
  averted['perc_death'] <- (hosp_death_age_stratified %>% filter(age_group == age_info))$perc_death
  averted['vaccine_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave_2'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  
  input<- averted
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  time_since_last <- input$months_since_last_dose_inf
  time_since_last_dose <- input$months_since_last_dose
  lambda <- input$lambda
  prob_death <- input$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity months
  index_recent_infection <- which(inf == 1 & time_since_last < 3 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 4 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  new_variant_marker <- rep(0,nrow(input)) #New variant is introduced in waves, where new variant has worsened waning protection
  updated_vax_marker <- rep(0, nrow(input)) #If someone gets vaccine during new variant, then they need to get improved vaccine protection
  months <- c(1:24)
  vaccine_wave <- input$vaccine_wave
  variant_wave <- input$variant_wave
  variant_wave_2 <- input$variant_wave_2
  
  #Iterate through each time step
  for (i in (1:25)) {
    
    #Staggering first variant introduction over 3-month window 
    if(i %in% c(2:4)){
      variant_wave_index <- which(variant_wave == i - 1)
      new_variant_marker[variant_wave_index] <- 1
      inf[intersect(variant_wave_index, which(inf == 0 & updated_vax_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(variant_wave_index, which(inf == 1 & updated_vax_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
      
    } 
    
    #Staggering second variant introduction over 3-month window 
    if(i %in% c(14:16)){
      variant_wave_index <- which(variant_wave_2 == i - 13)
      new_variant_marker[variant_wave_index] <- 2
      inf[intersect(variant_wave_index, which(inf == 2 & updated_vax_marker == 2))] <- 5 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 2
      inf[intersect(variant_wave_index, which(inf == 3 & updated_vax_marker == 2))] <- 4 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection before simulation start)
      inf[intersect(variant_wave_index, which(inf == 4 & updated_vax_marker == 2))] <- 6 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection during novel variant 1)
      
    } 
    
    #Staggering vaccination over 3-month window (1st year)
    if(i %in% c(2:4)){
      vaccine_wave_index <- which(vaccine_wave == i - 1)
      time_since_last[vaccine_wave_index] <- 1
      updated_vax_marker[vaccine_wave_index] <- 1
      inf[intersect(vaccine_wave_index, which(inf == 0 & new_variant_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(vaccine_wave_index, which(inf == 1 & new_variant_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
    } 
    
    #Staggering vaccination over 3-month window (2nd year)
    if(i %in% c(14:16)){
      vaccine_wave_index <- which((vaccine_wave == i - 13))
      time_since_last[vaccine_wave_index] <- 1
      updated_vax_marker[vaccine_wave_index] <- 2
      inf[intersect(vaccine_wave_index, which(inf == 2 & new_variant_marker == 2))] <- 5 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 2
      inf[intersect(vaccine_wave_index, which(inf == 3 & new_variant_marker == 2))] <- 4 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection before simulation start)
      inf[intersect(vaccine_wave_index, which(inf == 4 & new_variant_marker == 2))] <- 6 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection during novel variant 1)
      
    }
    
    time_since_last[time_since_last >= 24] <- 24     #Assuming that >24 month waning is same as 24 month waning pe
    
    month <- months[time_since_last]
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, month, lambda, perfect_immunity_counter, death_marker, new_variant_marker)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs,
    #change their prior infection status to 1/4/7 depending on period, time since last to 1, perfect immunity counter to 3
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    
    inf[intersect(index_outcome, which(new_variant_marker == 0))] <- 1 
    inf[intersect(index_outcome, which(new_variant_marker == 1))] <- 4
    inf[intersect(index_outcome, which(new_variant_marker == 2))] <- 7
    
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 3
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[1])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Add data to dataframe
    input[, i + 7] <- severe_outcomes
    input[, i + 32] <- nonsevere_outcomes
    
  }
  input$total_hosps <- hosp_count
  input$total_deaths <- death_count
  input[i,,drop = FALSE]
  
  return(list(colSums(input[, (8:57)]), 
              sum(colSums((input %>% filter(prior_inf == 1))[, (8:32)])), 
              sum(colSums((input %>% filter(prior_inf == 0))[, (8:32)]))))
}



biannualBoosterSimulation <- function(df){
  
  #Store averted outcomes in new df
  averted <- df %>% arrange(individual)
  averted[sprintf("month%s",(0:24))] <- NA
  averted[sprintf("nonsevere_month%s",(0:24))] <- NA
  averted['total_deaths'] <- 0
  averted['total_hosps'] <- 0
  age_info <- averted$age_group[1]
  averted['perc_death'] <- (hosp_death_age_stratified %>% filter(age_group == age_info))$perc_death
  averted['vaccine_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  averted['variant_wave_2'] <- sample(c(1:3), nrow(averted), replace = TRUE)
  
  input<- averted
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  time_since_last <- input$months_since_last_dose_inf
  time_since_last_dose <- input$months_since_last_dose
  lambda <- input$lambda
  prob_death <- input$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity months
  index_recent_infection <- which(inf == 1 & time_since_last < 3 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 4 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  new_variant_marker <- rep(0,nrow(input)) #New variant is introduced in waves, where new variant has worsened waning protection
  updated_vax_marker <- rep(0, nrow(input)) #If someone gets vaccine during new variant, then they need to get improved vaccine protection
  months <- c(1:24)
  vaccine_wave <- input$vaccine_wave
  variant_wave <- input$variant_wave
  variant_wave_2 <- input$variant_wave_2
  
  #Iterate through each time step
  for (i in (1:25)) {
    
    #Staggering first variant introduction over 3-month window 
    if(i %in% c(2:4)){
      variant_wave_index <- which(variant_wave == i - 1)
      new_variant_marker[variant_wave_index] <- 1
      inf[intersect(variant_wave_index, which(inf == 0 & updated_vax_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(variant_wave_index, which(inf == 1 & updated_vax_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
      
    } 
    
    #Staggering second variant introduction over 3-month window 
    if(i %in% c(14:16)){
      variant_wave_index <- which(variant_wave_2 == i - 13)
      new_variant_marker[variant_wave_index] <- 2
      inf[intersect(variant_wave_index, which(inf == 2 & updated_vax_marker == 2))] <- 5 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 2
      inf[intersect(variant_wave_index, which(inf == 3 & updated_vax_marker == 2))] <- 4 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection before simulation start)
      inf[intersect(variant_wave_index, which(inf == 4 & updated_vax_marker == 2))] <- 6 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection during novel variant 1)
      
    } 
    
    #Staggering vaccination over 3-month window (1st year, 1st booster)
    if(i %in% c(2:4)){
      vaccine_wave_index <- which(vaccine_wave == i - 1)
      time_since_last[vaccine_wave_index] <- 1
      updated_vax_marker[vaccine_wave_index] <- 1
      inf[intersect(vaccine_wave_index, which(inf == 0 & new_variant_marker == 1))] <- 2 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 1
      inf[intersect(vaccine_wave_index, which(inf == 1 & new_variant_marker == 1))] <- 3 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 1
    } 
    
    #Staggering vaccination over 3-month window (2nd year, 1st booster)
    if(i %in% c(14:16)){
      vaccine_wave_index <- which((vaccine_wave == i - 13))
      time_since_last[vaccine_wave_index] <- 1
      updated_vax_marker[vaccine_wave_index] <- 2
      inf[intersect(vaccine_wave_index, which(inf == 2 & new_variant_marker == 2))] <- 5 #Updated booster restores vaccine-induced immunity from downgraded curve of novel variant 2
      inf[intersect(vaccine_wave_index, which(inf == 3 & new_variant_marker == 2))] <- 4 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection before simulation start)
      inf[intersect(vaccine_wave_index, which(inf == 4 & new_variant_marker == 2))] <- 6 #Updated booster partially restores hybrid immunity from downgraded curve of novel variant 2 (prior infection during novel variant 1)
      
    }
    
    #Staggering vaccination over 3-month window (each year, 2nd booster)
    if(i %in% c(8:10, 20:22)){
      vaccine_wave_index <- which((vaccine_wave == i - 7) | (vaccine_wave == i - 19))
      time_since_last[vaccine_wave_index] <- 1
    } 
    
    time_since_last[time_since_last >= 24] <- 24     #Assuming that >24 month waning is same as 24 month waning pe
    
    month <- months[time_since_last]
    
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, month, lambda, perfect_immunity_counter, death_marker, new_variant_marker)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs,
    #change their prior infection status to 1/4/7 depending on period, time since last to 1, perfect immunity counter to 3
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    
    inf[intersect(index_outcome, which(new_variant_marker == 0))] <- 1 
    inf[intersect(index_outcome, which(new_variant_marker == 1))] <- 4
    inf[intersect(index_outcome, which(new_variant_marker == 2))] <- 7
    
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 3
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[1])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Add data to dataframe
    input[, i + 7] <- severe_outcomes
    input[, i + 32] <- nonsevere_outcomes
    
    
    
  }
  input$total_hosps <- hosp_count
  input$total_deaths <- death_count
  input[i,,drop = FALSE]
  
  return(list(colSums(input[, (8:57)]), 
              sum(colSums((input %>% filter(prior_inf == 1))[, (8:32)])), 
              sum(colSums((input %>% filter(prior_inf == 0))[, (8:32)]))))
}

###########################################################################################
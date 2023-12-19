########################################################################################################################
#Title: Vaccination Interventions (Dynamic with Age Mixing)
#Author: Hailey Park
#Date: December 7, 2023
########################################################################################################################

#Function for outcome occurrence based on risk (Risk = Lambda* (1 - PE))
outcome_occurrence <- function(age, inf, time, immuno, doses, lambda, perfect_immunity_counter, death_marker, weekly_infection_by_age, contact_matrix_adj_factors) {
  
  severe_pe <- rep(1, length(age))
  nonsevere_pe <- rep(1, length(age))
  beta <- rep(0, length(age))
  severe_multiplier_with_adj <- rep(1, length(age))
  
  #Creating a df of individuals eligible for infection to merge with waning_data_clean to get protection at specific time point
  index_individuals_eligible <- which(perfect_immunity_counter == 0 & death_marker == 0)
  df_individuals_eligible <- data.table(index_individual = index_individuals_eligible,
                                        age_group = age[index_individuals_eligible],
                                        prior_inf = inf[index_individuals_eligible],
                                        immunocompromised = immuno[index_individuals_eligible],
                                        num_doses = doses[index_individuals_eligible],
                                        weeks = time[index_individuals_eligible],
                                        key = c("age_group", "prior_inf", "immunocompromised", "weeks"))
  
  df_protection <- (df_individuals_eligible[waning_data_clean, 
                                            on=c("age_group", "prior_inf", "immunocompromised", "weeks"), 
                                            nomatch = NULL])[severe_infection_multipliers, on = c("age_group", "immunocompromised"), nomatch = NULL] %>% arrange(index_individual)
  
  print(df_protection %>% filter(index_individual == 9077775))
  
  #For individuals who are immune naive, they have 0 protection
  df_protection$severe_ve_pred[df_protection$num_doses == "unvax" & df_protection$prior_inf == 0] <- 0
  df_protection$nonsevere_ve_pred[df_protection$num_doses == "unvax" & df_protection$prior_inf == 0] <- 0
  
  #Individuals who are unvaccinated and have prior infection history have prior infection only waning immuntiy
  df_protection$severe_ve_pred[df_protection$num_doses=="unvax" & df_protection$prior_inf == 1] <- df_protection$severe_prior_inf_only_ve_pred[df_protection$num_doses=="unvax" & df_protection$prior_inf == 1]
  df_protection$nonsevere_ve_pred[df_protection$num_doses=="unvax" & df_protection$prior_inf == 1] <- df_protection$nonsevere_prior_inf_only_ve_pred[df_protection$num_doses=="unvax" & df_protection$prior_inf == 1]
  
  
  #Calculate protection (severe + nonsevere), beta, and multipliers for individuals eligible for infection
  severe_pe[index_individuals_eligible] <- df_protection$severe_ve_pred
  nonsevere_pe[index_individuals_eligible] <- df_protection$nonsevere_ve_pred
  beta[index_individuals_eligible] <- df_protection$beta
  severe_multiplier_with_adj[index_individuals_eligible] <- df_protection$multiplier/df_protection$multiplier_adj
  
  #Calculate the age contact matrix terms for all individuals
  contact_matrix_term_by_age <- data.table(age_group = c("0-17 years","18-49 years", "50-64 years", "65-74 years", "75+ years"),
                                           contact_matrix_term = c(sum(weekly_infection_by_age/inf_by_age$total_pop * contact_matrix$X0.17.years),
                                                                    sum(weekly_infection_by_age/inf_by_age$total_pop * contact_matrix$X18.49.years),
                                                                    sum(weekly_infection_by_age/inf_by_age$total_pop * contact_matrix$X50.64.years),
                                                                    sum(weekly_infection_by_age/inf_by_age$total_pop * contact_matrix$X65.74.years),
                                                                    sum(weekly_infection_by_age/inf_by_age$total_pop * contact_matrix$X75..years)))
  
  weekly_contact_matrix_term <- (data.table(age_group = age)[contact_matrix_term_by_age, 
                                                        on=c("age_group"), 
                                                        nomatch = NULL])$contact_matrix_term
  
  nonsevere_risk <- lambda * (1 - nonsevere_pe) * beta * contact_matrix_adj_factors * (weekly_contact_matrix_term)
  
  severe_risk <- lambda * (1 - severe_pe) * beta * contact_matrix_adj_factors * (weekly_contact_matrix_term) * severe_multiplier_with_adj
  
  #if nonsevere or severe risk > 1, set to 1
  nonsevere_risk[nonsevere_risk > 1] <- 1
  severe_risk[severe_risk > 1] <- 1
  
  #Simulate outcomes
  severe_outcomes <- rbinom(length(severe_risk), 1, severe_risk)
  nonsevere_outcomes <- rbinom(length(nonsevere_risk), 1, nonsevere_risk)
  
  return(list(severe_outcomes, nonsevere_outcomes))
}

########################################################################################################################
#Vaccination Assignment under different coverages (realistic vs. optimistic)
#Assign who is getting vaccinated using age-specific coverage rates. This is important for the 1-booster base case
#Additional vaccinations are assigned inside the vaccination intervention functions.

realistic_vax_assignment <- function(df){
  #Assign who is getting vaccinated using age-specific coverage rates
  vax_assignment <- df %>% mutate(vax = 0)
  
  age_18_49_index <- which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax")
  age_50_64_index <- which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax")
  age_65_plus_index <- which((vax_assignment$age_group == "65-74 years" | vax_assignment$age_group == "75+ years") & vax_assignment$num_doses != "unvax")
  immunocompromised_index <- which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2)  & vax_assignment$num_doses != "unvax")
  
  vax_assignment$vax[age_18_49_index] <- rbinom(length(age_18_49_index), 1, 0.17 * length(which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_18_49_index))
  vax_assignment$vax[age_50_64_index] <- rbinom(length(age_50_64_index), 1, 0.28 * length(which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_50_64_index))
  vax_assignment$vax[age_65_plus_index] <- rbinom(length(age_65_plus_index), 1, 0.45 * length(which(vax_assignment$age_group %in% c("65-74 years", "75+ years") & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_65_plus_index))
  vax_assignment$vax[immunocompromised_index] <- rbinom(length(immunocompromised_index), 1, 0.6 )
  
  print(paste0("Percentage of 18-49 years receiving vaccinated: ", length(which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_18_49_index)))
  print(paste0("Percentage of 50-64 years receiving vaccinated: ",  length(which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_50_64_index)))
  print(paste0("Percentage of 65+ years receiving vaccinated: ",  length(which(vax_assignment$age_group %in% c("65-74 years", "75+ years") & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_65_plus_index)))
  print(paste0("Percentage of immunocompromised receiving vaccinated: ",  length(which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2) & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(immunocompromised_index)))
  
  return(vax_assignment %>% arrange(individual))
}

optimistic_vax_assignment <- function(df){
  #Assign who is getting vaccinated using age-specific coverage rates
  vax_assignment <- df %>% mutate(vax = 0)
  
  age_18_49_index <- which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax")
  age_50_64_index <- which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax")
  age_65_plus_index <- which((vax_assignment$age_group == "65-74 years" | vax_assignment$age_group == "75+ years") & vax_assignment$num_doses != "unvax")
  immunocompromised_index <- which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2)  & vax_assignment$num_doses != "unvax")
  
  vax_assignment$vax[age_18_49_index] <- rbinom(length(age_18_49_index), 1, 0.3 * length(which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_18_49_index))
  vax_assignment$vax[age_50_64_index] <- rbinom(length(age_50_64_index), 1, 0.4 * length(which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_50_64_index))
  vax_assignment$vax[age_65_plus_index] <- rbinom(length(age_65_plus_index), 1, 0.7)# * length(which(vax_assignment$age_group %in% c("65-74 years", "75+ years") & vax_assignment$num_doses != "unvax" & vax_assignment$immunocompromised == 0)) / length(age_65_plus_index))
  vax_assignment$vax[immunocompromised_index] <- rbinom(length(immunocompromised_index), 1, 0.7 )
  
  print(paste0("Percentage of 18-49 years receiving vaccinated: ", length(which(vax_assignment$age_group == "18-49 years" & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_18_49_index)))
  print(paste0("Percentage of 50-64 years receiving vaccinated: ",  length(which(vax_assignment$age_group == "50-64 years" & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_50_64_index)))
  print(paste0("Percentage of 65+ years receiving vaccinated: ",  length(which(vax_assignment$age_group %in% c("65-74 years", "75+ years") & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(age_65_plus_index)))
  print(paste0("Percentage of immunocompromised receiving vaccinated: ",  length(which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2) & vax_assignment$num_doses != "unvax" & vax_assignment$vax == 1))/length(immunocompromised_index)))
  
  return(vax_assignment)
}
########################################################################################################################
noBoosterSimulation <- function(df){
  
  #Store severe and nonsevere outcome counts in df
  grouped_outcome_counts <- df %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n())
  grouped_outcome_counts[sprintf("week%s",(1:104))] <- NA
  grouped_outcome_counts[sprintf("nonsevere_week%s",(1:104))] <- NA
  
  #Input data (entire pop)
  averted <- df %>% arrange(individual)
  averted[sprintf("week%s",(1))] <- NA
  averted[sprintf("nonsevere_week%s",(1))] <- NA
  
  input <- averted
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  immuno <- input$immunocompromised
  doses <- input$num_doses
  time_since_last <- input$weeks_since_last_dose_inf
  time_since_last_dose <- as.numeric(as.character(input$weeks_since_last_dose))
  lambda <- input$lambda
  contact_matrix_adj_factors <-(data.table(age_group = age)[contact_matrix_adj,
                                                            on=c("age_group"), 
                                                            nomatch = NULL])$contact_matrix_adj
  prob_death <- (data.table(age_group = age)[hosp_death_age_stratified,
                                             on=c("age_group"), 
                                             nomatch = NULL])$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity weeks
  index_recent_infection <- which(inf == 1 & time_since_last < 13 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 14 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  weeks <- c(1:104)
  weekly_infection_by_age <- inf_by_age$total_inf
  
  #Iterate through each time step
  for (i in (1:104)) {
    
    print(i) 
    time_since_last[time_since_last >= 104] <- 104     #Assuming that >24 month waning is same as 24 month waning pe
    
    week <- weeks[time_since_last]
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, week, immuno, doses, lambda, perfect_immunity_counter, death_marker, weekly_infection_by_age, contact_matrix_adj_factors)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]

    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs, 
    #change their prior infection status to 1, time since last to 1, perfect immunity counter to 13
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    inf[index_outcome] <- 1
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 13
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[index_severe_outcome])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Re-update weekly_infection_by_age counter with new infection counts
    weekly_infection_by_age[1] <- sum(severe_outcomes[which(age == "0-17 years")]) + sum(nonsevere_outcomes[which(age == "0-17 years")])
    weekly_infection_by_age[2] <- sum(severe_outcomes[which(age == "18-49 years")]) + sum(nonsevere_outcomes[which(age == "18-49 years")])
    weekly_infection_by_age[3] <- sum(severe_outcomes[which(age == "50-64 years")]) + sum(nonsevere_outcomes[which(age == "50-64 years")])
    weekly_infection_by_age[4] <- sum(severe_outcomes[which(age == "65-74 years")]) + sum(nonsevere_outcomes[which(age == "65-74 years")])
    weekly_infection_by_age[5] <- sum(severe_outcomes[which(age == "75+ years")]) + sum(nonsevere_outcomes[which(age == "75+ years")])
    
    print(weekly_infection_by_age)

    #Add data to dataframe
    input$week1 <- severe_outcomes
    input$nonsevere_week1 <- nonsevere_outcomes
    
    grouped_outcomes <- input %>% group_by(age_group, immunocompromised) %>% summarise(total_severe = sum(week1),
                                                                                       total_nonsevere = sum(nonsevere_week1))
    grouped_outcome_counts[, i + 3] <- grouped_outcomes$total_severe
    grouped_outcome_counts[, i + 107] <- grouped_outcomes$total_nonsevere
    
  }
  
  return(grouped_outcome_counts)
}

oneBoosterSimulation <- function(df){
  #Assign who is getting vaccinated using age-specific coverage rates
  vax_assignment <- df %>% mutate(restrict_vax = 0)
  
  age_65_plus_index <- which((vax_assignment$age_group %in% c("65-74 years","75+ years")) &  vax_assignment$vax == 1)
  immunocompromised_index <- which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2)  & vax_assignment$vax == 1)
  
  vax_assignment$restrict_vax[age_65_plus_index] <- 1
  vax_assignment$restrict_vax[immunocompromised_index] <- 1
  
  #Store severe and nonsevere outcome counts in df
  grouped_outcome_counts <- vax_assignment %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n())
  grouped_outcome_counts[sprintf("week%s",(1:104))] <- NA
  grouped_outcome_counts[sprintf("nonsevere_week%s",(1:104))] <- NA
  
  #Input data (entire pop)
  input <- vax_assignment %>% arrange(individual)
  input[sprintf("week%s",(1))] <- NA
  input[sprintf("nonsevere_week%s",(1))] <- NA

  
    #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  immuno <- input$immunocompromised
  doses <- input$num_doses
  time_since_last <- input$weeks_since_last_dose_inf
  time_since_last_dose <- as.numeric(as.character(input$weeks_since_last_dose))
  lambda <- input$lambda
  contact_matrix_adj_factors <-(data.table(age_group = age)[contact_matrix_adj,
                                                            on=c("age_group"), 
                                                            nomatch = NULL])$contact_matrix_adj
  prob_death <- (data.table(age_group = age)[hosp_death_age_stratified,
                                             on=c("age_group"), 
                                             nomatch = NULL])$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity weeks
  index_recent_infection <- which(inf == 1 & time_since_last < 13 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 14 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  weeks <- c(1:104)
  to_vaccinate_index <- which(input$vax == 1)
  vaccine_wave <- rep(0, nrow(input))
  vaccine_wave[to_vaccinate_index] <- sample(c(1:13), length(to_vaccinate_index), replace = TRUE)
  weekly_infection_by_age <- inf_by_age$total_inf
  
  #Iterate through each time step
  for (i in (1:104)) {
    print(paste0("Week: ", i)) 
    
    #Staggering vaccination over 3-month window
    if(i %in% c(2:14)){
      vaccine_wave_index <- which(vaccine_wave == i-1 & input$vax == 1)
      time_since_last[vaccine_wave_index] <- 1
    } 
    
    time_since_last[time_since_last >= 104] <- 104     #Assuming that >24 month waning is same as 24 month waning pe
    week <- weeks[time_since_last]

    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, week, immuno, doses, lambda, perfect_immunity_counter, death_marker, weekly_infection_by_age, contact_matrix_adj_factors)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    print(paste0("Total weekly severe infections: ", sum(severe_outcomes)))
    print(paste0("Total weekly nonsevere infections: ", sum(nonsevere_outcomes)))
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs, 
    #change their prior infection status to 1, time since last to 1, perfect immunity counter to 13
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    inf[index_outcome] <- 1
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 13
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[index_severe_outcome])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Re-update weekly_infection_by_age counter with new infection counts
    weekly_infection_by_age[1] <- sum(severe_outcomes[which(age == "0-17 years")]) + sum(nonsevere_outcomes[which(age == "0-17 years")])
    weekly_infection_by_age[2] <- sum(severe_outcomes[which(age == "18-49 years")]) + sum(nonsevere_outcomes[which(age == "18-49 years")])
    weekly_infection_by_age[3] <- sum(severe_outcomes[which(age == "50-64 years")]) + sum(nonsevere_outcomes[which(age == "50-64 years")])
    weekly_infection_by_age[4] <- sum(severe_outcomes[which(age == "65-74 years")]) + sum(nonsevere_outcomes[which(age == "65-74 years")])
    weekly_infection_by_age[5] <- sum(severe_outcomes[which(age == "75+ years")]) + sum(nonsevere_outcomes[which(age == "75+ years")])
    
    print(weekly_infection_by_age)
    
    #Add data to dataframe
    input$week1 <- severe_outcomes
    input$nonsevere_week1 <- nonsevere_outcomes
    
    grouped_outcomes <- input %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_severe = sum(week1),
                                                                                                            total_nonsevere = sum(nonsevere_week1))
    grouped_outcome_counts[, i + 3] <- grouped_outcomes$total_severe
    grouped_outcome_counts[, i + 107] <- grouped_outcomes$total_nonsevere
    
  }
  
  return(grouped_outcome_counts)
}

annualBoosterSimulation <- function(df){
  #Assign who is getting vaccinated using age-specific coverage rates
  vax_assignment <- df %>% mutate(restrict_vax = 0)
  
  age_65_plus_index <- which((vax_assignment$age_group %in% c("65-74 years","75+ years")) &  vax_assignment$vax == 1)
  immunocompromised_index <- which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2)  & vax_assignment$vax == 1)
  
  vax_assignment$restrict_vax[age_65_plus_index] <- 1
  vax_assignment$restrict_vax[immunocompromised_index] <- 1
  
  #Store severe and nonsevere outcome counts in df
  grouped_outcome_counts <- vax_assignment %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n())
  grouped_outcome_counts[sprintf("week%s",(1:104))] <- NA
  grouped_outcome_counts[sprintf("nonsevere_week%s",(1:104))] <- NA
  
  #Input data (entire pop)
  input <- vax_assignment %>% arrange(individual)
  input[sprintf("week%s",(1))] <- NA
  input[sprintf("nonsevere_week%s",(1))] <- NA
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  immuno <- input$immunocompromised
  doses <- input$num_doses
  time_since_last <- input$weeks_since_last_dose_inf
  time_since_last_dose <- as.numeric(as.character(input$weeks_since_last_dose))
  lambda <- input$lambda
  contact_matrix_adj_factors <-(data.table(age_group = age)[contact_matrix_adj,
                                                            on=c("age_group"), 
                                                            nomatch = NULL])$contact_matrix_adj
  prob_death <- (data.table(age_group = age)[hosp_death_age_stratified,
                                             on=c("age_group"), 
                                             nomatch = NULL])$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity weeks
  index_recent_infection <- which(inf == 1 & time_since_last < 13 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 14 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  weeks <- c(1:104)
  to_vaccinate_index <- which(input$vax == 1)
  vaccine_wave <- rep(0, nrow(input))
  vaccine_wave[to_vaccinate_index] <- sample(c(1:13), length(to_vaccinate_index), replace = TRUE)
    weekly_infection_by_age <- inf_by_age$total_inf
  
  #Iterate through each time step
  for (i in (1:104)) {
    print(paste0("Week: ", i)) 
    
    #Staggering vaccination over 3-month window (13 weeks)
    if(i %in% c(2:14)){
      vaccine_wave_index <- which((vaccine_wave == i - 1) & input$vax == 1)
      time_since_last[vaccine_wave_index] <- 1
    } 
    
    if(i %in% c(53:65)){
      vaccine_wave_index <- which((vaccine_wave == i - 52) & input$restrict_vax == 1)
      time_since_last[vaccine_wave_index] <- 1
    } 
    
    
    time_since_last[time_since_last >= 104] <- 104     #Assuming that >24 month waning is same as 24 month waning pe
    week <- weeks[time_since_last]
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, week, immuno, doses, lambda, perfect_immunity_counter, death_marker, weekly_infection_by_age, contact_matrix_adj_factors)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    print(paste0("Total weekly severe infections: ", sum(severe_outcomes)))
    print(paste0("Total weekly nonsevere infections: ", sum(nonsevere_outcomes)))
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs, 
    #change their prior infection status to 1, time since last to 1, perfect immunity counter to 13
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    inf[index_outcome] <- 1
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 13
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[index_severe_outcome])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Re-update weekly_infection_by_age counter with new infection counts
    weekly_infection_by_age[1] <- sum(severe_outcomes[which(age == "0-17 years")]) + sum(nonsevere_outcomes[which(age == "0-17 years")])
    weekly_infection_by_age[2] <- sum(severe_outcomes[which(age == "18-49 years")]) + sum(nonsevere_outcomes[which(age == "18-49 years")])
    weekly_infection_by_age[3] <- sum(severe_outcomes[which(age == "50-64 years")]) + sum(nonsevere_outcomes[which(age == "50-64 years")])
    weekly_infection_by_age[4] <- sum(severe_outcomes[which(age == "65-74 years")]) + sum(nonsevere_outcomes[which(age == "65-74 years")])
    weekly_infection_by_age[5] <- sum(severe_outcomes[which(age == "75+ years")]) + sum(nonsevere_outcomes[which(age == "75+ years")])
    
    
    #Add data to dataframe
    input$week1 <- severe_outcomes
    input$nonsevere_week1 <- nonsevere_outcomes
    
    grouped_outcomes <- input %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_severe = sum(week1),
                                                                                                            total_nonsevere = sum(nonsevere_week1))
    grouped_outcome_counts[, i + 3] <- grouped_outcomes$total_severe
    grouped_outcome_counts[, i + 107] <- grouped_outcomes$total_nonsevere
    
  }
  
  return(grouped_outcome_counts)
}

biannualBoosterSimulation <- function(df){
  #Assign who is getting vaccinated using age-specific coverage rates
  vax_assignment <- df %>% mutate(restrict_vax = 0)
  
  age_65_plus_index <- which((vax_assignment$age_group %in% c("65-74 years","75+ years")) &  vax_assignment$vax == 1)
  immunocompromised_index <- which(vax_assignment$age_group != "0-17 years" & vax_assignment$immunocompromised %in% c(1, 2)  & vax_assignment$vax == 1)
  
  vax_assignment$restrict_vax[age_65_plus_index] <- 1
  vax_assignment$restrict_vax[immunocompromised_index] <- 1
  
  #Store severe and nonsevere outcome counts in df
  grouped_outcome_counts <- vax_assignment %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_pop = n())
  grouped_outcome_counts[sprintf("week%s",(1:104))] <- NA
  grouped_outcome_counts[sprintf("nonsevere_week%s",(1:104))] <- NA
  
  #Input data (entire pop)
  input <- vax_assignment %>% arrange(individual)
  input[sprintf("week%s",(1))] <- NA
  input[sprintf("nonsevere_week%s",(1))] <- NA
  
  #Population's info (age_group, num_doses, prior_inf, etc.) at each timestep
  age <- as.character(input$age_group)
  doses <- as.character(input$num_doses)
  inf <- input$prior_inf
  immuno <- input$immunocompromised
  doses <- input$num_doses
  time_since_last <- input$weeks_since_last_dose_inf
  time_since_last_dose <- as.numeric(as.character(input$weeks_since_last_dose))
  lambda <- input$lambda
  contact_matrix_adj_factors <-(data.table(age_group = age)[contact_matrix_adj,
                                                            on=c("age_group"), 
                                                            nomatch = NULL])$contact_matrix_adj
  prob_death <- (data.table(age_group = age)[hosp_death_age_stratified,
                                             on=c("age_group"), 
                                             nomatch = NULL])$perc_death
  perfect_immunity_counter <- rep(0,nrow(input)) #If non-death infection occurs, counting down perfect immunity weeks
  index_recent_infection <- which(inf == 1 & time_since_last < 13 & time_since_last < time_since_last_dose) #Individuals infected in 3 months preceding start of sim have perfect immunity at start
  perfect_immunity_counter[index_recent_infection] <- 14 - time_since_last[index_recent_infection] #REVISIT
  death_marker <- rep(0,nrow(input)) #If death occurs
  hosp_count <- rep(0, nrow(input))
  death_count <- rep(0, nrow(input))
  weeks <- c(1:104)
  to_vaccinate_index <- which(input$vax == 1)
  vaccine_wave <- rep(0, nrow(input))
  vaccine_wave[to_vaccinate_index] <- sample(c(1:13), length(to_vaccinate_index), replace = TRUE)
  weekly_infection_by_age <- inf_by_age$total_inf
  
  #Iterate through each time step
  for (i in (1:104)) {
    print(paste0("Week: ", i)) 
    
    #Staggering vaccination over 3-month window
    if(i %in% c(2:14)){
      vaccine_wave_index <- which((vaccine_wave == i - 1) & input$vax == 1)
      time_since_last[vaccine_wave_index] <- 1
    }  
    
    if(i %in% c(27:39, 53:65, 79:91)){
      vaccine_wave_index <- which((vaccine_wave == i - 26 | vaccine_wave == i - 52 | vaccine_wave == i - 78) & input$restrict_vax == 1)
      time_since_last[vaccine_wave_index] <- 1
    }  
    
    
    time_since_last[time_since_last >= 104] <- 104     #Assuming that >24 month waning is same as 24 month waning pe
    week <- weeks[time_since_last]
    
    #Do outcomes occur?
    outcomes <- outcome_occurrence(age, inf, week, immuno, doses, lambda, perfect_immunity_counter, death_marker, weekly_infection_by_age, contact_matrix_adj_factors)
    severe_outcomes <- outcomes[[1]]
    nonsevere_outcomes <- outcomes[[2]]
    print(paste0("Total weekly severe infections: ", sum(severe_outcomes)))
    print(paste0("Total weekly nonsevere infections: ", sum(nonsevere_outcomes)))
    
    #If no outcome occurs, increase time since last
    index_no_outcome <- which(severe_outcomes == 0 & nonsevere_outcomes == 0)
    time_since_last[index_no_outcome] <- time_since_last[index_no_outcome] + 1
    
    #Decrease 1 from perfect immunity counter (if applicable)
    perfect_immunity_counter[perfect_immunity_counter > 0] <- perfect_immunity_counter[perfect_immunity_counter > 0] - 1
    
    #If outcome occurs, 
    #change their prior infection status to 1, time since last to 1, perfect immunity counter to 13
    index_outcome <- which(severe_outcomes == 1 | nonsevere_outcomes == 1)
    inf[index_outcome] <- 1
    time_since_last[index_outcome] <- 1
    perfect_immunity_counter[index_outcome] <- 13
    
    #Then check if severe outcome is hosp vs. death
    index_severe_outcome <- which(severe_outcomes == 1)
    death_ind <- rbinom(length(index_severe_outcome), 1, prob_death[index_severe_outcome])
    
    #If death, cut simulation for individual (death marker)
    death_ind_index <- index_severe_outcome[which(death_ind == 1)]
    death_marker[death_ind_index] <- 1
    death_count[death_ind_index] <- death_count[death_ind_index] + 1
    
    hosp_ind_index <- index_severe_outcome[which(death_ind == 0)]
    hosp_count[hosp_ind_index] <- hosp_count[hosp_ind_index] + 1
    
    #If both severe outcome and nonsevere outcome occur in same individual, remove nonsevere outcome
    index_both_outcome <- which(severe_outcomes == 1 & nonsevere_outcomes == 1)
    nonsevere_outcomes[index_both_outcome] <- 0
    
    #Re-update weekly_infection_by_age counter with new infection counts
    weekly_infection_by_age[1] <- sum(severe_outcomes[which(age == "0-17 years")]) + sum(nonsevere_outcomes[which(age == "0-17 years")])
    weekly_infection_by_age[2] <- sum(severe_outcomes[which(age == "18-49 years")]) + sum(nonsevere_outcomes[which(age == "18-49 years")])
    weekly_infection_by_age[3] <- sum(severe_outcomes[which(age == "50-64 years")]) + sum(nonsevere_outcomes[which(age == "50-64 years")])
    weekly_infection_by_age[4] <- sum(severe_outcomes[which(age == "65-74 years")]) + sum(nonsevere_outcomes[which(age == "65-74 years")])
    weekly_infection_by_age[5] <- sum(severe_outcomes[which(age == "75+ years")]) + sum(nonsevere_outcomes[which(age == "75+ years")])
    
    
    #Add data to dataframe
    input$week1 <- severe_outcomes
    input$nonsevere_week1 <- nonsevere_outcomes
    
    grouped_outcomes <- input %>% filter(vax == 1) %>% group_by(age_group, immunocompromised) %>% summarise(total_severe = sum(week1),
                                                                                                            total_nonsevere = sum(nonsevere_week1))
    grouped_outcome_counts[, i + 3] <- grouped_outcomes$total_severe
    grouped_outcome_counts[, i + 107] <- grouped_outcomes$total_nonsevere
    
  }
  
  return(grouped_outcome_counts)
}

########################################################################################

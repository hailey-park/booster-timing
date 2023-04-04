install.packages("BiocManager")
require(BiocManager)
install("BiocParallel")
require(BiocParallel)

bpparam <- SnowParam(workers = 7, type = 'SOCK')

FUN <- function(i){
  #Individual's info (age_group, num_doses, prior_inf, etc.)
  individual <- tester[i,]
  age <- as.character(individual$age_group)
  doses <- as.character(individual$num_doses)
  inf <- individual$prior_inf
  time_since_last <- 0 #start at 0 because of bivalent booster @ beginning of simulation
  lambda <- individual$lambda
  months_list = c(0.5, 1:24)
  
  for(j in (1:25)) {
    #Does outcome occur?
    outcome <- severe_outcome(age, inf, months_list[time_since_last + 1], lambda)
    
    if(outcome == 1) { 
      total_outcomes[j] <- 1
      
      #Change prior infection status to 1
      inf <- 1
      
      #is outcome hosp or death?
      death_ind <- rbinom(1,1,prob_death)
      
      if(death_ind == 1) {
        death_count <- death_count + 1
        break
      }else{
        hosp_count <- hosp_count + 1
        time_since_last <- 0
        next
      }
    }
    #No severe outcome occurs (outcome == 0)
    else {time_since_last <- time_since_last + 1}
  }
  
  tester[i, 7:31] <- total_outcomes
  tester[i, 'total_hosps'] <- hosp_count
  tester[i, 'total_deaths'] <- death_count
  tester[i,,drop = FALSE]
}
  
tester <- averted_age_75_plus[1:100, ]

system.time( {
  output_test <- bplapply(1:nrow(tester), FUN, BPPARAM = bpparam)
  
})

system.time( {
  output_test <- foreach(i = 1:nrow(tester), .combine=rbind, .packages = 'dplyr') %dopar% {
    
    #Individual's info (age_group, num_doses, prior_inf, etc.)
    individual <- tester[i,]
    age <- as.character(individual$age_group)
    doses <- as.character(individual$num_doses)
    inf <- individual$prior_inf
    time_since_last <- 0 #start at 0 because of bivalent booster @ beginning of simulation
    lambda <- individual$lambda
    months_list = c(0.5, 1:24)
    
    for(j in (1:25)) {
      #Does outcome occur?
      outcome <- severe_outcome(age, inf, months_list[time_since_last + 1], lambda)
      
      if(outcome == 1) { 
        total_outcomes[j] <- 1
        
        #Change prior infection status to 1
        inf <- 1
        
        #is outcome hosp or death?
        death_ind <- rbinom(1,1,prob_death)
        
        if(death_ind == 1) {
          death_count <- death_count + 1
          break
        }else{
          hosp_count <- hosp_count + 1
          time_since_last <- 0
          next
        }
      }
      #No severe outcome occurs (outcome == 0)
      else {time_since_last <- time_since_last + 1}
    }
    
    tester[i, 7:31] <- total_outcomes
    tester[i, 'total_hosps'] <- hosp_count
    tester[i, 'total_deaths'] <- death_count
    tester[i,,drop = FALSE]
  }
})





#total outcomes
# results <- data.frame(months = c(0.5, 1:24),
#                       prior_inf = inf) %>% rowwise() %>%
#   mutate(outcome = severe_outcome(age,prior_inf,months,lambda))%>% ungroup() %>%
#   mutate(prior_inf = replace(prior_inf, cumany(outcome == 1), 1)) %>% rowwise() %>%
#   mutate(outcome_adj = ifelse(prior_inf == 1, severe_outcome(age,prior_inf,months,lambda), outcome))

setwd(here::here())

###################################################################################################
#Plotting Protection over time


#Loop through each age group and create immunocompetent plots
plot_list = list()
age_groups <- c("18-49 years", "50-64 years", "65-74 years", "75+ years")

for (i in 1:4) {
  age <- age_groups[i]
  
  lambda <- read.csv(file=paste0("results/calibration/main/immunocompetent/waning-mean/sero-mean/1mil-", age, "-monthly.csv"),nrows=1)$lambda
  
  age_data_severe <- read.csv("results/waning-predictions/severe_waning_predictions_monthly.csv")[,-1] %>% filter(age_group == age) %>%
    mutate(lambda = lambda,
           risk = (1 - ve_pred) * lambda) %>%
    mutate(group = if_else(prior_inf == 1, paste0('Prior Infection; \nImmunocompetent'), paste0('No Prior Infection; \nImmunocompetent')))
  
  plot_data <- age_data_severe %>% dplyr::select(group, estimate, risk, months) %>% 
    spread(estimate, risk)
  
  p <- ggplot(plot_data, aes(months)) + 
    geom_line(aes(y=mean , color = group), size = 1) + 
    geom_ribbon(aes(ymin=lower, ymax=upper, fill = group), alpha=0.4) +
    xlab("Time (months)") + 
    ylab(paste0("Monthly Risk of Severe COVID-19")) +
    ggtitle(paste0("Age Group: ", age))+
    labs(color='Group') +
    guides(fill = "none") +
    ylim(0,0.005)+
    scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size=12))
  
  plot_list[[i]] <- p
}

#Create risk plots for immunocompromised (mild + moderate/severe) groups (These are age-weighted)

#IMMUNOCOMPROMISED (MILD)
severe_waning_data <- read.csv("results/waning-predictions/severe_waning_predictions_monthly_immunoMild.csv")[,-1] %>%
  mutate(lambda = case_when(age_group == "18-49 years" ~ read.csv(file=paste0("results/calibration/main/immunoMild/waning-mean/sero-mean/1mil-18-49 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "50-64 years" ~ read.csv(file=paste0("results/calibration/main/immunoMild/waning-mean/sero-mean/1mil-50-64 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "65-74 years" ~ read.csv(file=paste0("results/calibration/main/immunoMild/waning-mean/sero-mean/1mil-65-74 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "75+ years" ~ read.csv(file=paste0("results/calibration/main/immunoMild/waning-mean/sero-mean/1mil-75+ years-monthly.csv"),nrows=1)$lambda,
                            TRUE ~ 0),
         risk = (1 - ve_pred) * lambda,
         weight = case_when(age_group == "18-49 years" ~ 0.365805431, #These are age-weights
                            age_group == "50-64 years" ~ 0.283233968,
                            age_group == "65-74 years" ~ 0.155565424,
                            age_group == "75+ years" ~ 0.195395177,
                            TRUE ~ 0)) %>%
  dplyr::select(months, estimate, prior_inf, age_group, weight, risk) %>%
  group_by(months, estimate, prior_inf) %>% summarise(risk = weighted.mean(risk, weight)) %>%
  mutate(group = if_else(prior_inf == 1, paste0('Prior Infection; \nImmunocompromised (Mild)'), paste0('No Prior Infection; \nImmunocompromised (Mild)')))

plot_data <- severe_waning_data %>% dplyr::select(group, estimate, risk, months) %>% 
  spread(estimate, risk) 

plot_list[[5]] <- ggplot(plot_data, aes(months)) + 
  geom_line(aes(y=mean , color = group), size = 1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = group), alpha=0.4) +
  xlab("Time (months)") + 
  ylab("Monthly Risk of Severe COVID-19") +
  ggtitle("Risk Group: Immunocompromised (Mild)") +
  labs(color='Group') +
  guides(fill = "none") +
  ylim(0,0.005)+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12))

#IMMUNOCOMPROMISED (SEVERE)
severe_waning_data <- read.csv("results/waning-predictions/severe_waning_predictions_monthly_immunoSevere.csv")[,-1] %>%
  mutate(lambda = case_when(age_group == "18-49 years" ~ read.csv(file=paste0("results/calibration/main/immunoSevere/waning-mean/sero-mean/1mil-18-49 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "50-64 years" ~ read.csv(file=paste0("results/calibration/main/immunoSevere/waning-mean/sero-mean/1mil-50-64 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "65-74 years" ~ read.csv(file=paste0("results/calibration/main/immunoSevere/waning-mean/sero-mean/1mil-65-74 years-monthly.csv"),nrows=1)$lambda,
                            age_group == "75+ years" ~ read.csv(file=paste0("results/calibration/main/immunoSevere/waning-mean/sero-mean/1mil-75+ years-monthly.csv"),nrows=1)$lambda,
                            TRUE ~ 0),
         risk = (1 - ve_pred) * lambda,
         weight = case_when(age_group == "18-49 years" ~ 0.365805431, #These are age-weights
                            age_group == "50-64 years" ~ 0.283233968,
                            age_group == "65-74 years" ~ 0.155565424,
                            age_group == "75+ years" ~ 0.195395177,
                            TRUE ~ 0)) %>%
  dplyr::select(months, estimate, prior_inf, age_group, weight, risk) %>%
  group_by(months, estimate, prior_inf) %>% summarise(risk = weighted.mean(risk, weight)) %>%
  mutate(group = if_else(prior_inf == 1, paste0('Prior Infection; \nImmunocompromised (Severe)'), paste0('No Prior Infection; \nImmunocompromised (Severe)')))

plot_data <- severe_waning_data %>% dplyr::select(group, estimate, risk, months) %>% 
  spread(estimate, risk) 

plot_list[[6]] <- ggplot(plot_data, aes(months)) + 
  geom_line(aes(y=mean , color = group), size = 1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = group), alpha=0.4) +
  xlab("Time (months)") + 
  ylab("Monthly Risk of Severe COVID-19") +
  ggtitle("Risk Group: Immunocompromised (Moderate/Severe)") +
  labs(color='Group') +
  guides(fill = "none") +
  ylim(0,0.005)+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12))

pdf("figures/supplementary/severe-risk-over-time.pdf")
for (i in 1:6) {
  print(plot_list[[i]])
}
dev.off()


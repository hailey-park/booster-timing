
###################################################################################################
#Getting Calibrated Lambdas
age_group <- "75+ years"
waning_lower_sero_lower <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-lower/sero-lower/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_lower_sero_mean <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-lower/sero-mean/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_lower_sero_upper <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-lower/sero-upper/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_mean_sero_lower <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-mean/sero-lower/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_mean_sero_mean <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-mean/sero-mean/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_mean_sero_upper <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-mean/sero-upper/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_upper_sero_lower <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-upper/sero-lower/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_upper_sero_mean <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-upper/sero-mean/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda
waning_upper_sero_upper <- read.csv(file=paste0("calibration/95ui/immuno-severe/waning-upper/sero-upper/1mil-", age_group, "-monthly.csv"),nrows=1)$lambda

min(waning_lower_sero_upper, waning_lower_sero_mean, waning_lower_sero_lower,
    waning_mean_sero_upper, waning_mean_sero_mean, waning_mean_sero_lower,
    waning_upper_sero_upper, waning_upper_sero_mean, waning_upper_sero_lower)

max(waning_lower_sero_upper, waning_lower_sero_mean, waning_lower_sero_lower,
    waning_mean_sero_upper, waning_mean_sero_mean, waning_mean_sero_lower,
    waning_upper_sero_upper, waning_upper_sero_mean, waning_upper_sero_lower)

waning_mean_sero_mean
###################################################################################################
#Plotting Protection over time
severe_waning_data <- read.csv("ve model results/severe_waning_predictions_monthly_immunoSevere.csv")[,-1] %>%
  mutate(lambda = case_when(age_group == "18-49 years" ~ 0.0008777705, #0.001020913, #0.0007333639,
                            age_group == "50-64 years" ~ 0.001558905,# 0.001744473, #0.001111062,
                            age_group == "65-74 years" ~ 0.003505925, #0.003875088, #0.002255153,
                            age_group == "75+ years" ~ 0.00966355, #0.010681, #0.006216297,
                            TRUE ~ 0),
         risk = (1 - ve_pred) * lambda,
         weight = case_when(age_group == "18-49 years" ~ 0.365805431,
                            age_group == "50-64 years" ~ 0.283233968,
                            age_group == "65-74 years" ~ 0.155565424,
                            age_group == "75+ years" ~ 0.195395177,
                            TRUE ~ 0)) %>%
  select(months, estimate, prior_inf, age_group, weight, risk) %>%
  group_by(months, estimate, prior_inf) %>% summarise(risk = weighted.mean(risk, weight)) %>%
  mutate(group = if_else(prior_inf == 1, paste0('Prior Infection; \nImmunocompromised (Severe)'), paste0('No Prior Infection; \nImmunocompromised (Severe)')))

plot_data <- severe_waning_data %>% select(group, estimate, risk, months) %>% 
  spread(estimate, risk) %>%
  filter(age_group == "18-49 years")

ggplot(plot_data, aes(months)) + 
  geom_line(aes(y=mean , color = group), size = 1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = group), alpha=0.4) +
  xlab("Time (months)") + 
  ylab("Monthly Risk of Severe COVID-19") +
  labs(color='Group') +
  guides(fill = "none") +
  ylim(0,0.005)+
  #ylim(0,160) +
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12))#+


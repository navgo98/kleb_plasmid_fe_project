alpha = 1
pop_init = rep(0,10)

t_end = 50

step <- c()
t_step <- rep(0,100)
t = rep(0,10)
pop_step <- rep(0,100)


for (i in 1:100) {
  wt <- rexp(100, rate = alpha)
  t_step[i+1] <- t_step[i] + wt[i]
  pop_step[i+1] <- pop_step[i] + 1 
  if (t_step[i] >= t_end) break 
  
  print(t_final <- t_step[1:(i-1)])
  print(pop_final <- pop_step[1:(i-1)])
 
}
plot(t_final, pop_final, type ="l", col = "red")


#death and birth process
pop_init = rep(10,100)

t_end = 10

t_step <- rep(0,100)

pop_step <- rep(0,100)

rate_b = 1
rate_d = 0.6


total_rate = (rate_b + rate_d)*pop_init
rand_unifrom <- runif(100, min = 0 , max = 1)

for (i in 1:100) {
  wt <- rexp(100, rate = total_rate)
  t_step[i+1] <- t_step[i] + wt[i]
  
  if (rand_unifrom[i] < rate_b/(rate_b+rate_d)) {
  pop_step[i+1] <- pop_step[i] + 1
  }
  else {
    pop_step[i+1] <- pop_step[i] - 1
  }
  
  if (t_step[i] >= t_end) break 
  
  print(t_final <- t_step[1:(i-1)])
  print(pop_final <- pop_step[1:(i-1)])
  
}

plot(t_final, pop_final, type ="l", col = "red")



install.packages("sjPlot")
library(sjPlot)
#test 
boxplot(GrowthRate ~ Environment*Strain, col =  c("white","lightgray"), 
        data = replication_m9_fe_complete)

tab_model(fit_lme_growth)

tab_model(fit_lme_lag)

tab_model(fit_lme_maxcap)

emmeans(fit_lme_growth, ~ Strain : Environment)

emmeans(fit_lme_growth, ~ Strain | Environment)

#Max Cap
fit_lme_maxcap <- lme(MaxCap ~ Strain*Environment, random = ~1 | Replicate, data = replication_data)
tab_model(fit_lme_maxcap)

chi_yen_replicate$maxcap_wls <- 1 / (chi_yen_replicate$SE_m^2) 

fit_lme_weighted_maxcap <- lme(MaxCap ~ Strain*Environment,
                            random = ~ 1 | Replicate,
                            weights = ~I(1 / maxcap_wls),
                            data = chi_yen_replicate)

tab_model(fit_lme_weighted_maxcap)

sjPlot::tab_model(fit_lme_weighted_maxcap, file = "maxcap_regression_results.html")
plot(resid(fit_lme_weighted_maxcap))

emm_maxcap <- emmeans(fit_lme_weighted_maxcap, ~Strain | Environment)


dunnett_maxcap <- contrast(
  emm_maxcap,
  method = "trt.vs.ctrl",
  ref = "W",
  adjust = "dunnett"
)

dunnett_maxcap_df <- as.data.frame(dunnett_maxcap)
dunnett_maxcap_df$stars <- cut(
  dunnett_maxcap_df$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)

emm_df_maxcap <- as.data.frame(emm_maxcap)

# Prepare keys for merging
dunnett_maxcap_df$Strain <- dunnett_maxcap_df$contrast
dunnett_maxcap_df$Environment <- dunnett_maxcap_df$Environment

# Clean contrast names: extract treatment strain ONLY
dunnett_maxcap_df$Strain <- sub(" - W", "", dunnett_maxcap_df$contrast)

plot_df_maxcap <- left_join(
  emm_df_maxcap,
  dunnett_maxcap_df[, c("Strain", "Environment", "stars")],
  by = c("Strain", "Environment")
)


ggplot(plot_df_maxcap, aes(x = Strain, y = emmean)) +
  geom_col(fill = "goldenrod1", width = 0.5) +
  geom_jitter(
    data = chi_yen_replicate,  # your replicate-level parameters
    aes(x = Strain, y = MaxCap, color = "violetred2"),
    position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.6),
    alpha = 0.7, size = 2
  )  +
  #geom_errorbar(data = ci_m9_fe_average_complete[b_seq_average_complete,], 
  #aes(x= Strain, y = GrowthRate, color = Environemnt, ymin = lower, ymax = upper), 
  #width = 0.2,  
  #linewidth = 0.8,  
  #color = "red", 
  #position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                color = "red", linewidth = 0.8) +
  geom_text(aes(label = stars, y = emmean + SE + .2), size = 6) +
  facet_wrap(~ factor(Environment, levels = c("M9", "Fe")), scales = "free_y") +
  scale_y_continuous(limits = c(0, 1.5))+
  labs(
    y = "Estimated Maximum Capacity",
    x = "Strain",
    title = "Maximum Capacity Estimates with Dunnett Post-hoc Tests"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  )+
  theme(legend.position = "none")

excel.

#LAG DURATION
fit_lme_lag <- lme(Lag ~ Strain*Environment, random = ~1 | Replicate, data = replication_data)

tab_model(fit_lme_lag)

chi_yen_replicate$lag_wls <- 1 / (chi_yen_replicate$SE_t1^2) 

fit_lme_weighted_lag <- lme(Lag ~ Strain*Environment,
                               random = ~ 1 | Replicate,
                               weights = ~I(1 / lag_wls),
                               data = chi_yen_replicate)

tab_model(fit_lme_weighted_lag)
sjPlot::tab_model(fit_lme_weighted_lag, file = "lag_regression_results.html")
plot(resid(fit_lme_weighted_lag))

emm_lag<- emmeans(fit_lme_weighted_lag, ~Strain | Environment)


dunnett_lag <- contrast(
  emm_lag,
  method = "trt.vs.ctrl",
  ref = "W",
  adjust = "dunnett"
)

dunnett_lag_df <- as.data.frame(dunnett_lag)
dunnett_lag_df$stars <- cut(
  dunnett_lag_df$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)


emm_df_lag <- as.data.frame(emm_lag)

# Prepare keys for merging
dunnett_lag_df$Strain <- dunnett_lag_df$contrast
dunnett_lag_df$Environment <- dunnett_lag_df$Environment

# Clean contrast names: extract treatment strain ONLY
dunnett_lag_df$Strain <- sub(" - W", "", dunnett_lag_df$contrast)

plot_df_lag <- left_join(
  emm_df_lag,
  dunnett_lag_df[, c("Strain", "Environment", "stars")],
  by = c("Strain", "Environment")
)


ggplot(plot_df_lag, aes(x = Strain, y = emmean)) +
  geom_col(fill = "seagreen1", width = 0.5) +
  geom_jitter(
    data = chi_yen_replicate,  # your replicate-level parameters
    aes(x = Strain, y = Lag, color = "violetred2"),
    position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.6),
    alpha = 0.7, size = 2
  )  +
  #geom_errorbar(data = ci_m9_fe_average_complete[b_seq_average_complete,], 
  #aes(x= Strain, y = GrowthRate, color = Environemnt, ymin = lower, ymax = upper), 
  #width = 0.2,  
  #linewidth = 0.8,  
  #color = "red", 
  #position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                color = "red", linewidth = 0.8) +
  geom_text(aes(label = stars, y = emmean + SE + 0.2), size = 6) +
  facet_wrap(~ factor(Environment, levels = c("M9", "Fe")), scales = "free_y") +
  scale_y_continuous(limits = c(0, 4))+
  labs(
    y = "Estimated Lag Duration",
    x = "Strain",
    title = "Lag Duration Estimates with Dunnett Post-hoc Tests"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  )+
  theme(legend.position = "none")
#geom_jitter(
#data = chi_yen_replicate,  # your replicate-level parameters
#aes(x = Strain, y = GrowthRate, color = Environment),
#position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
#alpha = 0.7, size = 2
#)  



#GROWTH RATE
chi_yen_replicate$growthrate_wls <- 1 / (chi_yen_replicate$SE_b^2) 

fit_lme_weighted_growth <- lme(GrowthRate ~ Strain*Environment,
  random = ~ 1 | Replicate,
  weights = ~I(1 / growthrate_wls),
  data = chi_yen_replicate)

tab_model(fit_lme_weighted_growth)
sjPlot::tab_model(fit_lme_weighted_growth, file = "growth_rate_regression_results.html")

summary(fit_lme_weighted_growth)
plot(residuals(fit_lme_weighted_growth))

anova(fit_lme_weighted_growth, type = "III")

emm_growth<- emmeans(fit_lme_weighted_growth, ~Strain | Environment)


dunnett_growth <- contrast(
  emm_growth,
  method = "trt.vs.ctrl",
  ref = "W",
  adjust = "dunnett"
)

dunnett_growth_df <- as.data.frame(dunnett_growth)
dunnett_growth_df$stars <- cut(
  dunnett_growth_df$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)


emm_df_growth <- as.data.frame(emm_growth)

# Prepare keys for merging
dunnett_growth_df$Strain <- dunnett_growth_df$contrast
dunnett_growth_df$Environment <- dunnett_growth_df$Environment

# Clean contrast names: extract treatment strain ONLY
dunnett_growth_df$Strain <- sub(" - W", "", dunnett_growth_df$contrast)

# Add environment, because Dunnett output usually lacks it
#dunnett_growth_df$Environment <- emm_df_growth$Environment[match(dunnett_growth_df$Strain,
                                                                 #emm_df_growth$Strain)]

# Keep only columns needed for plotting
#dunnett_growth_df_clean <- dunnett_growth_df[, c("Strain", "Environment", "p.value")]

# Create significance stars
dunnett_growth_df_clean$stars <- cut(
  dunnett_growth_df_clean$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)



plot_df <- left_join(
  emm_df_growth,
  dunnett_growth_df[, c("Strain", "Environment", "stars")],
  by = c("Strain", "Environment")
)

plot_df$average_estimate <- ci_m9_fe_average_complete[b_seq_average_complete,][,2]

library(ggplot2)

ggplot(plot_df, aes(x = Strain, y = emmean)) +
  geom_col(fill = "lightskyblue", width = 0.7) +
  geom_jitter(
    data = chi_yen_replicate,  # your replicate-level parameters
    aes(x = Strain, y = GrowthRate, color = "violetred2"),
    position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.6),
    alpha = 0.7, size = 2
  )  +
  #geom_errorbar(data = ci_m9_fe_average_complete[b_seq_average_complete,], 
                #aes(x= Strain, y = GrowthRate, color = Environemnt, ymin = lower, ymax = upper), 
                #width = 0.2,  
                #linewidth = 0.8,  
                #color = "red", 
                #position = position_dodge(width = 0.5)) + 
  #geom_errorbar(aes(ymin = emmean - 1.96*SE, ymax = emmean + 1.96*SE), width = 0.2,
                #color = "red", linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                color = "red", linewidth = 0.8) +
  geom_text(aes(label = stars, y = emmean + SE + 0.1), size = 6) +
  facet_wrap(~ factor(Environment, levels = c("M9", "Fe")), scales = "free_y") +
  scale_y_continuous(limits = c(0, 0.7))+
  labs(
    y = "Estimated Growth Rate",
    x = "Strain",
    title = "Growth Rate Estimates with Dunnett Post-hoc Tests"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  )+
  theme(legend.position = "none")
#geom_jitter(
  #data = chi_yen_replicate,  # your replicate-level parameters
  #aes(x = Strain, y = GrowthRate, color = Environment),
  #position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #alpha = 0.7, size = 2
#)  


# ggplot(ci_m9_fe_average_complete[b_seq_average_complete,], aes(x = Strain, y = estimates)) +
#   geom_point(size = 4, color = "blue") +  
#   geom_errorbar(aes(ymin = lower, ymax = upper), 
#                 width = 0.2,  
#                 linewidth = 0.8,  
#                 color = "red", 
#                 position = position_dodge(width = 0.5)) +  
#   labs(title = "Growth Rate (b) Estimates without Fe",
#        x = "Strains",
#        y = "Estimate") +
#   theme_minimal() +
#   ylim(0.3,0.6)

#MAXIMUM CAPACITY


ci_m9_complete$Environment <- "M9"
ci_fe_complete$Environment <- "Fe"

ci_m9_fe_average_complete <- rbind(ci_m9_complete,ci_fe_complete)
colnames(ci_m9_fe_average_complete)[4] <- "Strain"
colnames(ci_m9_fe_average_complete)[2] <- "GrowthRate"
b_seq_average_complete <- seq(1,30,3)


unique(emm_df_growth[, c("Strain", "Environment")])
unique(dunnett_growth_df[, c("Strain", "Environment")])

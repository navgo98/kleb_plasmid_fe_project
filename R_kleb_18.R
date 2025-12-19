#W + m9 
model_summary <- summary(fitted_model)
fixed_effects <- model_summary$tTable[c(1,2,3,4),]
estimates <- fixed_effects[, "Value"]  # Estimated parameter values
se <- fixed_effects[, "Std.Error"]  # Standard errors
lower_bound <- estimates - 1.96*se
upper_bound <- estimates + 1.96*se
ci_w_m9 <- data.frame(lower_bound,estimates,upper_bound)
ci_w_m9$strain <- "W"
colnames(ci_w_m9) <- c("lower","estimates","upper","strain")

#We + m9 
model_summary_2 <- summary(fitted_model_2)
fixed_effects_2 <- model_summary_2$tTable[c(1,2,3,4),]
estimates_2 <- fixed_effects_2[, "Value"]  # Estimated parameter values
se_2 <- fixed_effects_2[, "Std.Error"]  # Standard errors
lower_bound_2 <- estimates_2 - 1.96*se_2
upper_bound_2 <- estimates_2 + 1.96*se_2 
ci_we_m9 <- data.frame(lower_bound_2,estimates_2,upper_bound_2)
ci_we_m9$strain <- "WE"
colnames(ci_we_m9) <- c("lower","estimates","upper","strain")

#wh + m9
model_summary_3 <- summary(fitted_model_3)
fixed_effects_3 <- model_summary_3$tTable[c(1,2,3,4),]
estimates_3 <- fixed_effects_3[, "Value"]  # Estimated parameter values
se_3 <- fixed_effects_3[, "Std.Error"]  # Standard errors
lower_bound_3 <- estimates_3 - 1.96*se_3
upper_bound_3 <- estimates_3 + 1.96*se_3 
ci_wh_m9 <- data.frame(lower_bound_3,estimates_3,upper_bound_3)
ci_wh_m9$strain <- "WH"
colnames(ci_wh_m9) <- c("lower","estimates","upper","strain")

#wv + m9 

model_summary_4 <- summary(fitted_model_4)
fixed_effects_4 <- model_summary_4$tTable[c(1,2,3,4),]
estimates_4 <- fixed_effects_4[, "Value"]  # Estimated parameter values
se_4 <- fixed_effects_4[, "Std.Error"]  # Standard errors
lower_bound_4 <- estimates_4 - 1.96*se_4
upper_bound_4 <- estimates_4 + 1.96*se_4 
ci_wv_m9 <- data.frame(lower_bound_4,estimates_4,upper_bound_4)
ci_wv_m9$strain <- "WV"
colnames(ci_wv_m9) <- c("lower","estimates","upper","strain")

#wve + m9 
model_summary_5 <- summary(fitted_model_5)
fixed_effects_5 <- model_summary_5$tTable[c(1,2,3,4),]
estimates_5 <- fixed_effects_5[, "Value"]  # Estimated parameter values
se_5 <- fixed_effects_5[, "Std.Error"]  # Standard errors
lower_bound_5 <- estimates_5 - 1.96*se_5
upper_bound_5 <- estimates_5 + 1.96*se_5 
ci_wve_m9 <- data.frame(lower_bound_5,estimates_5,upper_bound_5)
ci_wve_m9$strain <- "WVE"
colnames(ci_wve_m9) <- c("lower","estimates","upper","strain")

#combine
library(dplyr)
library(gridExtra)
ci_m9_complete <- rbind(ci_w_m9,
                        ci_we_m9,
                        ci_wh_m9,
                        ci_wv_m9,
                        ci_wve_m9)
ci_m9_complete$parameters <- c("b","t1","t2")

ci_m9_excel <- ci_m9_complete
ci_m9_excel$envi <- "M9"

ci_complete_test_m9_excel <- ci_complete_test_m9
names(ci_complete_test_m9_excel)[names(ci_complete_test_m9_excel) == 'Median'] <- 'estimates'
ci_complete_test_m9_excel$parameters <- "m"

excel_m9 <- rbind(ci_m9_excel,ci_complete_test_m9_excel)
excel_m9 <- excel_m9[order(excel_m9$strain, decreasing = FALSE), ]

excel_m9_filtered_t2 <- excel_m9[excel_m9$parameters != "t2", ]

write.csv(excel_m9, "kleb_m9.csv")

### number string

b_sequence <- seq(2,20,4)
t1_sequence <- seq(3,20,4)
###
g1<- ggplot(ci_m9_complete[b_sequence_clinical,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Growth Rate (b) Estimates without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.3,0.6)


g2<- ggplot(ci_m9_complete[t1_sequence,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  # Minor dodge for alignment
  labs(title = "Lag Duration (c) Estimates Without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(2.3,4.0)

g3<- ggplot(ci_m9_complete[c(3,6,9,12,15),], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  # Minor dodge for alignment
  labs(title = "Time up to stationary phase (t2) without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() 

grid.arrange(g1,g2,g3,ncol = 3)

###########################################################################################

#w + fe

model_summary_6 <- summary(fitted_model_6)
fixed_effects_6 <- model_summary_6$tTable[c(1,2,3,4),]
estimates_6 <- fixed_effects_6[, "Value"]  # Estimated parameter values
se_6 <- fixed_effects_6[, "Std.Error"]  # Standard errors
lower_bound_6 <- estimates_6 - 1.96*se_6
upper_bound_6 <- estimates_6 + 1.96*se_6
ci_w_fe <- data.frame(lower_bound_6,estimates_6,upper_bound_6)
ci_w_fe$strain <- "W"
colnames(ci_w_fe) <- c("lower","estimates","upper","strain")

#we + fe

model_summary_7 <- summary(fitted_model_7)
fixed_effects_7 <- model_summary_7$tTable[c(1,2,3,4),]
estimates_7 <- fixed_effects_7[, "Value"]  # Estimated parameter values
se_7 <- fixed_effects_7[, "Std.Error"]  # Standard errors
lower_bound_7 <- estimates_7 - 1.96*se_7
upper_bound_7 <- estimates_7 + 1.96*se_7
ci_we_fe <- data.frame(lower_bound_7,estimates_7,upper_bound_7)
ci_we_fe$strain <- "WE"
colnames(ci_we_fe) <- c("lower","estimates","upper","strain")

#wh + fe

model_summary_8 <- summary(fitted_model_8)
fixed_effects_8 <- model_summary_8$tTable[c(1,2,3,4),]
estimates_8 <- fixed_effects_8[, "Value"]  # Estimated parameter values
se_8 <- fixed_effects_8[, "Std.Error"]  # Standard errors
lower_bound_8 <- estimates_8 - 1.96*se_8
upper_bound_8 <- estimates_8 + 1.96*se_8
ci_wh_fe <- data.frame(lower_bound_8,estimates_8,upper_bound_8)
ci_wh_fe$strain <- "WH"
colnames(ci_wh_fe) <- c("lower","estimates","upper","strain")

#wv + fe

model_summary_9 <- summary(fitted_model_9)
fixed_effects_9 <- model_summary_9$tTable[c(1,2,3,4),]
estimates_9 <- fixed_effects_9[, "Value"]  # Estimated parameter values
se_9 <- fixed_effects_9[, "Std.Error"]  # Standard errors
lower_bound_9 <- estimates_9 - 1.96*se_9
upper_bound_9 <- estimates_9 + 1.96*se_9
ci_wv_fe <- data.frame(lower_bound_9,estimates_9,upper_bound_9)
ci_wv_fe$strain <- "WV"
colnames(ci_wv_fe) <- c("lower","estimates","upper","strain")

# WVE + Fe

model_summary_10 <- summary(fitted_model_10)
fixed_effects_10 <- model_summary_10$tTable[c(1,2,3,4),]
estimates_10 <- fixed_effects_10[, "Value"]  # Estimated parameter values
se_10 <- fixed_effects_10[, "Std.Error"]  # Standard errors
lower_bound_10 <- estimates_10 - 1.96*se_10
upper_bound_10 <- estimates_10 + 1.96*se_10
ci_wve_fe <- data.frame(lower_bound_10,estimates_10,upper_bound_10)
ci_wve_fe$strain <- "WVE"
colnames(ci_wve_fe) <- c("lower","estimates","upper","strain")

ci_fe_complete <- rbind(ci_w_fe,
                        ci_we_fe,
                        ci_wh_fe,
                        ci_wv_fe,
                        ci_wve_fe)

ci_fe_complete$parameters <- c("c1","b","t1","t2")

g4<- ggplot(ci_fe_complete[b_sequence,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Growth Rate (b) Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal()+
  ylim(0.3,0.6)

g5<- ggplot(ci_fe_complete[t1_sequence,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Lag Duration (b) Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal()+
  ylim(2.3,4.0)

g6<- ggplot(ci_fe_complete[c(3,6,9,12,15),], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Time up to staionary phase (t2) Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() 

#IMPORTANT PLOT
grid.arrange(g1,g4,ncol = 2)
grid.arrange(g2,g5,ncol = 2)
###

library(MASS)
library(ggplot2)
library(gridExtra)


param_hat_mv_test <- lapply(mget(ls(pattern = "fitted_mode.+")),
                            function(x){data.frame(mvrnorm(n = 1000, mu = summary(x)$tTable[c(1,2,3,4),][,"Value"], 
                                                           Sigma = vcov(x)))})

param_hat_mv_test_list <- param_hat_mv_test[c(1,3,4,5,6,7,8,9,10,2)]
ci_max_cap_test <- list()
for (i in 1:10){
  ci_max_cap_test[[i]] <- param_hat_mv_test_list[[i]][1]*(exp(param_hat_mv_test_list[[i]][2]*(param_hat_mv_test_list[[i]][4]-param_hat_mv_test_list[[i]][3])))
}

ci_max_cap_df <- as.data.frame(ci_max_cap_test)
ci_quantile_test <- t(as.data.frame(lapply(ci_max_cap_df, quantile, probs = c(0.025,0.975))))
ci_median_test <- t(as.data.frame(lapply(ci_max_cap_df, median)))


############ MEAN 
ci_mean_test <- t(as.data.frame(lapply(ci_max_cap_df, mean)))

ci_complete_test_mean <- ci_complete_test <- as.data.frame(cbind(ci_quantile_test,ci_mean_test)) 
colnames(ci_complete_test_mean) <- c("lower","upper","Mean")
ci_complete_test_mean$strain <- rep(c("W","WE","WH","WV","WVE"))
ci_complete_test_mean$envi <- rep(c("M9","Fe"),each = 5)
ci_complete_test_m9_mean <- ci_complete_test_mean[1:5,]
ci_complete_test_fe_mean <- ci_complete_test_mean[6:10,]

g11 <- ggplot(ci_complete_test_m9_mean, aes(x = strain, y = Mean)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates without Fe - MEAN",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)


g12 <-  ggplot(ci_complete_test_fe_mean, aes(x = strain, y = Mean)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates with Fe - MEAN",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)

grid.arrange(g11, g12, ncol = 2)

grid.arrange(g11, g9, ncol = 2)
grid.arrange(g12, g10, ncol = 2)
############### 


ci_complete_test <- as.data.frame(cbind(ci_quantile_test,ci_median_test)) 
colnames(ci_complete_test) <- c("lower","upper","Median")
ci_complete_test$strain <- rep(c("W","WE","WH","WV","WVE"))
ci_complete_test$envi <- rep(c("M9","Fe"),each = 5)
ci_complete_test_m9 <- ci_complete_test[1:5,]
ci_complete_test_fe <- ci_complete_test[6:10,]

g9 <- ggplot(ci_complete_test_m9, aes(x = strain, y = Median)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)

g10 <- ggplot(ci_complete_test_fe, aes(x = strain, y = Median)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)

grid.arrange(g9, g10, ncol = 2)


ci_fe_excel <- ci_fe_complete
ci_fe_excel$envi <- "Fe"

ci_complete_test_fe_excel <- ci_complete_test_fe
names(ci_complete_test_fe_excel)[names(ci_complete_test_fe_excel) == 'Median'] <- 'estimates'
ci_complete_test_fe_excel$parameters <- "m"

excel_fe <- rbind(ci_fe_excel,ci_complete_test_fe_excel)
excel_fe <- excel_fe[order(excel_fe$strain, decreasing = FALSE), ]

excel_fe_filtered_t2 <- excel_fe[excel_fe$parameters != "t2", ]


write.csv(excel_fe, "kleb_fe.csv")


#Effect size different between iron and iron-limited 
growth_es <- (excel_fe_filtered_t2[excel_fe_filtered_t2$parameters == "b",]$estimates - 
  excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "b",]$estimates ) / 
  excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "b",]$estimates

lag_es <- (excel_fe_filtered_t2[excel_fe_filtered_t2$parameters == "t1",]$estimates - 
             excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "t1",]$estimates ) / 
  excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "t1",]$estimates

max_cap_es <- (excel_fe_filtered_t2[excel_fe_filtered_t2$parameters == "m",]$estimates - 
                 excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "m",]$estimates ) / 
  excel_m9_filtered_t2[excel_m9_filtered_t2$parameters == "m",]$estimates

#flip



#
effect_size <- data.frame(
  strain = rep(c("W","WE","WH","WV","WVE")),
  lag_effectsize = lag_es,
  growth_effectsize = growth_es,
  max_cap_effectsize = max_cap_es
)

print(effect_size)

# Create the barplot


# Load ggplot2 (only ggplot2, not all of tidyverse)
library(ggplot2)

# Use the effect_sizes data from earlier
# effect_sizes should have columns: Strain, Lag_EffectSize, GrowthRate_EffectSize, Capacity_EffectSize

# Reshape from wide to long format using base R
effect_long <- reshape(
  effect_size,
  varying = list(2:4),
  v.names = "effect_size",
  timevar = "trait",
  times = c("Lag Duration", "Growth Rate", "Maximum Capacity"),
  direction = "long"
)

# Optional: clean up row names and factor order
rownames(effect_long) <- NULL
effect_long$trait <- factor(effect_long$trait, levels = c("Lag Duration", "Growth Rate", "Maximum Capacity"))

# Plot with ggplot: Traits on x-axis, grouped by Strain
ggplot(effect_long, aes(x = trait, y = effect_size, fill = strain)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect Sizes of Traits in Iron vs Without Iron Culture",
    x = "Trait",
    y = "Relative Effect Size"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  ylim(0,1.5)


# bar_vals <- barplot(
#   t(as.matrix(effect_size[, 2:4])),  # Use correct variable name
#   beside = TRUE,
#   names.arg = effect_size$strain,
#   col = c("skyblue", "orange", "seagreen"),
#   main = "Effect Sizes of Traits (Iron vs absent Iron)",
#   ylab = "Relative Effect Size",
#   ylim = c(0,2)
# )

# # Add horizontal line at 0
# abline(h = 0, lty = 2)
# 
# # Add a custom legend in the top right
# legend(
#   "topright",
#   legend = c("lag duration","growth rate","maximum capacity"),
#   fill = c("skyblue", "orange", "seagreen"),
# )
# 
# 
# abline(h = 0, lty = 2)

excel_m9_fe_filtered_t2 <- rbind(excel_m9_filtered_t2,excel_fe_filtered_t2)

#Growth Rate
b_sequence_m9_fe_filtered_t2 <- seq(1,30,by=3)

ggplot(excel_m9_fe_filtered_t2[b_sequence_m9_fe_filtered_t2,],
       aes(x = strain, y = estimates, color = envi)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  
  geom_errorbar(aes(ymin = lower, ymax = upper, color = envi),
                width = 0.2,
                linewidth = 0.6,
                position = position_dodge(width = 0.5)) +  
  labs(title = "Growth Rate Estimates",
       x = "Strains",
       y = "Estimate",
       color = "Environment") +
  theme_minimal() +
  ylim(0.3, 0.6) 
  #scale_shape_manual(values = c(16, 17, 15, 18, 10))

#Lag Duration
t1_sequence_m9_fe_filtered_t2 <- seq(2,30,by=3)

ggplot(excel_m9_fe_filtered_t2[t1_sequence_m9_fe_filtered_t2,],
       aes(x = strain, y = estimates, color = envi)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  
  geom_errorbar(aes(ymin = lower, ymax = upper, color = envi),
                width = 0.2,
                linewidth = 0.6,
                position = position_dodge(width = 0.5)) +  
  labs(title = "Lag Duration Estimates",
       x = "Strains",
       y = "Estimate",
       color = "Environment") +
  theme_minimal() +
  ylim(2, 4) + 
  scale_shape_manual(values = c(16, 17, 15, 18, 10))


#Maximum Capacity
m_sequence_m9_fe_filtered_t2 <- seq(3,30,by=3)

ggplot(excel_m9_fe_filtered_t2[m_sequence_m9_fe_filtered_t2,],
       aes(x = strain, y = estimates, color = envi)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +  
  geom_errorbar(aes(ymin = lower, ymax = upper, color = envi),
                width = 0.2,
                linewidth = 0.8,
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates",
       x = "Strains",
       y = "Estimate",
       color = "Environment") +
  theme_minimal() +
  ylim(0.4, 1.3) + 
  scale_shape_manual(values = c(16, 17, 15, 18, 10))

#CLINICAL DATA - m9 
setwd("C:/Users/ngodi/OneDrive/Documents/Kleb_project")
kp_data_clinical <- read.xlsx(file = "C:/Users/ngodi/OneDrive/Documents/Kleb_project/Kleb_clinical.xlsx",1)

kp_data_clinical_m9 <- as.data.frame((kp_data_clinical[1:48,4:54]))

kp_data_clinical_m9_transposed <- data.frame(OD = as.vector(t(kp_data_clinical_m9)))

kp_data_clinical_m9_transposed$time <- seq(0,15,0.3)

kp_data_clinical_m9_transposed$Replicate <- rep(c(1,2,3),each = 51)

kp_data_clinical_m9_transposed$strain <- rep(c("ST887",
                                               "ST65",
                                               "ST375",
                                               "ST23.1",
                                               "ST714",
                                               "ST25",
                                               "ST1049",
                                               "ST86",
                                               "ST816",
                                               "ST367",
                                               "ST828",
                                               "ST29",
                                               "ST218",
                                               "ST23.2",
                                               "ST412",
                                               "ST420"),each = 153)


clinical_st887_m9_test <- kp_data_clinical_m9_transposed[1:153,1:3]

clinical_fit_st887_m9_test <- fit_piecewise_growth(
  data = clinical_w_m9_test,
  time_var = "time",
  od_var = "OD",
  replicate_var = "Replicate"
)


  
clinical_fit_m9 <- fit_piecewise_growth(
  data = kp_data_clinical_m9_transposed,
  time_var = "time",
  od_var = "OD",
  replicate_var = c("Replicate","strain")
)


summary(clinical_fit_m9)
coef(summary(clinical_fit_m9))

#Prediction
kp_data_clinical_m9_transposed$fitted_by_rep <- predict(clinical_fit_m9) # Predictions by replicate
kp_data_clinical_m9_transposed$fitted <- predict(clinical_fit_m9, level = 0)  # Fixed effect predictions
kp_data_clinical_m9_transposed$Replicate <- as.factor(kp_data_clinical_m9_transposed$Replicate)


library(ggplot2)
library(RColorBrewer)  # For distinct colors



strain_colour <- c(
  "red", "blue", "green", "purple", "orange", "brown",
  "darkred", "darkblue", "darkgreen", "darkorchid", "darkorange", "saddlebrown",
  "lightcoral", "deepskyblue", "limegreen", "mediumpurple"
)


g_clinical_m9_test <- ggplot(kp_data_clinical_m9_transposed, aes(x = time, y = OD, color = strain)) +
  geom_point(alpha = 0.6) +  # Observed data points
  #geom_line(aes(y = fitted_by_rep, group = Replicate), linewidth = 1) +  # Fitted curves for replicates
  geom_line(aes(y = fitted, linetype = "Average Growth Curve"), color = "black", linewidth = 1.2) +  # Fixed effects curve
  labs(
    title = "Clinical Strains - Observed Data and Fitted Piecewise Growth Curves M9",
    x = "time (hours)",
    y = "Optical Density (OD)",
    linetype = "Average Growth Curve"
  ) +
  theme_minimal() +
  scale_color_manual(
    name = "Strain",
    values = strain_colour
  ) +
  scale_linetype_manual(
    name = "Average Growth Curve",
    values = c("Average Growth Curve" = "dashed")  # Assign dashed line to average curve
  ) +
  guides(
    color = guide_legend(order = 1),  # Order replicates first
    linetype = guide_legend(order = 2) # Order the average curve after replicates
  ) + ylim(0,2.3)




g_clinical_m9_test <- ggplot(kp_data_clinical_m9_transposed, aes(x = time, y = OD, color = strain)) +
  geom_point(alpha = 0.6) +  # Observed data points
  #geom_line(aes(y = fitted_by_rep, group = Replicate), linewidth = 1) +  # Fitted curves for replicates
  geom_line(aes(y = fitted, linetype = "Average Growth Curve"), color = "black", linewidth = 1.2) +  # Fixed effects curve
  labs(
    title = "Clinical Strains - Observed Data and Fitted Piecewise Growth Curves M9",
    x = "time (hours)",
    y = "Optical Density (OD)",
    linetype = "Average Growth Curve"
  ) +
  theme_minimal() +
  scale_color_manual(
    name = "Strain",
    values = strain_colour
  ) +
  scale_linetype_manual(
    name = "Average Growth Curve",
    values = c("Average Growth Curve" = "dashed")  # Assign dashed line to average curve
  ) +
  theme(legend.position = "none") +  # Remove legend
  ylim(0, 2.3)

model_summary_clinical_m9 <- summary(clinical_fit_m9)
fixed_effects_clinical_m9 <- model_summary_clinical_m9$tTable[c(1,2,3,4),]
estimates_clinical_m9 <- fixed_effects_clinical_m9[, "Value"]  # Estimated parameter values
se_clinical_m9 <- fixed_effects_clinical_m9[, "Std.Error"]  # Standard errors
lower_bound_clinical_m9 <- estimates_clinical_m9 - 1.96*se_clinical_m9
upper_bound_clinical_m9 <- estimates_clinical_m9 + 1.96*se_clinical_m9
ci_clinical_m9 <- data.frame(lower_bound_clinical_m9,estimates_clinical_m9,upper_bound_clinical_m9)
ci_clinical_m9$strain <- "Clinical"
colnames(ci_clinical_m9) <- c("lower","estimates","upper","strain")
ci_clinical_m9$parameters <- c("c1","b","t1","t2")

ci_clinical_m9_complete <- rbind(ci_m9_complete,ci_clinical_m9[2:4,])


b_sequence_clinical <- seq(1,18,3)
t1_sequence_clinical <- seq(2,18,3)

###GG-PLOT

g_clinical_m9_growth_rate <- ggplot(ci_clinical_m9_complete[b_sequence_clinical,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Growth Rate Estimates without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.3,0.6)

g_clinical_m9_lag <- ggplot(ci_clinical_m9_complete[t1_sequence_clinical,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  # Minor dodge for alignment
  labs(title = "Lag Duration Estimates Without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(1.5,4.0)



### CLINICAL - FE

kp_data_clinical_fe <- as.data.frame((kp_data_clinical[49:96,4:54]))

kp_data_clinical_fe_transposed <- data.frame(OD = as.vector(t(kp_data_clinical_fe)))

kp_data_clinical_fe_transposed$time <- seq(0,15,0.3)

kp_data_clinical_fe_transposed$Replicate <- rep(c(1,2,3),each = 51)

kp_data_clinical_fe_transposed$strain <- rep(c("ST887",
                                               "ST65",
                                               "ST375",
                                               "ST23.1",
                                               "ST714",
                                               "ST25",
                                               "ST1049",
                                               "ST86",
                                               "ST816",
                                               "ST367",
                                               "ST828",
                                               "ST29",
                                               "ST218",
                                               "ST23.2",
                                               "ST412",
                                               "ST420"),each = 153)


clinical_fit_fe <- fit_piecewise_growth(
  data = kp_data_clinical_fe_transposed,
  time_var = "time",
  od_var = "OD",
  replicate_var = c("Replicate","strain")
)


summary(clinical_fit_fe)
coef(summary(clinical_fit_fe))

#Prediction
kp_data_clinical_fe_transposed$fitted_by_rep <- predict(clinical_fit_fe) # Predictions by replicate
kp_data_clinical_fe_transposed$fitted <- predict(clinical_fit_fe, level = 0)  # Fixed effect predictions
kp_data_clinical_fe_transposed$Replicate <- as.factor(kp_data_clinical_fe_transposed$Replicate)


g_clinical_fe_test <- ggplot(kp_data_clinical_fe_transposed, aes(x = time, y = OD, color = strain)) +
  geom_point(alpha = 0.6) +  # Observed data points
  #geom_line(aes(y = fitted_by_rep, group = Replicate), linewidth = 1) +  # Fitted curves for replicates
  geom_line(aes(y = fitted, linetype = "Average Growth Curve"), color = "black", linewidth = 1.2) +  # Fixed effects curve
  labs(
    title = "Clinical Strains - Observed Data and Fitted Piecewise Growth Curves With Iron",
    x = "time (hours)",
    y = "Optical Density (OD)",
    linetype = "Average Growth Curve"
  ) +
  theme_minimal() +
  scale_color_manual(
    name = "Strain",
    values = strain_colour
  ) +
  scale_linetype_manual(
    name = "Average Growth Curve",
    values = c("Average Growth Curve" = "dashed")  # Assign dashed line to average curve
  ) +
  guides(
    color = guide_legend(order = 1),  # Order replicates first
    linetype = guide_legend(order = 2)  # Order the average curve after replicates
  ) + ylim(0,2.3)

grid.arrange(g_clinical_m9_test, g_clinical_fe_test, ncol = 2)


model_summary_clinical_fe <- summary(clinical_fit_fe)
fixed_effects_clinical_fe <- model_summary_clinical_fe$tTable[c(1,2,3,4),]
estimates_clinical_fe <- fixed_effects_clinical_fe[, "Value"]  # Estimated parameter values
se_clinical_fe <- fixed_effects_clinical_fe[, "Std.Error"]  # Standard errors
lower_bound_clinical_fe <- estimates_clinical_fe - 1.96*se_clinical_fe
upper_bound_clinical_fe <- estimates_clinical_fe + 1.96*se_clinical_fe
ci_clinical_fe <- data.frame(lower_bound_clinical_fe,estimates_clinical_fe,upper_bound_clinical_fe)
ci_clinical_fe$strain <- "Clinical"
colnames(ci_clinical_fe) <- c("lower","estimates","upper","strain")
ci_clinical_fe$parameters <- c("c1","b","t1","t2")

ci_clinical_fe_complete <- rbind(ci_fe_complete,ci_clinical_fe[2:4,])



g_clinical_fe_growth_rate <- ggplot(ci_clinical_fe_complete[b_sequence_clinical,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Growth Rate Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.3,0.6)

g_clinical_fe_lag <- ggplot(ci_clinical_fe_complete[t1_sequence_clinical,], aes(x = strain, y = estimates)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  # Minor dodge for alignment
  labs(title = "Lag Duration Estimates With Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(1.5,4.0)

grid.arrange(g_clinical_m9_growth_rate,g_clinical_fe_growth_rate, ncol = 2)
grid.arrange(g_clinical_m9_lag,g_clinical_fe_lag, ncol = 2)
#as.data.frame(as.matrix(mvrnorm(n = 1000, mu = coef(fitted_model)[1,], 
                                #Sigma = vcov(fitted_model))))





fitted_model_11 <- clinical_fit_m9
fitted_model_12 <- clinical_fit_fe





param_hat_mv_test_clinical <- lapply(mget(ls(pattern = "fitted_mode.+")),
                            function(x){data.frame(mvrnorm(n = 1000, mu = summary(x)$tTable[c(1,2,3,4),][,"Value"], 
                                                           Sigma = vcov(x)))})

param_hat_mv_test_list_clinical <- param_hat_mv_test_clinical[c(1,5,6,7,8,3,9,10,11,12,2,4)]
ci_max_cap_test_clinical <- list()
for (i in 1:12){
  ci_max_cap_test_clinical[[i]] <- param_hat_mv_test_list_clinical[[i]][1]*(exp(param_hat_mv_test_list_clinical[[i]][2]*(param_hat_mv_test_list_clinical[[i]][4]-param_hat_mv_test_list_clinical[[i]][3])))
}

ci_max_cap_df_clinical <- as.data.frame(ci_max_cap_test_clinical)
ci_quantile_test_clinical <- t(as.data.frame(lapply(ci_max_cap_df_clinical, quantile, probs = c(0.025,0.975))))
ci_median_test_clinical <- t(as.data.frame(lapply(ci_max_cap_df_clinical, median)))



ci_complete_test_clinical <- as.data.frame(cbind(ci_quantile_test_clinical,ci_median_test_clinical)) 
colnames(ci_complete_test_clinical) <- c("lower","upper","Median")
ci_complete_test_clinical$strain <- rep(c("W","WE","WH","WV","WVE","Clinical"))
ci_complete_test_clinical$envi <- rep(c("M9","Fe"),each = 6)
ci_complete_test_m9_clinical <- ci_complete_test_clinical[1:6,]
ci_complete_test_fe_clinical <- ci_complete_test_clinical[7:12,]

g_clinical_m9_max_cap <- ggplot(ci_complete_test_m9_clinical, aes(x = strain, y = Median)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates without Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)

g_clinical_fe_max_cap <- ggplot(ci_complete_test_fe_clinical, aes(x = strain, y = Median)) +
  geom_point(size = 4, color = "blue") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2,  
                linewidth = 0.8,  
                color = "red", 
                position = position_dodge(width = 0.5)) +  
  labs(title = "Maximum Capacity Estimates with Fe",
       x = "Strains",
       y = "Estimate") +
  theme_minimal() +
  ylim(0.4,1.3)

grid.arrange(g_clinical_m9_max_cap, g_clinical_fe_max_cap, ncol = 2)

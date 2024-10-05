getwd()
{
  library(readxl)
  library(dplyr)
  library(metafor)
  library(readr)
  library(ggplot2)
}

BTCQ_combined <- read_csv("data_for_analysis/BTCQ_MA_v4.csv", na="")

## What are the names of interventions?
table(BTCQ_combined$`Intervention(=arm)`)

BTCQ_combined$author_arm <- paste(BTCQ_combined$Study_Author," ","(",BTCQ_combined$Study_Arm,")", sep="")

# Create Open subset
open_subset <- BTCQ_combined %>% filter(`Intervention(=arm)` == "Open")
# Create Endoscopic subset
endoscopic_subset <- BTCQ_combined %>% filter(`Intervention(=arm)` == "Endoscopic")
#create non operative subset 
nonoperative_subset <- BTCQ_combined %>% filter(`Intervention(=arm)` == "Non-operative")

meta_analysis_re <- function(data,time_point_mean, time_point_sd, time_label) {
  
  # Subset rows that have NA for the specific time point
  data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
  
  # Compute the standardized mean change and its variance
  data_time$smc <- (data_time[[time_point_mean]] - data_time$Mean_Baseline) / data_time$SD_baseline
  data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm 
  
  # Random-Effects Meta-Analysis
  res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
  
  # Forest Plot to visualize the results
  forest(res_re, slab = data_time$author_arm,
         xlab = "Standardised Mean Change", 
         sub = paste("BCTQ Score Change at", time_label, "weeks"))
  
  # Funnel Plot
  funnel(res_re, main = paste("Funnel Plot for Time Point", time_label, "weeks"))
  
  return(res_re)
  
  }




meta_analysis_re <- function(data, data_name, time_point_mean, time_point_sd, time_label) {
  
  # Load necessary libraries
  library(metafor)
  library(ggplot2)
  
  # Subset rows that have NA for the specific time point
  data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
  
  # Compute the standardized mean change and its variance
  data_time$smc <- (data_time[[time_point_mean]] - data_time$Mean_Baseline) / data_time$SD_baseline
  data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm 
  
  # Random-Effects Meta-Analysis
  res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
  
  # Forest Plot to visualize the results
  jpeg(file=paste0(data_name,"_","Forest_Plot_", time_label, "weeks.jpeg"), width=10, height=8, units="in", res=1200)
  forest(res_re, slab = data_time$author_arm,
         xlab = "Standardized Mean Change", 
         sub = paste("BCTQ Score Change at", time_label, "weeks"))
  dev.off()
  
  # Funnel Plot
  jpeg(file=paste0(data_name,"_","Funnel_Plot_", time_label, "weeks.jpeg"), width=10, height=8, units="in", res=1200)
  funnel(res_re, main = paste("BTCQ Score Standardised Mean Change", time_label, "weeks"))
  dev.off()
  
  return(res_re)
}

# Run meta-analysis for each time point combined and save plots as JPEG
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_2weeks", "SD_2weeks", "2")
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_4weeks", "SD_4weeks", "4")
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_6weeks", "SD_6weeks", "6")
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_12weeks", "SD_12weeks", "12")
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_24weeks", "SD_24weeks", "24")
meta_analysis_re(data = BTCQ_combined, data_name = "BTCQ_combined", "Mean_52weeks", "SD_52weeks", "52")


# Run meta-analysis for each time point - "OPEN" subset

meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_2weeks", "SD_2weeks", "2")
meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_4weeks", "SD_4weeks", "4")
meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_6weeks", "SD_6weeks", "6")
meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_12weeks", "SD_12weeks", "12")
meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_24weeks", "SD_24weeks", "24")
meta_analysis_re(data = open_subset, data_name = "BTCQ_open_subset", "Mean_52weeks", "SD_52weeks", "52")


# Run meta-analysis for each time point - "Endoscopic" subset

meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_2weeks", "SD_2weeks", "2")
meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_4weeks", "SD_4weeks", "4")
meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_6weeks", "SD_6weeks", "6")
meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_12weeks", "SD_12weeks", "12")
meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_24weeks", "SD_24weeks", "24")
meta_analysis_re(data = endoscopic_subset, data_name = "BTCQ_endoscopic_subset", "Mean_52weeks", "SD_52weeks", "52")


#### SAVE POOLED EFFECTS---


# Define a function to perform meta-analysis and return the pooled effect size
meta_analysis_re2 <- function(data,time_point_mean, time_point_sd, time_label) {
  
  # Subset rows that have NA for the specific time point
  data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
  
  # Compute the standardized mean change and its variance
  data_time$smc <- (data_time[[time_point_mean]] - data_time$Mean_Baseline) / data_time$SD_baseline
  data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm 
  
  # Random-Effects Meta-Analysis
  res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
  
  # Forest Plot to visualize the results
  p <- forest(res_re, slab = data_time$Study_Author, 
              xlab = "Standardised Mean Change",
              sub = paste("BCTQ Score Change at", time_label, "weeks"))
  
  # Return the pooled effect size
  return(res_re$b)
}

# (ALL) Run meta-analysis for each time point and store the pooled effect sizes
pooled_effects_all <- c()
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined, "Mean_2weeks", "SD_2weeks", "2"))
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined,"Mean_4weeks", "SD_4weeks", "4"))
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined,"Mean_6weeks", "SD_6weeks", "6"))
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined,"Mean_12weeks", "SD_12weeks", "12"))
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined,"Mean_24weeks", "SD_24weeks", "24"))
pooled_effects_all <- c(pooled_effects_all, meta_analysis_re2(data=BTCQ_combined,"Mean_52weeks", "SD_52weeks", "52"))

# Plot pooled effects
plot(1:length(pooled_effects_all), pooled_effects_all, type = "b", 
     xlab = "Time Point", ylab = "Pooled Effect Size",
     main = "Pooled Effect Sizes over Time SMC (all)")


# Run meta-analysis for each time point and store the pooled effect sizes - open subset
pooled_effects_open <- c()
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_2weeks", "SD_2weeks", "2"))
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_4weeks", "SD_4weeks", "4"))
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_6weeks", "SD_6weeks", "6"))
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_12weeks", "SD_12weeks", "12"))
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_24weeks", "SD_24weeks", "24"))
pooled_effects_open <- c(pooled_effects_open, meta_analysis_re2(data=open_subset,"Mean_52weeks", "SD_52weeks", "52"))


# Plot pooled effects
plot(1:length(pooled_effects_open), pooled_effects_open, type = "b", 
     xlab = "Time Point", ylab = "Pooled Effect Size",
     main = "Pooled Effect Sizes over Time SMC Open Subset")



# Run meta-analysis for each time point and store the pooled effect sizes - endoscopic
pooled_effects_endoscopic <- c()
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_2weeks", "SD_2weeks", "2"))
pooled_effects_endoscopic <- c(NA)
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_4weeks", "SD_4weeks", "4"))
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_6weeks", "SD_6weeks", "6"))
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_12weeks", "SD_12weeks", "12"))
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_24weeks", "SD_24weeks", "24"))
pooled_effects_endoscopic <- c(pooled_effects_endoscopic, meta_analysis_re2(data=endoscopic_subset,"Mean_52weeks", "SD_52weeks", "52"))

# Plot pooled effects
plot(1:length(pooled_effects_endoscopic), pooled_effects_endoscopic, type = "b", 
     xlab = "Time Point", ylab = "Pooled Effect Size",
     main = "Pooled Effect Sizes over Time SMC Endoscopic Subset")


### PLOT ALL POOLED EFFECTS TOGETHER


all_PE <- data.frame("Intervention_arm" = rep(c("All"),6), "Pooled_Effect_Size" = pooled_effects_all, "Weeks" = c(2,4,6,12,24,52), "TP" = c(1,2,3,4,5,6))
all_open <- data.frame("Intervention_arm" = rep(c("Open"),6), "Pooled_Effect_Size" = pooled_effects_open, "Weeks" = c(2,4,6,12,24,52), "TP" = c(1,2,3,4,5,6))
all_endoscopic <- data.frame("Intervention_arm" = rep(c("Endoscopic"),6), "Pooled_Effect_Size" = pooled_effects_endoscopic, "Weeks" = c(2,4,6,12,24,52), "TP" = c(1,2,3,4,5,6))
# Combine datasets
pooled_effects_summary <- rbind(all_PE, all_open, all_endoscopic)

#Plot combined datasets
rmv_wks <- c(2,52)
pooled_effects_summary %>% 
  filter(!Weeks %in% rmv_wks) %>%
  ggplot(aes(x=Weeks, y=Pooled_Effect_Size, colour=Intervention_arm)) +
  geom_line(alpha=0.5, size = 0.8) + 
  geom_point(alpha=0.5, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 24, by = 4), limits = c(2, 24)) +
  theme_minimal() + 
  # Specify plot text size
  theme(text = element_text(size = 14)) +
  # Specify what the label text is
  labs( x="Weeks", y="Pooled Effect Size")  +
  scale_colour_manual(name = "Intervetion Type", values = c("black", "red3", "deepskyblue2"))

#ggsave("BTCQ_pooled_effects_size_SMC.jpeg", dpi=1200)

#################


all_PE <- data.frame("Intervention_arm" = rep(c("All"),6), "Pooled_Effect_Size" = pooled_effects_all, "Weeks" = c(2,4,6,12,24,52), "TP" = c(1,2,3,4,5,6))
# Combine datasets
pooled_effects_all <- rbind(all_PE)


rmv_wks <- c(2,52)
pooled_effects_all %>% 
  filter(!Weeks %in% rmv_wks) %>%
  ggplot(aes(x=Weeks, y=Pooled_Effect_Size, colour=Intervention_arm)) +
  geom_line(alpha=0.5, size = 0.8) + 
  geom_point(alpha=0.5, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 24, by = 4), limits = c(2, 24)) +
  theme_minimal() + 
  # Specify plot text size
  theme(text = element_text(size = 14)) +
  # Specify what the label text is
  labs( x="Weeks", y="Pooled Effect Size")  +
  scale_colour_manual(name = "Intervetion Type", values = c("black"))

#ggsave("BTCQ_pooled_effects_size_SMC_all.jpeg", dpi=1200)


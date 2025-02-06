# Title: Run CTR meta analysis and create forest + funnel Plots
# Date: 13/10/2024

#=========================================================#
# 1 DEFINE FUNCTIONS ----
#=========================================================#

#### SMC FROM BASELINE
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
  
  # Extract heterogeneity statistics
  #I2 <- res_re$I2
  # Extract heterogeneity statistics
  I2 <- res_re$I2  # This gives the I² value (% of variation due to heterogeneity)
  Q <- res_re$QE   # Cochran's Q statistic
  tau2 <- res_re$tau2  # Between-study variance

  ### Funnel and Forest on the same plot
  #jpeg(file=paste0(data_name,"_","FF_plots_", time_label, "weeks.jpeg"), width=16, height=10, units="in", res=1200)
  par(mfrow = c(1, 2))
  # forest plot
  forest(res_re, slab = data_time$author_arm, xlab = "Standardized Mean Change", main="(a)")
  # funnel plot
  funnel(res_re, main="(b)")
  #dev.off()
  
  # Return result summary
  result_data <- data.frame("time_point" = time_label, 
                            "SMC_Baseline" = res_re$beta, 
                            "lCI_Baseline" = res_re$ci.lb, 
                            "uCI_Baseline" = res_re$ci.ub, 
                            I2 = I2, 
                            Q = Q, 
                            tau2 = tau2)
  #return(I2)
  return(result_data)
  
}



#===========================================================#
# 2 IMPORT DATA -----
#===========================================================#

getwd()
{
  library(readxl)
  library(dplyr)
  library(metafor)
  library(readr)
  library(ggplot2)
}

BCTQ_combined <- read_csv("CTR_review/outputs-2025-02-06/raw data/BTCQ_MA_v4.csv", na="")

## What are the names of interventions?
table(BCTQ_combined$`Intervention(=arm)`)

BCTQ_combined$author_arm <- paste(BCTQ_combined$Study_Author," ","(",BCTQ_combined$Study_Arm,")", sep="")

# Create Open subset
open_subset <- BCTQ_combined %>% filter(`Intervention(=arm)` == "Open")
# Create Endoscopic subset
endoscopic_subset <- BCTQ_combined %>% filter(`Intervention(=arm)` == "Endoscopic")

#===========================================================================#
# 3 META ANALYSIS, FOREST PLOTS, FUNNEL PLOTS ----
#===========================================================================#

## SMC + CIs for Open Subset

## 1 week---
## raw smc
(open_1week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_1week", "SD_ 1week", "1"))

## 2 weeks---
## raw smc
(open_2week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_2weeks", "SD_2weeks", "2"))

## 3 weeks---
## raw smc
(open_3week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_3weeks", "SD_3weeks", "3"))

## 4 weeks---
## raw smc
(open_4week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_4weeks", "SD_4weeks", "4"))

## 6 weeks---
## raw smc
(open_6week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_6weeks", "SD_6weeks", "6"))

## 12 weeks---
## raw smc
(open_12week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_12weeks", "SD_12weeks", "12"))

## 24 weeks---
## raw smc
(open_24week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_24weeks", "SD_24weeks", "24"))

## 52 weeks---
## raw smc
(open_52week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_52weeks", "SD_52weeks", "52"))

## 72 weeks---
## raw smc
(open_72week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_72weeks", "SD_72weeks", "72"))

## 104 weeks---
## raw smc
(open_104week_FF <- meta_analysis_re(data = open_subset, data_name = "open_subset",  "Mean_104weeks", "SD_104weeks", "104"))


## SMC + CIs for Endoscopic Subset

## 1 week---
## raw smc
(endo_1week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_1week", "SD_ 1week", "1"))

## 2 weeks---
## raw smc
(endo_2week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_2weeks", "SD_2weeks", "2"))

## 3 weeks---
## raw smc
(endo_3week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_3weeks", "SD_3weeks", "3"))

## 4 weeks---
## raw smc
(endo_4week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_4weeks", "SD_4weeks", "4"))

## 6 weeks---
## raw smc
(endo_6week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_6weeks", "SD_6weeks", "6"))

## 12 weeks---
## raw smc
(endo_12week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_12weeks", "SD_12weeks", "12"))

## 24 weeks---
## raw smc
(endo_24week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_24weeks", "SD_24weeks", "24"))

## 52 weeks---
## raw smc
(endo_52week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_52weeks", "SD_52weeks", "52"))

## 72 weeks---
## raw smc
(endo_72week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_72weeks", "SD_72weeks", "72"))

## 104 weeks---
## raw smc
(endo_104week_FF <- meta_analysis_re(data = endoscopic_subset, data_name = "endoscopic_subset",  "Mean_104weeks", "SD_104weeks", "104"))


#============================================================#
# 4 Export SMC data ----
#============================================================#

### Export dataset on SMC from baseline at each time point
baseline_smc_open <- rbind(open_1week_FF, open_2week_FF, open_3week_FF, open_4week_FF, open_6week_FF, open_12week_FF, 
                           open_24week_FF, open_52week_FF, open_72week_FF, open_104week_FF)
baseline_smc_open$intervention <- "Open"

baseline_smc_endo <- rbind(endo_1week_FF, endo_2week_FF, endo_3week_FF, endo_4week_FF, endo_6week_FF, endo_12week_FF, 
                           endo_24week_FF, endo_52week_FF, endo_72week_FF, endo_104week_FF)
baseline_smc_endo$intervention <- "Endoscopic"

baseline_smc_dat <- rbind(baseline_smc_open, baseline_smc_endo)
#write.csv(baseline_smc_dat, "CTR_review/outputs-2025-02-06/model_data/SMC_from_Baseline.csv", row.names = FALSE)

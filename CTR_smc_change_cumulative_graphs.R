#
# Title: Plot cumulative SMC from baseline with CIs 
# Project: Carpel tunnel release systematic review
# Date: 03/10/2024
#

#================================================================================================#
### ABOUT THE SCRIPT

## This script takes the BTCQ dataset, filters for separate Open vs Endoscopic subsets,
## calculates the SMC + CIs at all time points (using 'from' and 'to' timepoints), 
## runs a cumulative calculation of SMCs and CIs,
## then plots the information as a line graph (with ribbons).

#==================================================================================================#

# Load packages-----------------------
{
  library(readxl)
  library(dplyr)
  library(metafor)
  library(readr)
  library(ggplot2)
}

# Import and prep data----------------

data <- read_csv("CTR_review/data_for_analysis/BTCQ_MA_v4.csv", na="")

# Remove empty rows
data <- data %>% filter(!is.na(Study_Author))

# Add author_arm
data$author_arm <- paste(data$Study_Author," ","(",data$Study_Arm, ",", data$N_In_Arm, ")", sep="")

# Create open vs endoscopic subset
subset <- data %>% filter(`Intervention(=arm)` == "Open" | `Intervention(=arm)` == "Endoscopic" )
table(data$`Intervention(=arm)`)

# Create Open subset
open_subset <- data %>% filter(`Intervention(=arm)` == "Open")
# Create Endoscopic subset
endo_subset <- data %>% filter(`Intervention(=arm)` == "Endoscopic")


# Define functions------------------

### CUSTOM META ANALYSIS FUNCTION
##### - Includes "from" and "to" components.
##### - Includes checks and imputes data from the time point mean to handle missing values.

meta_analysis_custom <- function(data, mean_from, sd_from, time_point_mean, time_point_sd, time_label) {
  
  # Impute missing values for the columns
  # Calculate the mean of the 'from' and 'to' time point data for imputation
  mean_from_value <- mean(data[[mean_from]], na.rm = TRUE)
  sd_from_value <- mean(data[[sd_from]], na.rm = TRUE)
  time_point_mean_value <- mean(data[[time_point_mean]], na.rm = TRUE)
  time_point_sd_value <- mean(data[[time_point_sd]], na.rm = TRUE)
  
  # Impute missing values with the mean of the available data
  data[[mean_from]][is.na(data[[mean_from]])] <- mean_from_value
  data[[sd_from]][is.na(data[[sd_from]])] <- sd_from_value
  data[[time_point_mean]][is.na(data[[time_point_mean]])] <- time_point_mean_value
  data[[time_point_sd]][is.na(data[[time_point_sd]])] <- time_point_sd_value
  
  # Now proceed with the meta-analysis
  # Subset rows that have valid data for the 'to' time point
  data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
  
  # Check if k (the number of valid rows) is greater than 0
  k <- nrow(data_time)
  
  if (k > 0) {
    # Proceed with meta-analysis
    data_time$smc <- (data_time[[time_point_mean]] - data_time[[mean_from]]) / data_time[[sd_from]]
    data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm
    
    # Random-Effects Meta-Analysis
    result_data <- tryCatch({
      res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
      data.frame("time_point" = time_label, "SMC" = res_re$beta, "lCI" = res_re$ci.lb, "uCI" = res_re$ci.ub)
    }, error = function(e) {
      print(paste("Error in rma():", e))
      data.frame("time_point" = time_label, "SMC" = NA, "lCI" = NA, "uCI" = NA)
    })
    
  } else {
    # If there are no valid rows even after imputation, return NA
    result_data <- data.frame("time_point" = time_label, "SMC" = NA, "lCI" = NA, "uCI" = NA)
  }
  
  # Return the result with the time point label
  row.names(result_data) <- NULL
  return(result_data)
}


## Analysis-----------------------

## SMC + CIs for Open Subset-----------------
open_1week <- meta_analysis_custom(data=open_subset, "Mean_Baseline", "SD_baseline", "Mean_1week", "SD_ 1week", "1")
open_2weeks <- meta_analysis_custom(data=open_subset, "Mean_1week", "SD_ 1week", "Mean_2weeks", "SD_2weeks", "2")
open_3weeks <- meta_analysis_custom(data=open_subset, "Mean_2weeks", "SD_2weeks", "Mean_3weeks", "SD_3weeks", "3")
open_4weeks <- meta_analysis_custom(data=open_subset, "Mean_3weeks", "SD_3weeks", "Mean_4weeks", "SD_4weeks", "4")
open_6weeks <- meta_analysis_custom(data=open_subset, "Mean_4weeks", "SD_4weeks", "Mean_6weeks", "SD_6weeks", "6")
open_12weeks <- meta_analysis_custom(data=open_subset, "Mean_6weeks", "SD_6weeks", "Mean_12weeks", "SD_12weeks", "12")
open_24weeks <- meta_analysis_custom(data=open_subset, "Mean_12weeks", "SD_12weeks", "Mean_24weeks", "SD_24weeks", "24")
open_52weeks <- meta_analysis_custom(data=open_subset, "Mean_24weeks", "SD_24weeks", "Mean_52weeks", "SD_52weeks", "52")
open_72weeks <- meta_analysis_custom(data=open_subset, "Mean_52weeks", "SD_52weeks", "Mean_72weeks", "SD_72weeks", "72")
open_104weeks <- meta_analysis_custom(data=open_subset, "Mean_72weeks", "SD_72weeks", "Mean_104weeks", "SD_104weeks", "104")

# Combine open data
open_smc_data <- rbind(open_1week, open_2weeks, open_3weeks, open_4weeks, open_6weeks, open_12weeks, open_24weeks,open_52weeks, open_72weeks, open_104weeks)
# Add time point
open_smc_data$time_point <- as.numeric(open_smc_data$time_point)

# Sort the data by time_point to ensure proper calculation
open_smc_data <- open_smc_data[order(open_smc_data$time_point), ]

# Calculate cumulative changes
open_smc_data$smc_cum <- cumsum(open_smc_data$SMC)
# Calculate cumulative lower CI
open_smc_data$cumulative_lCI <- cumsum(open_smc_data$lCI)
# Calculate cumulative upper CI
open_smc_data$cumulative_uCI <- cumsum(open_smc_data$uCI)
# Add intervention type
open_smc_data$intervention <- "Open"


## SMC + CIs for Endoscopic Subset----------------
endo_1week <- meta_analysis_custom(data=endo_subset, "Mean_Baseline", "SD_baseline", "Mean_1week", "SD_ 1week", "1")
endo_2weeks <- meta_analysis_custom(data=endo_subset, "Mean_1week", "SD_ 1week", "Mean_2weeks", "SD_2weeks", "2")
endo_3weeks <- meta_analysis_custom(data=endo_subset, "Mean_2weeks", "SD_2weeks", "Mean_3weeks", "SD_3weeks", "3")
endo_4weeks <- meta_analysis_custom(data=endo_subset, "Mean_2weeks", "SD_2weeks", "Mean_4weeks", "SD_4weeks", "4")
endo_6weeks <- meta_analysis_custom(data=endo_subset, "Mean_4weeks", "SD_4weeks", "Mean_6weeks", "SD_6weeks", "6")
endo_12weeks <- meta_analysis_custom(data=endo_subset, "Mean_6weeks", "SD_6weeks", "Mean_12weeks", "SD_12weeks", "12")
endo_24weeks <- meta_analysis_custom(data=endo_subset, "Mean_12weeks", "SD_12weeks", "Mean_24weeks", "SD_24weeks", "24")
endo_52weeks <- meta_analysis_custom(data=endo_subset, "Mean_24weeks", "SD_24weeks", "Mean_52weeks", "SD_52weeks", "52")
endo_72weeks <- meta_analysis_custom(data=endo_subset, "Mean_52weeks", "SD_52weeks", "Mean_72weeks", "SD_72weeks", "72")
endo_104weeks <- meta_analysis_custom(data=endo_subset, "Mean_72weeks", "SD_72weeks", "Mean_104weeks", "SD_104weeks", "104")

# Combine endo data
endo_smc_data <- rbind(endo_1week, endo_2weeks, endo_3weeks, endo_4weeks, endo_6weeks, endo_12weeks, endo_24weeks, endo_52weeks, endo_72weeks, endo_104weeks)
# Add time point
endo_smc_data$time_point <- as.numeric(endo_smc_data$time_point)

# Sort the data by time_point to ensure proper calculation
endo_smc_data <- endo_smc_data[order(endo_smc_data$time_point), ]

# Calculate cumulative changes
endo_smc_data$smc_cum <- cumsum(endo_smc_data$SMC)
# Calculate cumulative lower CI
endo_smc_data$cumulative_lCI <- cumsum(endo_smc_data$lCI)
# Calculate cumulative upper CI
endo_smc_data$cumulative_uCI <- cumsum(endo_smc_data$uCI)
# Add intervention type
endo_smc_data$intervention <- "Endoscopic"


# Plot-----------------

smc_dat <- rbind(open_smc_data, endo_smc_data) ## data for plotting
timepoints <- c(1,2,3,4,5,6,12,24) ## data for plotting

cols <- c("Open" = "lightblue", "Endoscopic" = "red3")

smc_dat %>% 
  filter(time_point %in% timepoints) %>%
  ggplot(aes(x = time_point, y = smc_cum, color = as.factor(intervention))) +
  geom_line(size=1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = cumulative_lCI, ymax = cumulative_uCI, fill=as.factor(intervention)), alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  labs(x = "Weeks", 
       y = "Cumulative Standardised Mean Change") +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.2, 0.1))

#ggsave("CTR_review/plots/BTCQ_smc_change_cumulative_03102024v2.jpeg", dpi=1200)



# Title: Create cumulative standardised mean change time series
# Date: 06/02/2025
# Authors: Olivia Hartrick & Rebecca Turner

#========================================================#
# 1 FUNCTIONS-----
#========================================================#

meta_analysis_combined <- function(data, 
                                   mean_from, sd_from, 
                                   time_point_mean, time_point_sd, 
                                   time_label) {
  
  # Load necessary libraries
  library(metafor)
  # Subset rows that have valid data for the specific time point
  data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
    
  # Compute the standardized mean change and its variance
  data_time$smc <- (data_time[[time_point_mean]] - data_time[[mean_from]]) / data_time[[sd_from]]
  data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm
  
  # Random-Effects Meta-Analysis
  res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
  
  # Calculate the sample size for this time point
  sample_size_col <- paste0(time_point_mean, "_samplesize")
  
  # Check if the sample size column exists before attempting to use it
  if (sample_size_col %in% colnames(data_time)) {
    sample_size <- mean(data_time[[sample_size_col]], na.rm = TRUE)  # mean gets 1 value
  } else {
    sample_size <- NA 
  }
  
  # Return result summary
  result_data <- data.frame("time_point" = time_label,
                            "sample_size" = sample_size,
                            "SMC" = res_re$beta, 
                            "lCI" = res_re$ci.lb, 
                            "uCI" = res_re$ci.ub)
  # Return the result with the time point label
  row.names(result_data) <- NULL
  return(result_data)
  }

#============================================================#
# 2 IMPORT DATA -----
#============================================================#

{
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


#=====================================================#
# 3 DATA PREP ----
#=====================================================#

## Impute means for missing values in open subset
dat1 <- open_subset

# Define the columns to work on (columns 11 to 31)
mean_columns <- colnames(dat1)[11:31]

# Loop over each column and replace NA with the weighted mean
for (col in mean_columns) {
  
  # Identify rows that have non-NA values in the column and N_In_Arm
  non_na_rows <- !is.na(dat1[[col]]) & !is.na(dat1$N_In_Arm)
  
  # Compute total sample size before imputation
  total_sample_size <- sum(dat1$N_In_Arm[non_na_rows], na.rm = TRUE)
  
  # Define new column name
  timepoint_sample <- paste0(col, "_samplesize")
  
  # Store sample size in new column
  dat1[[timepoint_sample]] <- total_sample_size
  
  # Calculate the weighted mean based on non-NA rows and N_In_Arm
  if (sum(non_na_rows) > 0) {  # Ensure there are rows with valid data
    weighted_mean <- sum(dat1[[col]][non_na_rows] * dat1$N_In_Arm[non_na_rows], na.rm = TRUE) / 
      sum(dat1$N_In_Arm[non_na_rows], na.rm = TRUE)
    
    # Replace NA values with the calculated weighted mean
    dat1[[col]][is.na(dat1[[col]])] <- weighted_mean
  }
}

# Check
str(dat1)

## Impute means for missing values in endoscopic subset
dat2 <- endoscopic_subset

# Define the columns to work on (columns 11 to 31)
mean_columns <- colnames(dat2)[11:31]

# Loop over each column and replace NA with the weighted mean
for (col in mean_columns) {
  
  # Identify rows that have non-NA values in the column and N_In_Arm
  non_na_rows <- !is.na(dat2[[col]]) & !is.na(dat2$N_In_Arm)
  
  # Compute total sample size before imputation
  total_sample_size <- sum(dat2$N_In_Arm[non_na_rows], na.rm = TRUE)
  
  # Define new column name
  timepoint_sample <- paste0(col, "_samplesize")
  
  # Store sample size in new column
  dat2[[timepoint_sample]] <- total_sample_size
  
  # Calculate the weighted mean based on non-NA rows and N_In_Arm
  if (sum(non_na_rows) > 0) {  # Ensure there are rows with valid data
    weighted_mean <- sum(dat2[[col]][non_na_rows] * dat2$N_In_Arm[non_na_rows], na.rm = TRUE) / 
      sum(dat2$N_In_Arm[non_na_rows], na.rm = TRUE)
    
    # Replace NA values with the calculated weighted mean
    dat2[[col]][is.na(dat2[[col]])] <- weighted_mean
  }
}

# Re-assign to open vs endoscopic subsets
open_subset <- dat1
endoscopic_subset <- dat2


#=================================================#
# 4 ANALYSIS----
#=================================================#

## SMC + CIs for Open Subset - NO MISSING DATA IMPUTATION

## 1 week---
## csmc
(open_1week <- meta_analysis_combined(data=open_subset, "Mean_Baseline", "SD_baseline", "Mean_1week", "SD_ 1week", "1"))

## 2 weeks---
## csmc
(open_2weeks <- meta_analysis_combined(data=open_subset, "Mean_1week", "SD_ 1week", "Mean_2weeks", "SD_2weeks", "2"))

## 3 weeks---
## csmc : 
(open_3weeks <- meta_analysis_combined(data=open_subset, "Mean_2weeks", "SD_2weeks", "Mean_3weeks", "SD_3weeks", "3"))

## 4 weeks---
## csmc : FROM 2 WEEKS to 4 WEEKS (3 weeks failed)
(open_4weeks <- meta_analysis_combined(data=open_subset, "Mean_3weeks", "SD_3weeks", "Mean_4weeks", "SD_4weeks", "4"))

## 6 weeks---
## csmc 
(open_6weeks <- meta_analysis_combined(data=open_subset, "Mean_4weeks", "SD_4weeks", "Mean_6weeks", "SD_6weeks", "6"))

## 12 weeks---
## csmc
(open_12weeks <- meta_analysis_combined(data=open_subset, "Mean_6weeks", "SD_6weeks", "Mean_12weeks", "SD_12weeks", "12"))

## 24 weeks---
## csmc
(open_24weeks <- meta_analysis_combined(data=open_subset, "Mean_12weeks", "SD_12weeks", "Mean_24weeks", "SD_24weeks", "24"))

## 52 weeks---
## csmc
(open_52weeks <- meta_analysis_combined(data=open_subset, "Mean_24weeks", "SD_24weeks", "Mean_52weeks", "SD_52weeks", "52"))

## 72 weeks---
## csmc
(open_72weeks <- meta_analysis_combined(data=open_subset, "Mean_52weeks", "SD_52weeks", "Mean_72weeks", "SD_72weeks", "72"))

## 104 weeks---
## csmc
(open_104weeks <- meta_analysis_combined(data=open_subset, "Mean_72weeks", "SD_72weeks", "Mean_104weeks", "SD_104weeks", "104"))

open_smc_data <- rbind(open_1week, open_2weeks, open_3weeks, open_4weeks, open_6weeks, open_12weeks, open_24weeks,open_52weeks, open_72weeks, open_104weeks)

# add time_point column
open_smc_data$time_point <- as.numeric(open_smc_data$time_point)

# Sort the data by time_point to ensure proper calculation
open_smc_data <- open_smc_data[order(open_smc_data$time_point), ]
open_smc_data

# Calculate cumulative SMC changes
open_smc_data$smc_cum <- cumsum(open_smc_data$SMC)


# Calculate Wald-based CI correction-----------------------

# Compute cumulative variance
open_smc_data <- open_smc_data %>%
  mutate(var_cum = cumsum((uCI - lCI)^2 / (2 * 1.96)^2))  # Approximate variance from CIs
# Compute upper and lower cumulative confidence intervals
open_smc_data <- open_smc_data %>% 
  mutate(lCI_wald = smc_cum - 1.96 * sqrt(var_cum),
         uCI_wald = smc_cum + 1.96 * sqrt(var_cum))

# create rows for missing data values---------------------
open_smc_data <- rbind(0, open_smc_data)

### PLOT WALD CIs
ggplot(open_smc_data, aes(x = time_point, y = smc_cum)) +
  geom_line(size = 1, color = "blue") +  
  geom_ribbon(aes(ymin = lCI_wald, ymax = uCI_wald), fill = "blue", alpha = 0.3) +  
  theme_minimal() +
  labs(x = "Weeks", y = "Cumulative Standardised Mean Change",
       title = "Cumulative SMC with Adjusted Confidence Intervals") +
  theme(legend.position = "bottom")


## Endoscopic Subset----------

## Raw SMC + CIs calculations

## 1 week---
## csmc
(endo_1week <- meta_analysis_combined(data=endoscopic_subset, "Mean_Baseline", "SD_baseline", "Mean_1week", "SD_ 1week", "1"))

## 2 weeks---
## csmc
(endo_2weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_1week", "SD_ 1week", "Mean_2weeks", "SD_2weeks", "2"))

## 3 weeks---
## csmc 
(endo_3weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_2weeks", "SD_2weeks", "Mean_3weeks", "SD_3weeks", "3"))

## 4 weeks---
(endo_4weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_3weeks", "SD_3weeks", "Mean_4weeks", "SD_4weeks", "4"))

## 6 weeks---
## csmc 
(endo_6weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_4weeks", "SD_4weeks", "Mean_6weeks", "SD_6weeks", "6"))

## 12 weeks---
## csmc
(endo_12weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_6weeks", "SD_6weeks", "Mean_12weeks", "SD_12weeks", "12"))

## 24 weeks---
## csmc
(endo_24weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_12weeks", "SD_12weeks", "Mean_24weeks", "SD_24weeks", "24"))

## 52 weeks---
## csmc
(endo_52weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_24weeks", "SD_24weeks", "Mean_52weeks", "SD_52weeks", "52"))

## 72 weeks---
## csmc - NOT RUN
(endo_72weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_52weeks", "SD_52weeks", "Mean_72weeks", "SD_72weeks", "72"))

## 104 weeks---
## csmc
(endo_104weeks <- meta_analysis_combined(data=endoscopic_subset, "Mean_72weeks", "SD_72weeks", "Mean_104weeks", "SD_104weeks", "104"))

endo_smc_data <- rbind(endo_1week, endo_2weeks, endo_3weeks, endo_4weeks, endo_6weeks, endo_12weeks, endo_24weeks, endo_52weeks, endo_104weeks)

# Add time points
endo_smc_data$time_point <- as.numeric(endo_smc_data$time_point)

# Sort the data by time_point to ensure proper calculation
endo_smc_data <- endo_smc_data[order(endo_smc_data$time_point), ]

# Calculate cumulative changes
endo_smc_data$smc_cum <- cumsum(endo_smc_data$SMC)

# Calculate Wald-based CI correction Endoscopic-----------------------

# Compute cumulative variance
endo_smc_data <- endo_smc_data %>%
  mutate(var_cum = cumsum((uCI - lCI)^2 / (2 * 1.96)^2))  # Approximate variance from CIs
# Compute upper and lower cumulative confidence intervals
endo_smc_data <- endo_smc_data %>% 
  mutate(lCI_wald = smc_cum - 1.96 * sqrt(var_cum),
         uCI_wald = smc_cum + 1.96 * sqrt(var_cum))


# create rows for missing data values---------------------
endo_smc_data <- rbind(0, endo_smc_data)

### PLOT WALD CIs
ggplot(endo_smc_data, aes(x = time_point, y = smc_cum)) +
  geom_line(size = 1, color = "red") +  
  geom_ribbon(aes(ymin = lCI_wald, ymax = uCI_wald), fill = "red", alpha = 0.3) +  
  theme_minimal() +
  labs(x = "Weeks", y = "Cumulative Standardised Mean Change",
       title = "Cumulative SMC with Wald Adjusted Confidence Intervals") +
  theme(legend.position = "bottom")


# identify which time_points in which dataset need dummy data
open_smc_data$time_point
endo_smc_data$time_point

# Add intervention names
open_smc_data$intervention <- "Open"
endo_smc_data$intervention <- "Endoscopic"

### Create dataset for plot
smc_dat <- rbind(open_smc_data, endo_smc_data)
str(smc_dat)
head(smc_dat)


# Save Cumulative SMC data from plot
#write.csv(smc_dat, "CTR_review/outputs-2025-02-06/model_data/SMC_from_previous_timepoint_20250206.csv", row.names = FALSE)

#==================================================#
# 5 PLOT (no smoothing) ----
#==================================================#

timepoints <- c(0,1,2,4,6,12,24,52) ## data for plotting

cols <- c("Open" = "lightblue", "Endoscopic" = "red3")

par(mfrow=c(1,1))
smc_dat %>% 
  filter(time_point %in% timepoints) %>%
  ggplot(aes(x = time_point, y = smc_cum, color = as.factor(intervention))) +
  geom_line(size=1) +
  geom_point(size = 1.2) +
  #geom_ribbon(aes(ymin = cumulative_lCI, ymax = cumulative_uCI, fill=as.factor(intervention)), alpha = 0.2) +
  geom_ribbon(aes(ymin = (lCI_wald), ymax = (uCI_wald), 
                  fill=as.factor(intervention)), alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 52, by = 2), limits = c(0, 52)) +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  labs(x = "Weeks", 
       y = "Cumulative Standardised Mean Change") +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")

#ggsave("CTR_review/outputs-2024-10-13/model_data/BTCQ_smc_change_cumulative_13102024.jpeg", dpi=1200)


#==================================================#
# 6 PLOT (with smoothing) ----
#==================================================#

# Create sequence of time points for smoother predictions
values <- tibble(time_point = seq(0, 52, 0.001))

# Reshape data
smc_dat_long <- smc_dat %>%
  pivot_longer(cols= c(smc_cum, lCI_wald, uCI_wald), names_to = "parameter") %>%
  mutate(parameter2 = paste0(intervention, parameter))

smooth_parameter <- function(param_name) {
  loess(value ~ time_point, 
        data = filter(smc_dat_long, parameter2 == param_name), 
        span = 0.4  # Lower span = follows data more closely
  ) %>%
    predict(newdata = values)
}


# Apply smoothing function to all required variables
smc_smooth <- values %>% mutate("ouci" = smooth_parameter("OpenuCI_wald"),
                                "olci" = smooth_parameter("OpenlCI_wald"),
                                "ope"  = smooth_parameter("Opensmc_cum"),
                                "euci" = smooth_parameter("EndoscopicuCI_wald"),
                                "elci" = smooth_parameter("EndoscopiclCI_wald"),
                                "epe" = smooth_parameter("Endoscopicsmc_cum"))

cols = c("Open" = "lightblue", "Endoscopic" = "red4")

smc_smooth <- smc_smooth %>%
  mutate(across(c(ouci, olci, ope, euci, elci, epe),
                ~ ifelse(time_point >= 0 & time_point < 0.001, 0, .)))

## PLOT------------------------------------------

smc_smooth %>% 
  
  ggplot(aes(x = time_point)) +
  
  # Open intervention - smoothed
  geom_point(aes(y = ope), size = 0.1, color = "lightblue") +
  geom_ribbon(aes(ymin = olci, ymax = ouci), fill = "lightblue", alpha = 0.3) +
  
  # Endoscopic intervention - smoothed
  geom_point(aes(y = epe), size = 0.1, color = "red3") +
  geom_ribbon(aes(ymin = elci, ymax = euci), fill = "red3", alpha = 0.3) +
  
  # Overlay `smc_cum` values
  geom_point(data = smc_dat, aes(x = time_point, y = smc_cum, color = intervention), 
             size = 2, shape = 16) +
  # Aesthetics
  theme_minimal() +
  labs(x = "Weeks", y = "Cumulative Standardised Mean Change") +
  scale_color_manual(values = cols) +
  xlim(c(0,52)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

#ggsave("CTR_review/outputs-2024-10-13/model_data/BTCQ_smc_change_cumulative_06022025.jpeg", dpi=600)



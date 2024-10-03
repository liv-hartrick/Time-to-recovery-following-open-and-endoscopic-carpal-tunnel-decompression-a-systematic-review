setwd("C:/Users/olivi/OneDrive/Desktop/Research/R_Folder")

{
  library(readxl)
  library(dplyr)
  library(metafor)
  library(readr)
  library(ggplot2)
}

BTCQ_combined <- read_csv("BTCQ_MA_v4.csv", na="")

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
  
  return(res_re)
  
  }



# Run meta-analysis for each time point combined

jpeg(file="SMC_2weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined, "Mean_2weeks", "SD_2weeks", "2")
dev.off()

jpeg(file="SMC_4weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined,"Mean_4weeks", "SD_4weeks", "4")
dev.off()

jpeg(file="SMC_6weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined,"Mean_6weeks", "SD_6weeks", "6")
dev.off()

jpeg(file="SMC_12weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined, "Mean_12weeks", "SD_12weeks", "12")
dev.off()

jpeg(file="SMC_24weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined,"Mean_24weeks", "SD_24weeks", "24")
dev.off()

jpeg(file="SMC_52weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=BTCQ_combined,"Mean_52weeks", "SD_52weeks", "52")
dev.off()





# Run meta-analysis for each time point - "OPEN" subset

jpeg(file="open_SMC_2weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=open_subset, "Mean_2weeks", "SD_2weeks", "2")
dev.off()

jpeg(file="open_SMC_4weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=open_subset,"Mean_4weeks", "SD_4weeks", "4")
dev.off()

jpeg(file="open_SMC_6weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=open_subset,"Mean_6weeks", "SD_6weeks", "6")
dev.off()

jpeg(file="open_SMC_12weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=open_subset, "Mean_12weeks", "SD_12weeks", "12")
dev.off()

jpeg(file="open_SMC_24weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=open_subset,"Mean_24weeks", "SD_24weeks", "24")
dev.off()

jpeg(file="open_SMC_52weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=open_subset,"Mean_52weeks", "SD_52weeks", "52")
dev.off()



# Run meta-analysis for each time point - "Endoscopic" subset

jpeg(file="endoscopic_SMC_2weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset, "Mean_2weeks", "SD_2weeks", "2")
dev.off()

jpeg(file="endoscopic_SMC_4weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset,"Mean_4weeks", "SD_4weeks", "4")
dev.off()

jpeg(file="endoscopic_SMC_6weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset,"Mean_6weeks", "SD_6weeks", "6")
dev.off()

jpeg(file="endoscopic_SMC_12weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset, "Mean_12weeks", "SD_12weeks", "12")
dev.off()

jpeg(file="endoscopic_SMC_24weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset,"Mean_24weeks", "SD_24weeks", "24")
dev.off()

jpeg(file="endoscopic_SMC_52weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=endoscopic_subset,"Mean_52weeks", "SD_52weeks", "52")
dev.off()

# Run meta-analysis for each time point - "nonoperative" subset

jpeg(file="nonoperative_SMC_2weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset, "Mean_2weeks", "SD_2weeks", "2")
dev.off()

jpeg(file="nonoperative_SMC_4weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset,"Mean_4weeks", "SD_4weeks", "4")
dev.off()

jpeg(file="nonoperative_SMC_6weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset,"Mean_6weeks", "SD_6weeks", "6")
dev.off()

jpeg(file="nonoperative_SMC_12weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset, "Mean_12weeks", "SD_12weeks", "12")
dev.off()

jpeg(file="nonoperative_SMC_24weeks.jpeg", width=9, height=13, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset,"Mean_24weeks", "SD_24weeks", "24")
dev.off()

jpeg(file="nonoperative_SMC_52weeks.jpeg", width=10, height=8, units="in", res=1200)
meta_analysis_re(data=nonoperative_subset,"Mean_52weeks", "SD_52weeks", "52")
dev.off()

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

ggsave("BTCQ_pooled_effects_size_SMC.jpeg", dpi=1200)

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

ggsave("BTCQ_pooled_effects_size_SMC_all.jpeg", dpi=1200)



###################################################################
## Perform a correlation Matrix
library("Hmisc")
BTCQ_combined2 <- BTCQ_combined %>% 
  filter(!is.na(Study_Author))
res <- rcorr(as.matrix(BTCQ_combined2[,c(15,17,21,23)]))
res


library(psych)
means_BTCQ <- BTCQ_combined2[,c(15,17,21,23)]
correlations <- psych::corr.test(x=means_BTCQ, use="pairwise.complete.obs")
print(psych::corr.p(correlations$r, n=4), short=FALSE)


# these packages need to be installed before running this script
install.packages(c("psychometric","rtf"))

# function

cortable <- function(x, data, docname){ 
  library(Hmisc)
  library(psychometric)
  library(rtf)
  library(psych)
  
  # Ensure 'data' and 'x' are matrices
  data <- as.matrix(data)
  x <- as.matrix(x)
  
  # Debug print to check the input
  print("Data and X converted to matrix")
  
  # Perform correlation test
  corr_test <- corr.test(data)
  print(corr_test, short=FALSE)
  
  # Perform rcorr
  rcorr_result <- rcorr(x)
  R <- rcorr_result$r
  p <- rcorr_result$P
  
  # Formatting correlations
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "#", ""))))
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]  # truncate correlations to two decimals
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  
  # Adding confidence intervals
  for (i in 1:length(R)) {
    ci <- round(CIr(r = corr_test$r[i], n = corr_test$n[i], level = .95), 2)
    Rnew[i] <- paste(Rnew[i], " [", ci[1], ", ", ci[2], "]", sep="")
  }
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  # Write to RTF file
  rtffile <- RTF(file=paste0(docname, ".doc"), width=11, height=8.5, font.size=11)
  addTable(rtffile, cbind(rownames(Rnew), Rnew), col.justify="C", header.col.justify="C")
  addText(rtffile, "# p < .10. * p < .05. ** p < .01. *** p < .001.")
  done(rtffile)
  
  print("File written successfully")
}

# Correct function call
cortable(x=means_BTCQ, data=means_BTCQ, docname="TEST_CorTable2")



# Run on filtered 4weeks
means_BTCQ %>% 
  dplyr::select(Mean_4weeks, Mean_6weeks) %>%
  drop_na() %>%
  cortable("CorTable_4wks") # replace "CorTable" with what you want the exported file to be called; do not remove quotation marks


########## GET CORRELATIONS OF SMCs and CONFIDENCE INTERVALS IN A TABLE

meta_analysis_table <- function(data,time_point_mean, time_point_sd, time_label) {
    
    # Subset rows that have NA for the specific time point
    data_time <- data[!is.na(data[[time_point_mean]]) & !is.na(data[[time_point_sd]]), ]
    
    # Compute the standardized mean change and its variance
    data_time$smc <- (data_time[[time_point_mean]] - data_time$Mean_Baseline) / data_time$SD_baseline
    data_time$var_smc <- (data_time$N_In_Arm + data_time$smc^2) / data_time$N_In_Arm 
    
    # Random-Effects Meta-Analysis
    res_re <- rma(yi = data_time$smc, vi = data_time$var_smc, method = "REML", data = data_time)
    
   
    return(data_time) 
    
  }

BCTQ_open_2week <- meta_analysis_table(data=open_subset, "Mean_2weeks", "SD_2weeks", "2")
BCTQ_open_2week <- BCTQ_open_2week[,c(27,28)]
colnames(BCTQ_open_2week)[2] <- "week2"

BCTQ_open_4week <- meta_analysis_table(data=open_subset, "Mean_4weeks", "SD_4weeks", "4")
BCTQ_open_4week <- BCTQ_open_4week[,c(27,28)] 
colnames(BCTQ_open_4week)[2] <- "week4"

BCTQ_open_6week <- meta_analysis_table(data=open_subset,"Mean_6weeks", "SD_6weeks", "6")  
BCTQ_open_6week <- BCTQ_open_6week[,c(27,28)] 
colnames(BCTQ_open_6week)[2] <- "week6"

BCTQ_open_12week <- meta_analysis_table(data=open_subset, "Mean_12weeks", "SD_12weeks", "12")
BCTQ_open_12week <- BCTQ_open_12week[,c(27,28)]
colnames(BCTQ_open_12week)[2] <- "week12"
 
BCTQ_open_24week <- meta_analysis_table(data=open_subset,"Mean_24weeks", "SD_24weeks", "24")
BCTQ_open_24week <- BCTQ_open_24week[,c(27,28)]
colnames(BCTQ_open_24week)[2] <- "week24"

BCTQ_open_52week <- meta_analysis_table(data=open_subset,"Mean_52weeks", "SD_52weeks", "52")
BCTQ_open_52week <- BCTQ_open_52week[,c(27,28)]
colnames(BCTQ_open_52week)[2] <- "week52"

temp <- full_join(BCTQ_open_2week, BCTQ_open_4week, by=c("author_arm"))
temp <- full_join(temp, BCTQ_open_6week, by="author_arm", relationship = "many-to-many")
temp <- full_join(temp, BCTQ_open_12week, by="author_arm", relationship = "many-to-many")
temp <- full_join(temp, BCTQ_open_24week, by="author_arm", relationship = "many-to-many")
open_smcs <- full_join(temp, BCTQ_open_52week, by="author_arm", relationship = "many-to-many")
open_smcs <- distinct(open_smcs)

# Results for OPEN subset - currently not saving to file for some reason.
cortable(data=open_smcs[,c(2:7)], "CorTable_SMCs_Open")


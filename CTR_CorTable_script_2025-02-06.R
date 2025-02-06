#====================================================#
## Title: Generate correlation coefficient table
## Date: 03/10/2024
##===================================================#


## Function to calculate and save correlation coefficients table

#============================================================================#
### DEFINE FUNCTION----
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
#==================================================================#

# Set working directory
setwd("CTR_review/")

# Import data----
data <- read_csv("outputs-2025-02-06/raw data/BTCQ_MA_v4.csv", na="")

## What are the names of interventions?
table(data$`Interventions`)

# Remove rows with all missing values
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

# get means data subsets for correlation coefficients
colnames(subset)
means_combined <- subset[,c(11,13,15,17,19,23,25,27,29,31)]
means_open <- open_subset[,c(11,13,15,17,19,23,25,27,29,31)]
means_endo <- endo_subset[,c(11,13,15,17,19,23,25,27,29,31)]

#means_BTCQ_operative <- operative_subset[,c(7,9,11,13,15,17)]

# Generate Correlation Matrix for Combined Subset
cortable(x=means_combined, data=means_combined, docname="outputs-2025-02-06/model_data/CTR_CorTable_BTCQ")

# Generate Correlation Matrix for Arthroplasty Subset
cortable(x=means_open, data=means_open, docname="outputs-2025-02-06/model_data/CTR_CorTable_BTCQ_Open")

# Generate Correlation Matrix for Trapeziectomy subset
cortable(x=means_endo, data=means_endo, docname="outputs-2025-02-06/model_data/CTR_CorTable_BTCQ_Endoscopic")


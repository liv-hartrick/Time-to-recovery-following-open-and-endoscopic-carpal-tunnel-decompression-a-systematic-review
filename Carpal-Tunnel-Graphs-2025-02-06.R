# Title: Plots of BCTQ and qDASH recovery scores following open and endoscopic carpal tunnel release
# Date: 06/02/2025
# Authors: Olivia Hartrick, Rebecca Turner

#### Library packages ####

{library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
}
setwd("/data/notebooks/rstudio-testbook/")

#### Load CSV files ####
BCTQ <- read_csv("CTR_review/outputs-2025-02-06/raw data/BTCQ_Graphs_021024.csv", na="")
qDASH <- read_csv("CTR_review/outputs-2025-02-06/raw data/qDASH_Graphs_v4.csv", na=)

#### Visualise plots ####
##### BCTQ #####
# Pivot and sort the data
BCTQ_pivot_new <- BCTQ %>%
  pivot_longer(cols = 7:18,  # Using column positions
               names_to = "Weeks", 
               values_to = "Value") %>%
  mutate(Weeks = as.numeric(gsub("[^[:digit:]]", "", Weeks)),
         Study_Arm = as.factor(Study_Arm)) %>%  # Convert Study Arm to factor
  arrange(Study_Arm, Weeks)

BCTQ_pivot_new <- BCTQ_pivot_new[!is.na(BCTQ_pivot_new$Value),] #removing lines with NA in value

BCTQ_pivot_new$Intervention_Type <- as.factor(BCTQ_pivot_new$`Intervention(=arm)`) 

#Graph with all data
cols <- c("Open" ="lightblue", "Endoscopic" = "red3")

# Line per study
BCTQ_pivot_new %>% 
  filter(Intervention_Type == "Open" | Intervention_Type == "Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm, colour = factor(Intervention_Type))) +
  #geom_line(alpha=0.5, size = 1) + 
  geom_point(alpha=0.5, size = 1.2) +
  geom_smooth(method="loess", se=FALSE, size = 0.7) +
  #scale_fill_brewer(palette= "Set1") +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score")  +
  scale_colour_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.8))

# Save plot
#ggsave("CTR_review/outputs-2025-02-06/plots/BCTQ_graph_endovsopen_0602025.jpeg", dpi=600)


##### qDASH #####

# Pivot and sort the data
qDASH_pivot_new <- qDASH %>%
  pivot_longer(cols = 6:13,  # Using column positions
               names_to = "Weeks", 
               values_to = "Value") %>%
  mutate(Weeks = as.numeric(gsub("[^[:digit:]]", "", Weeks)),
         `Study_Arm` = as.factor(`Study_Arm`)) %>%  # Convert Study Arm to factor
  arrange(`Study_Arm`, Weeks)

qDASH_pivot_new<-qDASH_pivot_new[!is.na(qDASH_pivot_new$Value),] #removing lines with NA in value

qDASH_pivot_new$Intervention_Type <- as.factor(qDASH_pivot_new$Intervention_Type)

#Graph
cols <- c("Open" = "lightblue", "Endoscopic" = "red3", "Both" = "black")

# Line per study
qDASH_pivot_new %>% 
  filter(Intervention_Type == "Open" | Intervention_Type == "Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm, colour = factor(Intervention_Type))) +
  #geom_line(alpha=0.5, size = 1) + 
  geom_point(alpha=0.5, size = 1.2) +
  geom_smooth(method="loess", span=0.8, se=FALSE, size=0.7) +
  #scale_fill_brewer(palette= "Set1") +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score")  +
  scale_colour_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.8))

# Save the plot
#ggsave("CTR_review/outputs-2025-02-06/plots/qDASH_graph_endovsopen_06022025.jpeg", dpi=600)


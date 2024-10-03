setwd("C:/Users/olivi/OneDrive/Desktop/Research/R_Folder/")
#### Library packages ####

{library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(metafor)
  library(readxl)
  library(DACF)
  library(cowplot)
  library(grid)
  library(readr)
}

#### Load CSV files ####
BTCQ <- read_csv("BTCQ_Graphs_021024.csv", na="")
qDASH <- read_csv("qDASH_Graphs_v3.csv", na=)

#### Visualise plots ####
##### BTCQ #####
# Pivot and sort the data
BTCQ_pivot_new <- BTCQ %>%
  pivot_longer(cols = 7:18,  # Using column positions
               names_to = "Weeks", 
               values_to = "Value") %>%
  mutate(Weeks = as.numeric(gsub("[^[:digit:]]", "", Weeks)),
         Study_Arm = as.factor(Study_Arm)) %>%  # Convert Study Arm to factor
  arrange(Study_Arm, Weeks)  # Sort the data

BTCQ_pivot_new <- BTCQ_pivot_new[!is.na(BTCQ_pivot_new$Value),] #removing lines with NA in value

BTCQ_pivot_new$Intervention_Type <- as.factor(BTCQ_pivot_new$`Intervention(=arm)`) #Make this colunn a factor

#Graph with all data

cols <- c("Open" ="lightblue", "Endoscopic" = "red3")

BTCQ_pivot_new%>% 
  filter(Intervention_Type == "Open" | Intervention_Type == "Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm, colour = factor(Intervention_Type))) +
  geom_line(alpha=0.5, size = 1) + 
  geom_point(alpha=0.5, size = 1.2) +
  scale_fill_brewer(palette= "Set1") +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score")  +
  scale_colour_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.8))


ggsave("BCTQ_graph_endovsopen.jpeg", dpi=1200)

#+
  #facet_wrap("Intervention_Type", ncol = 1)





#Graphs by interventions
{
p1 <- BTCQ_pivot_new%>% 
  filter(Intervention_Type == "Open") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm)) +
  geom_line(color="darkred", alpha=0.5) + 
  geom_point(color="darkred", alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(0, 7.5, by = 2.5), limits = c(0, 7.5)) +
  scale_color_discrete(name = "Intervention Type", labels = "Open") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title="BTCQ Scores Over Time", x="Weeks", y="Score")

p2 <- BTCQ_pivot_new%>% 
  filter(Intervention_Type == "Open + Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm)) +
  geom_line(color="navy", alpha=0.5) + 
  geom_point(color="navy", alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(0, 7.5, by = 2.5), limits = c(0, 7.5)) +
  scale_color_discrete(name = "Intervention Type", labels = "Open + Endoscopic") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score")

p3 <- BTCQ_pivot_new%>% 
  filter(Intervention_Type == "Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm)) +
  geom_line(color="darkgreen", alpha=0.5) + 
  geom_point(color="darkgreen", alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(0, 7.5, by = 2.5), limits = c(0, 7.5)) +
  scale_color_discrete(name = "Intervention Type", labels = "Endoscopic") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs( x="Weeks", y="Score")

p4 <- BTCQ_pivot_new%>% 
  filter(Intervention_Type == "Non-operative") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm)) +
  geom_line(color="black", alpha=0.5) + 
  geom_point(color="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(0, 7.5, by = 2.5), limits = c(0, 7.5)) +
  scale_color_discrete(name = "Intervention Type", labels = "Non-operative") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs( x="Weeks", y="Score")
}

#Combine all graphs
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4),size = "last")) #combines the two graphs    


## Graphs by operative vs. non-operative ##
operative_v <- c("Endoscopic", "Open", "Open + Endoscopic")

BTCQ_pivot_new %>% 
  mutate(op_type = ifelse(Intervention_Type %in% operative_v, "Operative", "Non-operative")) %>%
 ggplot(aes(x=Weeks, y=Value, group=Study_Arm, colour = factor(op_type))) +
  geom_line(alpha=0.5, size = 1) + 
  geom_point(alpha=0.5, size = 1.2) +
  scale_colour_brewer(palette= "Set1") +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score")  +
  #scale_colour_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.8))

ggsave("BCTQ_graph_operative vs non operative.jpeg", dpi=1200)
## Graphs Intervention Type 2x2 Panel Plot ##

# Get dataset

# Order the factor levels for the plot order
BTCQ_pivot_new$Intervention_Type <- factor(BTCQ_pivot_new$Intervention_Type, levels = c("Open", "Non-operative", "Endoscopic", "Both"))

BTCQ_pivot_new %>% 
  # Ask it to plot, specify the x, y values; group=number of lines, colour=what group to colour by
  ggplot(aes(x=Weeks, y=Value, # specify x & y
             group=Study_Arm, # spec number of lines (by study arm)
             colour = factor(Intervention_Type))) + #spec colour by Intervention_Type
  # Specify that it's a line graph, alpha=transparency + size=thickness
  geom_line(alpha=0.5, size = 0.8) + 
  # Specify to add points, and their size
  geom_point(alpha=0.5, size = 1.2) +
  # Specify the x-axis scale (and what intervals on x)
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  # Specify the overall style of the whole plot
  theme_minimal() + #theme_bw(), theme_classic(), theme_light(), theme_dark
  # Specify plot text size
  theme(text = element_text(size = 10)) +
  # Specify what the label text is
  labs( x="Weeks", y="Score")  +
  # Specify the colours of the lines (note: order is important a-z)
  scale_colour_manual(values = c("lightblue", "mediumseagreen", "red3", "black")) +
  # Plot graphs by *intervention_type* separately, free_x = add x-axis labels to all plots
  facet_wrap(~Intervention_Type, scales = "free_x") +
  # Remove the legend
  theme(legend.position = "none")



ggsave("BCTQ_graph_pivot_graph_interventions.jpeg", dpi=1200)


##### qDASH #####
# Pivot and sort the data
qDASH_pivot_new <- qDASH %>%
  pivot_longer(cols = 6:10,  # Using column positions
               names_to = "Weeks", 
               values_to = "Value") %>%
  mutate(Weeks = as.numeric(gsub("[^[:digit:]]", "", Weeks)),
         `Study_Arm` = as.factor(`Study_Arm`)) %>%  # Convert Study Arm to factor
  arrange(`Study_Arm`, Weeks)  # Sort the data

qDASH_pivot_new<-qDASH_pivot_new[!is.na(qDASH_pivot_new$Value),] #removing lines with NA in value

qDASH_pivot_new$Intervention_Type <- as.factor(qDASH_pivot_new$Intervention_Type) #Make this column a factor

#GRaph for all interventions


cols <- c("Open" = "lightblue", "Endoscopic" = "red3", "Both" = "black")

qDASH_pivot_new %>% 
  filter(Intervention_Type == "Open" | Intervention_Type == "Endoscopic") %>% 
  ggplot(aes(x=Weeks, y=Value, group=Study_Arm, colour = factor(Intervention_Type))) +
  geom_line(alpha=0.5, size = 1) + 
  geom_point(alpha=0.5, size = 1.2) +
  scale_fill_brewer(palette= "Set1") +
  scale_x_continuous(breaks = seq(0, 13, by = 1), limits = c(0, 13)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_color_discrete(name = "Intervention Type") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(x="Weeks", y="Score") +
  scale_colour_manual(values = cols) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.8))
  



ggsave("DASH_graph_endovsopen_v25July2024.jpeg", dpi=1200)


#####





BTCQ_pivot_new%>% 
  ggplot(aes(x=Weeks, y=Value, group=Intervention_Type)) +
  geom_line() + 
  geom_point(color = "Intervention_Type") +
  scale_x_continuous(breaks = seq(0, 52, by = 4), limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(0, 7.5, by = 2.5), limits = c(0, 7.5)) +
  scale_color_discrete(name = "Intervention Type", labels = "Open") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title="BTCQ Scores Over Time", x="Weeks", y="Score")


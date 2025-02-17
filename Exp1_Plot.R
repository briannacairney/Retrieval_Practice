# Exp. 1 violin plots split by Group

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load data 
load("Data/FullData_Factors.Rdata")

# Calculate the mean T2_SCORE for each participant for the four T1_SCORE and Condition combinations
grouped_data <- FullData %>%
  group_by(SubID, T1_SCORE, Condition, Group) %>%  # Make sure to include Group in the grouping
  summarize(mean_T2_SCORE = mean(T2_SCORE, na.rm = TRUE)) %>%
  ungroup() 

# Calculate the means for T1_Condition and Group
mean_se_data <- grouped_data %>%
  group_by(Condition, T1_SCORE, Group) %>%
  summarize(mean_T2_SCORE = mean(mean_T2_SCORE), .groups = 'drop')

# Create long format data frame but keep individual subject values
FullDataLong <- FullData %>%
  select(SubID, Condition, Group, T1_SCORE, T2_SCORE) %>%
  group_by(SubID, Group, Condition, T1_SCORE) %>%
  summarise(
    ave_score = mean(T2_SCORE, na.rm = TRUE),
    sd_score = sd(T2_SCORE, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(
    sd_lower = ave_score - sd_score,
    sd_upper = ave_score + sd_score
  )

# Calculate Summary Statistics for Error Bars by Group
SummaryStats <- FullDataLong %>%
  group_by(T1_SCORE, Condition, Group) %>%
  summarise(
    Mean = mean(ave_score, na.rm = TRUE),
    SE = sd(ave_score, na.rm = TRUE) / sqrt(n()), # Standard error
    Lower = Mean - SE,
    Upper = Mean + SE,
    .groups = 'drop'
  )

# Calculate the mean T2_SCORE for each T1_SCORE, Condition, and Group combination
group_means <- grouped_data %>%
  group_by(T1_SCORE, Condition, Group) %>%
  summarize(mean_T2_SCORE = mean(mean_T2_SCORE), .groups = 'drop')

# Custom colors (colorblind-friendly!)
CustomColors <- c("AUT" = "#0C7BDC", "NONAUT" = "#FFC20A")

# Define a dodge position for both violins and points
dodge <- position_dodge(width = 0.9)

# Edit labels
T1_SCORE_new_labels <- function(T1_SCORE){
  ifelse(T1_SCORE == 0, "Incorrect at T1", "Correct at T1")
}
Condition_new_labels <- function(Condition){
  ifelse(Condition == "FB", "Feedback", "No Feedback")
}


# Plot
Exp1_Plot_Groups <- ggplot(grouped_data, aes(x = Condition, y = mean_T2_SCORE, color = Group)) +
  geom_violin(position = dodge) +
  geom_point(aes(color = Group), position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.3), size = 2, alpha = 0.8) +  # Color based on Group
  geom_point(data = group_means, aes(x = Condition, y = mean_T2_SCORE, group = Group), color = "black", size = 3, position = dodge) +  # Black points for means
  geom_errorbar(
    data = SummaryStats,
    aes(x = Condition, ymin = Lower, ymax = Upper, y = Mean, group = Group),
    width = 0.2,
    position = dodge,
    color = "black"
  ) +  # Error bars
  facet_grid(~ T1_SCORE, labeller = labeller(T1_SCORE = T1_SCORE_new_labels)) +
  scale_color_manual(values = CustomColors) +  # Apply custom colors
  scale_x_discrete(labels = Condition_new_labels) +  # Apply custom x-axis labels
  labs(x = NULL, y = "T2 recall performance") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(vjust = 2),
    axis.text.x = element_text(size = 14, face = "bold", color = "black"), # x-axis for Condition
    axis.text.y = element_text(size = 14, face = "bold", color = "black"), # y-axis for T2 score (0 - 1)
    plot.title = element_text(size = 16, face = "bold", color = "black"),
    strip.text = element_text(size = 14, face = "bold", color = "black")
  ) +
  ggtitle("Experiment 1")
Exp1_Plot_Groups

# Save
# ggsave(plot = Exp1_Plot_Groups, file = "Plots/Exp1_Plot_Groups.png", bg = "white", width = 9, height = 7, dpi = 300, units = "in")

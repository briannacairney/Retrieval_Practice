# Exp. 2 plot

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load data 
load("Data/Exp2_FullData_Factors.Rdata")
source("Functions/StandardError.R")

# PREP DATA FOR PLOTTING
# Create long format data frame but keep individual subject values
FullDataLong <- FullData |>
  select(PROLIFIC_PID, Condition, Group, PracAccuracy, SCORE) |>
  pivot_longer(cols = c("PracAccuracy", "SCORE"),
               names_to = "TestTimepoint", values_to = "Score") |>
  group_by(PROLIFIC_PID, Group, Condition, TestTimepoint) |>
  summarise(
    ave_score = mean(Score, na.rm = TRUE),
    se_score = standard_error(Score), 
    .groups = 'drop'
  ) |>
  mutate(
    se_lower = ave_score - se_score,
    se_upper = ave_score + se_score
  ) 

# Calculate summary statistics 
sumStats <- FullData %>%
  select(PROLIFIC_PID, Condition, Group, PracAccuracy, SCORE) |>
  pivot_longer(cols = c("PracAccuracy", "SCORE"),
               names_to = "TestTimepoint", values_to = "Score") |>
  group_by(Condition, Group, TestTimepoint) |>
  summarise(
    ave_score_All = mean(Score, na.rm = TRUE),
    se_score_All = standard_error(Score), 
    .groups = 'drop'
  ) |>
  mutate(
    se_lower = ave_score_All - se_score_All,
    se_upper = ave_score_All + se_score_All
  ) |>
  filter(!is.na(Group))

# Add summary statistics to FullDataLong
FullDataLong <- FullDataLong %>%
  left_join(
    sumStats %>%
      select(Group, Condition, TestTimepoint, ave_score_All, se_score_All), 
    by = c("Group", "Condition", "TestTimepoint")
  ) %>%
  rename(OverallMean = ave_score_All,
         OverallSE = se_score_All)
# Add new SE for plots
FullDataLong$Plot_SE_Lower = FullDataLong$OverallMean - FullDataLong$OverallSE
FullDataLong$Plot_SE_Upper = FullDataLong$OverallMean + FullDataLong$OverallSE

# Update labels for plot
FullDataLong <- FullDataLong %>% 
  mutate(TestTimepoint = ifelse(TestTimepoint == "PracAccuracy",
                                "T1",
                                "T2"),
         Condition = ifelse(Condition == "FB", 
                            "Feedback", 
                            "No feedback"),
         Group = ifelse(Group == "Aut", 
                        "Autistic", 
                        "Non-autistic"))

# PLOT DATA with individual data points 
Exp2_Plot <- ggplot(data = FullDataLong, aes(x = TestTimepoint, y = ave_score, color = Group, group = Group)) + 
  geom_jitter(position = position_dodge(width = 0.5), # Dodge groups side by side
              # width = 0.2,  # Add slight horizontal jitter
              size = 2) +   # Adjust size for visibility
  
  # Optional: Add standard error bars
  geom_errorbar(aes(x = TestTimepoint, y = ave_score, ymin = Plot_SE_Lower, ymax = Plot_SE_Upper),
                color = "black",
                position =  position_dodge(width = 0.5),
                width = 0.5) +
  
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_dodge(width = 0.5)) +
  
  # Use "expand" to add space on the ends of the x-axis
  scale_x_discrete(expand = c(0.5, 0.5)) +
  
  # Faceting by Condition
  facet_wrap(~ Condition, strip.position = "bottom") +
  
  # Customizing theme
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        legend.position = "bottom",
        strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "black")
  ) + 
  labs(fill = "", x = "", y = "Mean recall score") 

Exp2_Plot

# Save plot
# ggsave(plot = Exp2_Plot, file = "Plots/Exp2_Plot.png", bg = "white")







# PLOT DATA AS BAR PLOT
# Convert data to long format and summarize with mean and standard error values for score
long <- FullData |>
  select(PROLIFIC_PID, Condition, Group, PracAccuracy, SCORE) |>
  pivot_longer(cols = c("PracAccuracy", "SCORE"),
               names_to = "TestTimepoint", values_to = "Score") |>
  group_by(Condition, Group, TestTimepoint) |>
  summarise(
    ave_score = mean(Score, na.rm = TRUE),
    se_score = standard_error(Score), 
    .groups = 'drop'
  ) |>
  mutate(
    se_lower = ave_score - se_score,
    se_upper = ave_score + se_score
  ) |>
  filter(!is.na(Group))

# Update labels for plot
long <- long %>% 
  mutate(TestTimepoint = ifelse(TestTimepoint == "PracAccuracy",
                                "T1",
                                "T2"),
         Condition = ifelse(Condition == "FB", 
                            "Feedback", 
                            "No feedback"),
         Group = ifelse(Group == "Aut", 
                        "Autistic", 
                        "Non-autistic"))

# Bar plot
Exp2_BarPlot <- long |>
  
  ggplot(aes(TestTimepoint, ave_score, fill = Group)) + 
  geom_col(position = "dodge",
           width = .75) + 
  
  # Add standard error bars
  geom_errorbar(aes(ymin = se_lower, ymax = se_upper), position = position_dodge(0.75), width = .5) + 
  
  # Use "expand" to add space on the ends of the axis
  scale_x_discrete(expand = c(.5, .5)) +
  
  # Manually set the fill colors
  scale_fill_manual(values = c("blueviolet", "darkgoldenrod1")) +
  
  # Plot aethetics 
  # Use "strip.position" to move the facet title
  facet_wrap(~ Condition, strip.position = "bottom") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        legend.position = "bottom",
        strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "black")
  ) + 
  labs(fill = "", x = "", y = "Mean recall score") 

Exp2_BarPlot

# Save plot
# ggsave(plot = Exp2_BarPlot, file = "Plots/Exp2_BarPlot.png", bg = "white")

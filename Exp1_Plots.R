# Exp. 1 plot

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load data 
load("Data/FullData_Factors.Rdata")
source("Functions/StandardError.R")



# Convert data to long format and summarize with mean and standard error values for score
long <- FullData |>
  select(SubID, Condition, Group, T1_SCORE, T2_SCORE) |>
  pivot_longer(cols = c("T1_SCORE", "T2_SCORE"),
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



# Plot
Exp1_Plot <- long |>
  
  # Update labels for plot
  mutate(TestTimepoint = ifelse(TestTimepoint == "T1_SCORE",
                                "T1",
                                "T2"),
         Condition = ifelse(Condition == "FB", 
                            "Feedback", 
                            "No feedback"),
         Group = ifelse(Group == "AUT", 
                        "Autistic", 
                        "Non-autistic")) |>
  
  ggplot(aes(TestTimepoint, ave_score, fill = Group)) + 
  geom_col(position = "dodge",
           width = .75) + 
  
  # Add standard error bars
  # geom_errorbar(aes(x = TestTimepoint, y = ave_score, ymin = se_lower, ymax = se_upper)) +

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

Exp1_Plot

# Save plot
ggsave(plot = Exp1_Plot, file = "Plots/Exp1_Plot.png", bg = "white")



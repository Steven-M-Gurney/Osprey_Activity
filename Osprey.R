###############################################################################
# Osprey Activity Summary 
# Author: Steven M. Gurney
# Last updated: 01 JAN 2026
#
# Purpose:
#   • Import DTW wildlife-activity report data
#   • Summarize the proportion of osprey observations by year
#   • Estimate mean osprey count per observation with 95% CI
#   • Produce and save two publication-ready figures
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(tidyverse)
#library(date)      # <-- included from original script
library(ggplot2)


# =============================================================================
# 📁 1. Load & Prepare Activity Data
# =============================================================================

# Read in wildlife-activity dataset
activity <- read.csv("Wildlife_ActivityData_14Nov2024.csv") %>%
  
  # Keep only necessary fields
  dplyr::select(
    objectid,
    species_name,
    activity_year,
    num_observed
  ) %>%
  
  # Rename fields to simpler labels
  rename(
    ID      = objectid,
    Species = species_name,
    Year    = activity_year,
    Count   = num_observed
  ) %>%
  
  # Add report-type identifier
  add_column(ReportType = "Activity") %>%
  
  # Remove rows missing count values
  filter(!is.na(Count))


# =============================================================================
# 📊 2. Proportion of Osprey Observations by Year
# =============================================================================

# Count osprey vs. total activity records per year
osprey_by_year <- activity %>%
  group_by(Year) %>%
  summarise(
    Total_Count  = n(),                          # total activity entries
    Osprey_Count = sum(Species == "OSPREY"),     # osprey-only entries
    .groups = "drop"
  ) %>%
  mutate(
    Percent_Osprey = (Osprey_Count / Total_Count) * 100
  )

# Create line plot of osprey observation proportion by year
osprey1 <- ggplot(osprey_by_year,
                  aes(x = Year, y = Percent_Osprey)) +
  geom_line(color = "firebrick4", size = 2) +
  geom_point(color = "firebrick4", size = 5) +
  labs(
    x = "Year",
    y = "Osprey observations (% of total)"
  ) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.text.x  = element_text(size = 12, color = "black")
  )

# Preview plot
osprey1 

# Save proportional-observations plot
ggsave(
  "plot_osprey_percent_of_obsevations.png",
  plot   = osprey1,
  width  = 6.5,
  height = 5,
  dpi    = 300
)


# =============================================================================
# 🔁 3. Osprey Count Distribution by Year
# =============================================================================

# Restrict dataset to osprey entries only
activity_osprey <- activity %>%
  filter(Species == "OSPREY")

# Estimate mean counts and 95% CI by year
summary_data <- activity_osprey %>%
  group_by(Year) %>%
  summarise(
    Mean_Count = mean(Count),
    SE_Count   = sd(Count) / sqrt(n()),
    Lower_CI   = Mean_Count - 1.96 * SE_Count,
    Upper_CI   = Mean_Count + 1.96 * SE_Count,
    .groups = "drop"
  )

# Create point-estimate plot with confidence intervals
osprey2 <- ggplot(summary_data,
                  aes(x = as.factor(Year),
                      y = Mean_Count,
                      color = as.factor(Year))) +
  geom_point(size = 5) +
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI),
    width = 0.2,
    size  = 1
  ) +
  scale_color_manual(
    values = c(
      "2022" = "skyblue4",
      "2023" = "palegreen4",
      "2024" = "palevioletred4"
    )
  ) +
  labs(
    x = "Year",
    y = "Osprey per observation",
    color = "Year"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.text.x  = element_text(size = 12, color = "black"),
    plot.title   = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "none"
  )

# Preview plot
osprey2

# Save mean-count plot
ggsave(
  "plot_osprey_per_obsevations.png",
  plot   = osprey2,
  width  = 6.5,
  height = 5,
  dpi    = 300
)

###############################################################################
# End of Script
###############################################################################

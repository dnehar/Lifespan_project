# Supplementary_Fig_9a.R

# This script is designed to process and analyze data for Supplementary Figure 9a.
# It includes various sections, each with distinct purposes and functionalities.

# Load necessary libraries
library(ggplot2) # For data visualization
library(dplyr)  # For data manipulation

# Set the working directory
setwd("/path/to/directory") # Change this to your working directory

# Load data
# Reads the data from a CSV file. Make sure the file path is correct.
data <- read.csv("/path/to/data.csv")

# Data cleaning
# This section performs necessary cleaning steps such as handling missing values and filtering data.
data <- data %>%
  filter(!is.na(value)) %>% # Remove rows with NA values in the 'value' column
  mutate(category = factor(category)) # Convert 'category' column to a factor

# Data analysis
# Analyze the data and summarize key statistics.
summary_stats <- data %>%
  group_by(category) %>%
  summarize(mean_value = mean(value), sd_value = sd(value))

# Data visualization
# Create a plot to visualize the results.
ggplot(data, aes(x = category, y = value)) +
  geom_boxplot() + # Use boxplot to show the distribution of values
  theme_minimal() +
  labs(title = "Supplementary Figure 9a", x = "Category", y = "Value")

# Save plot
# Save the generated plot to the working directory.
ggsave("Supplementary_Fig_9a_plot.png") # Change the file name as needed
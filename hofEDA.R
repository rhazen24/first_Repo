# HOF EDA
# Load necessary libraries
library(tidyverse)

# Set the file path
file_path <- '/Users/rj_hazen/Documents/GitHub/first_Repo/HallOfFame.csv'

# Read the data
hall_of_fame <- read.csv(file_path)

# View the first few rows of the dataset
head(hall_of_fame)

# Summary of the dataset
summary(hall_of_fame)

# Structure of the dataset
str(hall_of_fame)

# Check for missing values
sum(is.na(hall_of_fame))

# Basic statistical analysis
hall_of_fame %>%
  select_if(is.numeric) %>%
  summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))

# Visualizations
# Histograms for numeric variables
numeric_vars <- hall_of_fame %>% select_if(is.numeric)
for (var in names(numeric_vars)) {
  ggplot(hall_of_fame, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle(paste("Histogram of", var)) +
    xlab(var) +
    ylab("Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Boxplots for numeric variables
for (var in names(numeric_vars)) {
  ggplot(hall_of_fame, aes_string(y = var)) +
    geom_boxplot(fill = "orange", color = "black") +
    theme_minimal() +
    ggtitle(paste("Boxplot of", var)) +
    ylab(var) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Scatter plot for pairs of numeric variables
# Adjust the pair of variables as needed
ggplot(hall_of_fame, aes(x = variable1, y = variable2)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Scatter plot of variable1 vs variable2") +
  xlab("variable1") +
  ylab("variable2")

# Correlation plot
# Requires the corrplot package
library(corrplot)
numeric_corr <- cor(numeric_vars, use = "complete.obs")
corrplot(numeric_corr, method = "circle")

#---------------------------------------------------------------------------------
#script Name: data_manipul.R
#Purpose: Using a data frame as an example, write a short code to illustrate some functions or packages for data processing.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-03-22
#---------------------------------------------------------------------------------

#Import and save data
library(xlsx)
data <- read.xlsx("D://吴凤箫//homework_2024//吴凤箫+homework2024//studentgrades.xlsx",1)

#Inspect data structure

str(data)

# Check whether a column or row has missing data
# Check for missing values in a specific column

any(is.na(data$column_name))

# Check for missing values in a specific row

row_has_missing <- apply(data, 1, function(row) any(is.na(row)))

#Extract values from a column or select/add a column
# Extract values from a column

column_values <- data$column_name

# Select a column
library(tidyverse)
selected_column <- select(data, Science)

# Add a new column

library(dplyr)
data <- mutate(data, sum=Science+Social.Studies)

#Transform a wider table to a long format

library(tidyr)
data$Science <- as.character(data$Science)
data$Social.Studies <- as.character(data$Social.Studies)
data$Math <- as.character(data$Math)
data$sum <- as.character(data$sum)
long_data <- pivot_longer(data, cols =-StudentID, names_to = "variable_name", values_to = "value")

# 6) Visualize the data
# Example plot using ggplot2
library(ggplot2)
ggplot(data, aes(x = First, y = Math)) +
  geom_point() +
  labs(title = "Studentgrade", x = "Firstname", y = "Mathgrades")

# Save the modified data
write.csv(data, "modified_data.csv")

# global.R - Loads libraries and data
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# Read the CSV file
# Replace "path/to/your/reports.csv" with the actual path to your CSV file
# The CSV should have columns: year, report_type, report_count
report_data <- read_csv("/Users/lydiagibson/Documents/Education/GitHub-repos-for-courses/shiny-tiny-hackathon/tidy_faers_dataset.csv") %>%
  # Ensure report_type is a factor for proper filtering
  mutate(report_type = factor(report_type, levels = unique(report_type)))

# In case your CSV doesn't match exactly, you might need to transform it
# Example if your CSV has a different structure:
# report_data <- read_csv("path/to/your/data.csv") %>%
#   rename(year = Year, report_type = Type, report_count = Count) %>%
#   mutate(report_type = factor(report_type))

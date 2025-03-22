library(tidyverse)
# Increase n to create a bigger dataset (e.g., 100k rows)
set.seed(123)
n <- 100000  # Adjust this number to increase/decrease dataset size

# Define possible values
drugs <- c("Drug A", "Drug B", "Drug C", "Drug D", "Drug E")
report_types <- c("Expedited", "Non-Expedited", "BSR", "Direct")
seriousness_levels <- c("Serious", "Non-serious")
years <- 2010:2023
quarters <- c("Q1", "Q2", "Q3", "Q4")

# Create a large data frame
big_data <- data.frame(
  ReportID         = seq_len(n),
  DrugName         = sample(drugs, n, replace = TRUE),
  ReportType       = sample(report_types, n, replace = TRUE, 
                            prob = c(0.3, 0.3, 0.2, 0.2)),
  Seriousness      = sample(seriousness_levels, n, replace = TRUE, 
                            prob = c(0.4, 0.6)),
  DeathCategory    = sample(c("Death", "No Death"), n, replace = TRUE,
                            prob = c(0.1, 0.9)),
  Year             = sample(years, n, replace = TRUE),
  Quarter          = sample(quarters, n, replace = TRUE),
  # Example numeric value (e.g., how many individual “sub-reports” might be grouped)
  NumberOfReports  = sample(1:100, n, replace = TRUE)
)

# Peek at the first few rows
head(big_data)

# Quick summary of the dataset
summary(big_data)


write_csv(x = big_data,file = "data.csv")

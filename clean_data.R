library(readxl)
library(tidyverse)
library(janitor)

# Step 1: Load your Excel file (assuming first sheet)
file_path <- "/Users/lydiagibson/Documents/Education/GitHub-repos-for-courses/shiny-tiny-hackathon/FAERS_data.xlsx"
raw_data <- read_xlsx(file_path)

# Step 2: Inspect the data structure
glimpse(raw_data)
head(raw_data)

# Step 3: Clean column names (lowercase + snake_case)
data_clean <- raw_data %>%
  clean_names()  # janitor::clean_names() standardizes column names

# Step 4: Change data type

library(dplyr)

data_clean |>
  dplyr::mutate(
    report_count = as.integer(report_count),
    dplyr::across(
      c("report_type","year"),
      as.factor
    )
  )


# Final: View the tidied data
glimpse(data_clean)

#Save as csv

# Save your cleaned dataset as CSV
write_csv(data_clean, "tidy_faers_dataset.csv")


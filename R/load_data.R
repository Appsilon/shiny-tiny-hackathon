library(readxl)
library(dplyr)

# Module for Loading FAERS Data
mod_load_data <- function(input, output, session) {
  load_faers_data <- function() {
    file_path <- "data/faers_data_long_format.xlsx"
    df <- read_excel(file_path, sheet = 1) %>%
      rename(Year = `Year`, Report_Type = `Report Type`, Report_Count = `Report Count`)
    
    # Convert Year to numeric (if needed)
    df <- df %>% mutate(Year = as.numeric(Year))
    
    return(df)
  }
  
  # Reactive data object
  faers_data <- reactive({ load_faers_data() })
  
  return(faers_data)
}
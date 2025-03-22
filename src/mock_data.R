# Script created with Perplexity AI and Claude Sonnet
#
# This script generates synthetic data for the FDA Adverse Event Reporting System (FAERS) Dashboard
# The data mimics patterns from the real FAERS data with increasing reports over time as shown
# in the historical FAERS data from 1968 to 2024

# Load required packages
library(tidyverse)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Function to create mock FAERS data
create_mock_faers_data <- function(n_reports = 150000) {
  # Create date sequence from 1968 to 2024
  start_year <- 1968
  end_year <- 2024
  years <- start_year:end_year
  
  # Define drug names
  drug_names <- c(
    "Acetaminophen", "Ibuprofen", "Atorvastatin", "Lisinopril", 
    "Metformin", "Levothyroxine", "Simvastatin", "Omeprazole",
    "Amlodipine", "Metoprolol", "Albuterol", "Sertraline",
    "Gabapentin", "Hydrochlorothiazide", "Losartan", "Fluoxetine",
    "Amoxicillin", "Azithromycin", "Ciprofloxacin", "Prednisone",
    "Alprazolam", "Zolpidem", "Tramadol", "Furosemide",
    "Citalopram", "Escitalopram", "Warfarin", "Clopidogrel",
    "Montelukast", "Rosuvastatin", "Pantoprazole", "Doxycycline"
  )
  
  # Define adverse events
  adverse_events <- c(
    "Headache", "Nausea", "Dizziness", "Fatigue", "Rash", 
    "Abdominal pain", "Vomiting", "Diarrhea", "Insomnia",
    "Joint pain", "Fever", "Cough", "Chest pain", "Anxiety",
    "Depression", "Muscle pain", "Shortness of breath", "Constipation",
    "Blurred vision", "Dry mouth", "Increased blood pressure", "Palpitations",
    "Edema", "Weight gain", "Weight loss", "Loss of appetite",
    "Confusion", "Tremor", "Somnolence", "Pruritus",
    "Hyponatremia", "Hyperkalemia", "Anaphylaxis", "Anemia",
    "Thrombocytopenia", "Neutropenia", "Hepatotoxicity", "Nephrotoxicity"
  )
  
  # Define outcomes
  outcomes <- c(
    "Hospitalization", "Life-threatening", "Death", "Disability", 
    "Congenital anomaly", "Required intervention", "Other"
  )
  
  # Define report types based on Excel data
  report_types <- c("Expedited", "Non-Expedited", "Direct", "BSR")
  
  # Define reporter types based on Excel data
  reporter_types <- c("Consumer", "Healthcare Professional", "Not Specified", "Other")
  
  # Create sample dates with exponential growth over time
  # Based on the provided Excel data showing significant increase in reports over years
  year_weights <- exp(0.15 * (years - start_year))
  year_weights <- year_weights / sum(year_weights)
  
  # Sample years based on weights (to simulate increasing reports over time)
  sampled_years <- sample(years, n_reports, replace = TRUE, prob = year_weights)
  
  # Generate random dates within the sampled years
  random_days <- sample(0:364, n_reports, replace = TRUE)
  dates <- as.Date(paste0(sampled_years, "-01-01")) + random_days
  
  # Extract year and quarter for grouping
  extracted_years <- year(dates)
  extracted_quarters <- paste0(extracted_years, "-Q", quarter(dates))
  
  # Generate age values with realistic distribution
  ages <- sample(c(NA, 0:100), n_reports, replace = TRUE, 
                 prob = c(0.05, # 5% missing
                          rep(0.001, 18), # 0-17 years (lower probability for pediatric)
                          rep(0.01, 47), # 18-64 years (adult - higher probability)
                          rep(0.008, 36))) # 65-100 years (geriatric - medium probability)
  
  # Create age groups based on FDA categorization
  age_groups <- case_when(
    is.na(ages) ~ "Unknown",
    ages < 18 ~ "Pediatric (<18)",
    ages >= 18 & ages < 65 ~ "Adult (18-64)",
    ages >= 65 ~ "Geriatric (65+)"
  )
  
  # Generate realistic weight distribution (in kg)
  # More missing values for weight than age
  weights <- sample(c(NA, 3:150), n_reports, replace = TRUE, 
                    prob = c(0.2, rep(0.8/148, 148)))
  
  # Generate sex distribution
  sexes <- sample(c("Male", "Female", "Unknown"), n_reports, replace = TRUE, 
                  prob = c(0.47, 0.49, 0.04))
  
  # Sample report types with changing distribution over time
  # Earlier years have more Non-Expedited, later years have more Expedited
  report_type_probs <- function(year) {
    if(year < 1980) return(c(0.05, 0.75, 0.19, 0.01)) # Expedited, Non-Expedited, Direct, BSR
    if(year < 1990) return(c(0.15, 0.70, 0.14, 0.01))
    if(year < 2000) return(c(0.30, 0.55, 0.14, 0.01))
    if(year < 2010) return(c(0.50, 0.35, 0.14, 0.01))
    return(c(0.60, 0.30, 0.09, 0.01)) # Most recent distribution
  }
  
  sampled_report_types <- rep(NA, n_reports)
  for(i in 1:n_reports) {
    probs <- report_type_probs(sampled_years[i])
    sampled_report_types[i] <- sample(report_types, 1, prob = probs)
  }
  
  # Sample reporter types with changing distribution over time
  # Earlier years more Consumer, later years more balanced
  reporter_type_probs <- function(year) {
    if(year < 1980) return(c(0.70, 0.29, 0.01, 0.00)) # Consumer, Healthcare Professional, Not Specified, Other
    if(year < 1990) return(c(0.65, 0.34, 0.01, 0.00))
    if(year < 2000) return(c(0.40, 0.40, 0.20, 0.00))
    if(year < 2010) return(c(0.35, 0.45, 0.20, 0.00))
    return(c(0.40, 0.50, 0.09, 0.01)) # Most recent distribution
  }
  
  sampled_reporter_types <- rep(NA, n_reports)
  for(i in 1:n_reports) {
    probs <- reporter_type_probs(sampled_years[i])
    sampled_reporter_types[i] <- sample(reporter_types, 1, prob = probs)
  }
  
  # Generate country data
  countries <- sample(
    c("United States", "Canada", "United Kingdom", "Germany", "France", 
      "Japan", "Australia", "Spain", "Italy", "Brazil", "Other"), 
    n_reports, 
    replace = TRUE,
    prob = c(0.70, 0.05, 0.05, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, 0.02, 0.03)
  )
  
  # Generate US state data (only for US reports)
  us_state_probs <- c(
    CA = 0.12, TX = 0.09, FL = 0.06, NY = 0.06, PA = 0.04, 
    IL = 0.04, OH = 0.03, GA = 0.03, NC = 0.03, MI = 0.03,
    NJ = 0.03, VA = 0.03, WA = 0.02, AZ = 0.02, MA = 0.02,
    TN = 0.02, IN = 0.02, MO = 0.02, MD = 0.02, CO = 0.02,
    MN = 0.02, SC = 0.02, AL = 0.01, LA = 0.01, KY = 0.01,
    OR = 0.01, OK = 0.01, CT = 0.01, UT = 0.01, IA = 0.01,
    NV = 0.01, AR = 0.01, MS = 0.01, KS = 0.01, NM = 0.01,
    NE = 0.01, WV = 0.01, ID = 0.01, HI = 0.01, NH = 0.01,
    ME = 0.01, MT = 0.01, RI = 0.01, DE = 0.01, SD = 0.01,
    ND = 0.01, AK = 0.01, DC = 0.01, VT = 0.01, WY = 0.01
  )
  
  states <- ifelse(
    countries == "United States",
    sample(names(us_state_probs), n_reports, replace = TRUE, prob = us_state_probs),
    NA
  )
  
  # Create the final mock dataset
  mock_faers_data <- tibble(
    report_id = paste0("R", sprintf("%07d", 1:n_reports)),
    date_received = dates,
    year = extracted_years,
    quarter = extracted_quarters,
    patient_age = ages,
    age_group = age_groups,
    patient_sex = sexes,
    patient_weight = weights,
    drug_name = sample(drug_names, n_reports, replace = TRUE),
    adverse_event = sample(adverse_events, n_reports, replace = TRUE),
    outcome = sample(outcomes, n_reports, replace = TRUE),
    report_type = sampled_report_types,
    reporter_type = sampled_reporter_types,
    country = countries,
    state = states
  )
  
  return(mock_faers_data)
}

# Create mock data
mock_faers_data <- create_mock_faers_data()

# Optional: Save the data for faster loading
# Uncomment to save the data
saveRDS(mock_faers_data, file = "./data/mock_faers_data.rds")

# Return the data for use in the app
mock_faers_data

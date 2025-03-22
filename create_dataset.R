# Load required packages
library(dplyr)
library(stringi)

# Define number of records
n <- 10000
set.seed(123)  # For reproducibility

# Generate the dataset
faers_simulated <- tibble(
  ID = sprintf("P%05d", 1:n),
  Year = sample(2010:2024, n, replace = TRUE),
  Gender = sample(c("Male", "Female", "Not specified"), n, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
  Age_group = sample(
    c("0 to 1 month", "2 months to 2 years", "3 to 11 years", "12 to 17 years", 
      "18 to 64 years", "65 to 85 years", "85+", "Not specified"),
    n, replace = TRUE,
    prob = c(0.01, 0.05, 0.10, 0.10, 0.50, 0.15, 0.05, 0.04)
  ),
  Seriousness = sample(c("Non-serious", "Serious", "Death"), n, replace = TRUE, prob = c(0.60, 0.35, 0.05)),
  Region = sample(c("Domestic", "Foreign", "Not specified"), n, replace = TRUE, prob = c(0.85, 0.10, 0.05)),
  Reporter = sample(c("Consumer", "Healthcare professional", "Other", "Not specified"), n, replace = TRUE, prob = c(0.50, 0.30, 0.15, 0.05)),
  Report_type = sample(c("BSR", "Direct", "Expedited", "Non-expedited"), n, replace = TRUE, prob = c(0.10, 0.20, 0.40, 0.30))
)

# Define ordered factor for Age_group
faers_simulated$Age_group <- factor(
  faers_simulated$Age_group,
  levels = c(
    "0 to 1 month",
    "2 months to 2 years",
    "3 to 11 years",
    "12 to 17 years",
    "18 to 64 years",
    "65 to 85 years",
    "85+",
    "Not specified"
  ),
  ordered = TRUE
)

# Simulate drug names
drug_list <- c("Paracetamol", "Ibuprofen", "Amoxicillin", "Atorvastatin", "Metformin",
               "Omeprazole", "Prednisone", "Azithromycin", "Cetirizine", "Formoterol")

faers_simulated$Drug <- sample(drug_list, nrow(faers_simulated), replace = TRUE, prob = c(
  0.20, 0.15, 0.12, 0.10, 0.10, 0.08, 0.08, 0.07, 0.05, 0.05
))


# Preview the first rows
#print(head(faers_simulated))

# To save the dataset for later use:
write.csv(faers_simulated, "faers_simulated.csv", row.names = FALSE)

library(dplyr)

set.seed(123) # For reproducibility

# Create base values using Poisson
base_data <- tibble::tibble(
  year = 1968:2024,
  
  # Common in all datasets - using rpois() for count data
  total = rpois(n = length(year), lambda = 500000),
  
  # by reporter type
  expedited = rpois(n = length(year), lambda = 300000),
  non_expedited = rpois(n = length(year), lambda = 300000),
  direct = rpois(n = length(year), lambda = 7000),
  bsr = rpois(n = length(year), lambda = 50),
  
  # by reporter
  consumer = rpois(n = length(year), lambda = 1000000),
  health_professional = rpois(n = length(year), lambda = 800000),
  not_specified_reporter = rpois(n = length(year), lambda = 60000),
  other_reporter = rpois(n = length(year), lambda = 50000),
  
  # by reporter region
  domestic = rpois(n = length(year), lambda = 1000000),
  foreign = rpois(n = length(year), lambda = 800000),
  not_specified_region = rpois(n = length(year), lambda = 60000),
  
  # by report seriousness
  serious = rpois(n = length(year), lambda = 1000000),
  death = rpois(n = length(year), lambda = 800000),
  non_serious = rpois(n = length(year), lambda = 60000),
  
  # by age group
  months_0_1 = rpois(n = length(year), lambda = 1000000),
  months_2_36 = rpois(n = length(year), lambda = 800000),
  years_3_11 = rpois(n = length(year), lambda = 60000),
  years_12_17 = rpois(n = length(year), lambda = 50000),
  years_18_64 = rpois(n = length(year), lambda = 40000),
  
  # by sex
  female = rpois(n = length(year), lambda = 1000000),
  male = rpois(n = length(year), lambda = 800000),
  not_specified_sex = rpois(n = length(year), lambda = 60000)
)

# Create custom growth factor:
# 1. Start at 0.25 in 1968 (quarter of the base values)
# 2. Gradually increase to above 1.0 in later years
years_range <- length(base_data$year)
growth_factor <- 0.25 + (1:years_range - 1) / (years_range - 1) * 1.75

# Apply the growth factor to all columns except year
mock_data <- base_data %>%
  mutate(across(.cols = -year, 
                .fns = function(x) round(x * growth_factor),
                .names = "{.col}"))

# Ensure the 'year' column remains unchanged
mock_data$year <- base_data$year

# Save to internal data
usethis::use_data(mock_data, internal = TRUE, overwrite = TRUE)
require(tidyverse)

set.seed(123)
years <- 2000:2024
report_types <- c("Expedited", "Non-Expedited", "Direct", "BSR")

mock_data <- expand.grid(
  Year = years,
  ReportType = report_types
) %>%
  mutate(Count = sample(1000:250000, n(), replace = TRUE))

total_reports <- mock_data %>%
  group_by(Year) %>%
  summarise(TotalReports = sum(Count)) %>%
  mutate(
    SeriousReports = round(TotalReports * runif(n(), 0.4, 0.7)),
    DeathReports = round(TotalReports * runif(n(), 0.05, 0.1))
  )

write_csv(mock_data, "data/mock_report_data.csv")
write_csv(total_reports, "data/mock_total_summary.csv")

create_grouped_data <- function(groups, filename_prefix, col_name) {

  data <- tidyr::crossing(
    Year = 2000:2024,
    !!sym(col_name) := groups
  ) %>%
    mutate(Count = sample(1000:250000, n(), replace = TRUE)) %>%
    pivot_wider(names_from = !!sym(col_name), values_from = Count, values_fill = 0) %>%
    mutate(TotalReports = rowSums(across(where(is.numeric))))
  
  write_csv(data, glue::glue("data/{filename_prefix}_summary.csv"))
  
  long_data <- tidyr::crossing(
    Year = 2000:2024,
    !!sym(col_name) := groups
  ) %>%
    mutate(Count = sample(1000:250000, n(), replace = TRUE))
  
  write_csv(long_data, glue::glue("data/{filename_prefix}_data.csv"))
  
}

create_grouped_data(c("Female", "Male", "Not specified"), "sex", "Sex")
create_grouped_data(c("0–1 month", "2 months–2 years", "3–11 years", "12–17 years", "18–64 years", "65–85 years", "More than 85 years", "Not specified"), "age", "AgeGroup")
create_grouped_data(c("Serious", "Death", "Non-serious"), "seriousness", "Seriousness")
create_grouped_data(c("Domestic", "Foreign", "Not specified"), "region", "Region")
create_grouped_data(c("Consumer", "Healthcare professional", "Not specified", "Other"), "reporter", "Reporter")




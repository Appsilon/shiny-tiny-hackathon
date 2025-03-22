box::use(
  tibble[tibble],
)

#' Generate mock FAERS-like data
#' @export
years <- seq(2000, 2025)

#' @export
report_types <- c("BSR", "directed", "expedited", "non_expedited")

#' @export
reporters <- c("consumer", "healthcare_profesional", "not_specified", "other")

#' @export
regions <- c("domestic", "foreign", "not_specified")

#' @export
seriousness <- c("serious", "death", "not_serious")

#' @export
age_groups <- c(
  "0-1_month",
  "2month-2years",
  "3-11years",
  "12-17years",
  "18-64years",
  "65-85years",
  "morethan85yeasr"
)

#' @export
sexes <- c("female", "male", "not_specified")

n <- 1000

#' @export
mock_data <- tibble(
  year = sample(years, size = n, replace = TRUE),
  report_type = sample(report_types, size = n, replace = TRUE),
  reporter = sample(reporters, size = n, replace = TRUE),
  region = sample(regions, size = n, replace = TRUE),
  seriousness = sample(seriousness, size = n, replace = TRUE),
  age_group = sample(age_groups, size = n, replace = TRUE),
  sex = sample(sexes, size = n, replace = TRUE),
  stringsAsFactors = FALSE
)

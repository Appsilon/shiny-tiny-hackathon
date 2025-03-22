# ChatGPT script to download FAERS data files.
# Prompt: Provide me with r code to web scrap the FAERS Quarterly Data Files for the last 10 years

library(rvest)
library(httr)
library(stringr)
library(fs)
library(utils)
library(readr)
library(dplyr)

# Set the minimum year for filtering (last 10 years)
min_year <- as.numeric(format(Sys.Date(), "%Y")) - 9

# Base URL
base_url <- "https://www.fda.gov/drugs/questions-and-answers-fdas-adverse-event-reporting-system-faers/fda-adverse-event-reporting-system-faers-latest-quarterly-data-files"

# Read the webpage
webpage <- read_html(base_url)

# Extract ZIP file links
zip_links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.zip$") %>%
  unique()

# Ensure absolute URLs
zip_links <- ifelse(grepl("^http", zip_links), zip_links, paste0("https://www.fda.gov", zip_links))

# Filter for last 10 years (>= min_year)
zip_links <- zip_links %>%
  str_subset(paste0("(", min_year, "|", paste((min_year + 1):(min_year + 9), collapse = "|"), "|20[2-9][0-9])"))

# Output and log directories
output_dir <- "FAERS_Last10Years"
log_dir <- "Logs"
dir_create(output_dir)
dir_create(log_dir)

# Initialize log
log_df <- data.frame(
  file_name = character(),
  year_detected = character(),
  download_status = character(),
  unzip_status = character(),
  download_url = character(),
  stringsAsFactors = FALSE
)

# Process each ZIP file
for (link in zip_links) {
  file_name <- basename(link)
  dest_path <- path(output_dir, file_name)
  unzip_dir <- path(output_dir, tools::file_path_sans_ext(file_name))

  # Detect year from filename
  year_detected <- str_extract(file_name, "20[0-9]{2}")

  # Download
  if (!file_exists(dest_path)) {
    cat("Downloading:", file_name, "\n")
    GET(link, write_disk(dest_path, overwrite = TRUE))
    download_status <- "Downloaded"
  } else {
    cat("Already downloaded:", file_name, "\n")
    download_status <- "Already Downloaded"
  }

  # Unzip
  if (!dir_exists(unzip_dir)) {
    cat("Unzipping:", file_name, "\n")
    unzip(dest_path, exdir = unzip_dir)
    unzip_status <- "Unzipped"
  } else {
    cat("Already unzipped:", file_name, "\n")
    unzip_status <- "Already Unzipped"
  }

  # Log entry
  log_df <- bind_rows(log_df, data.frame(
    file_name = file_name,
    year_detected = year_detected,
    download_status = download_status,
    unzip_status = unzip_status,
    download_url = link,
    stringsAsFactors = FALSE
  ))
}

# Save CSV log
log_path <- path(log_dir, paste0("faers_last10years_log_", Sys.Date(), ".csv"))
write_csv(log_df, log_path)

cat("Completed downloading, unzipping, and logging for the last 10 years.\n")

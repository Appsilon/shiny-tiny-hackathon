# ChatGPT script to download FAERS data files.
# Prompt: Provide me with r code to web scarp the FAERS Quarterly Data Files from 1968 until now

library(rvest)
library(httr)
library(stringr)
library(fs)
library(utils)
library(readr)
library(dplyr)

# Base URL for FAERS Quarterly Data Files page
base_url <- "https://www.fda.gov/drugs/questions-and-answers-fdas-adverse-event-reporting-system-faers/fda-adverse-event-reporting-system-faers-latest-quarterly-data-files"

# Read the webpage
webpage <- read_html(base_url)

# Extract all links to ZIP files
zip_links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.zip$") %>%
  unique()

# Ensure absolute URLs
zip_links <- ifelse(grepl("^http", zip_links), zip_links, paste0("https://www.fda.gov", zip_links))

# Optional filter for pre-2000 data (if found)
pre_2000_links <- zip_links[str_detect(zip_links, "(19[6-9][0-9]|2000)")]

if (length(pre_2000_links) > 0) {
  zip_links <- pre_2000_links
}

# Create output and log directories
output_dir <- "FAERS_Data"
log_dir <- "Logs"
dir_create(output_dir)
dir_create(log_dir)

# Initialize log dataframe
log_df <- data.frame(
  file_name = character(),
  year_detected = character(),
  download_status = character(),
  unzip_status = character(),
  download_url = character(),
  stringsAsFactors = FALSE
)

# Loop through all ZIP files
for (link in zip_links) {
  file_name <- basename(link)
  dest_path <- path(output_dir, file_name)
  unzip_dir <- path(output_dir, tools::file_path_sans_ext(file_name))

  # Extract year from filename (rough heuristic)
  year_detected <- str_extract(file_name, "(19[6-9][0-9]|20[0-2][0-9])")

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

  # Log info
  log_df <- bind_rows(log_df, data.frame(
    file_name = file_name,
    year_detected = year_detected,
    download_status = download_status,
    unzip_status = unzip_status,
    download_url = link,
    stringsAsFactors = FALSE
  ))
}

# Save log to CSV
log_path <- path(log_dir, paste0("faers_download_log_", Sys.Date(), ".csv"))
write_csv(log_df, log_path)

cat("Download, extraction, and logging complete.\n")

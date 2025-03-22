
library(data.table)
library(shiny)
library(ggplot2)

# ------ Global variables ------------------------------------------------------

ref_files <- list.files("data/refs", full.names = TRUE)
ref_list <- lapply(ref_files, data.table::fread)
names(ref_list) <- stringr::str_remove(basename(ref_files), ".csv")

# ------ Load data -------------------------------------------------------------
data_files <- list.files("data", pattern = "*.csv", full.names = TRUE)
data_list <- lapply(data_files, data.table::fread)
names(data_list) <- stringr::str_remove(basename(data_files), ".csv")

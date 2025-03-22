
library(data.table)
library(shiny)
library(ggplot2)

# ------ Global variables ------------------------------------------------------

ref_files <- list.files("data/refs", full.names = TRUE)
ref_list <- lapply(ref_files, data.table::fread)
names(ref_list) <- stringr::str_remove(basename(ref_files), ".csv")

custom_palette <- c(
    "#023364",
    "#2E86C1",
    "#F39C12",
    "#DC3545",
    "#28A745",
    "#6C5CE7",
    "#E84393",
    "#00CEC9"
)

custom_palette <- c(
    "#023364", # dark-blue (primary)
    "#0466C8", # light-blue
    "#081535", # extra-blue
    "#FF7043", # orange
    "#80ED99", # green
    "#1F6F31", # success
    "#B05617", # warning
    "#B71B1B"  # danger
)

custom_palette <- c(
    "#023364", # dark-blue (primary) - anchor the palette with your primary brand color
    "#FF7043", # orange - strong contrast to blue for clear category separation
    "#80ED99", # green - complementary to the blues and orange
    "#B05617", # warning amber - earthy tone that contrasts with previous colors
    "#0466C8", # light-blue - return to the brand family but visually distinct
    "#B71B1B", # danger red - high contrast
    "#FFD4C7", # orange-30 - soft tone for variety
    "#1F6F31"  # success green - darker than the first green for distinction
)

# ------ Load data -------------------------------------------------------------
data_files <- list.files("data", pattern = "*.csv", full.names = TRUE)
data_list <- lapply(data_files, data.table::fread)
names(data_list) <- stringr::str_remove(basename(data_files), ".csv")

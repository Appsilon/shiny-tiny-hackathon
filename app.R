# Main app file that sources the ui.R and server.R files

# Load necessary libraries
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

#data
source("data.R")
my_data <- read_my_data()

# Source UI and server components
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)

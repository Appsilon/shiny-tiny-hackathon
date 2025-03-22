# app.R - Main application file
source("global.R")  # Load libraries and data
source("ui.R")      # Load UI definition
source("server.R")  # Load server logic

# Run the application
shinyApp(ui = ui, server = server)

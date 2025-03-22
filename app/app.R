library(shiny)
library(bslib)



# UI modules for each tab
##################################### observatoire UI
observatoireUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("L'Observatoire"),
    p("Présentation de l'observatoire de la sécheresse")
  )
}



################################################# Server modules
########################################## observatoire server
observatoireServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for L'Observatoire
  })
}





############################################# app.R


# UI
ui <- page_navbar(
  title = span(
    img(src = "logo.png", height = "50px", alt = "Logo"),
    "Observatoire de la Sécheresse"
  ),
  # Add favicon
  window_title = "Observatoire de la Sécheresse",
  theme = bs_theme(bootswatch = "pulse", primary = '#008000',"navbar-bg" = "blue"),
  # Footer bar
  #footer = div(
  #style = "padding: 5px; text-align: center; position : fixed-bottom; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
  #"© 2025 - Pôle Digital du Ministère de l’Agriculture"
  #),
  id = "navbar",
  #position = "fixed-top",
  # Add favicon here with the other named parameters
  header = tags$head(tags$link(rel = "icon", href = "favicon.ico")),
  
  

  nav_panel("L'OBSERVATOIRE", observatoireUI("observatoire"))

  
)
# Server
server <- function(input, output, session) {
  
  observatoireServer("observatoire")
  
}
# Run the app
shinyApp(ui = ui, server = server)
#' simple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simple_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("My Simple Application"),
    p("This is a minimalistic Golem app."),
    numericInput(ns("number"), "Enter a number:", 10),
    verbatimTextOutput(ns("result"))
  )
}
    
#' simple Server Functions
#'
#' @noRd 
mod_simple_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$result <- renderPrint({
      paste("You entered:", input$number, 
            "- Doubled value is:", input$number * 2)
    })
  })
}
    
## To be copied in the UI
# mod_simple_ui("simple_1")
    
## To be copied in the server
# mod_simple_server("simple_1")

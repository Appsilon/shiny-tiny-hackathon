# mod_disclaimer.R
library(shiny)
library(shinyjs)

# UI Module for Disclaimer
mod_disclaimer_ui <- function(id) {
  ns <- NS(id)
  
  div(
    id = ns("disclaimer_content"),
    h3("Welcome to the FAERS Dashboard Replica", style = "color: #337ab7;"),
    p("As Master Oogway from Kung Fu Panda would say, 'Yesterday is history, tomorrow is a mystery, but today is a gift. That's why it's called the present.'"),
    p("This dashboard is a demonstration replica of the FDA Adverse Events Reporting System (FAERS) Public Dashboard, created in just about 4 hours of coding time."),
    p("Like Po's journey to becoming the Dragon Warrior, this dashboard is on its own journey of improvement!"),
    p("Please note that this is not the official FDA dashboard and is intended for demonstration purposes only."),
    div(
      style = "text-align: center; margin-top: 20px;",
      actionButton(ns("accept_button"), "I understand and accept", 
                   class = "btn btn-primary", 
                   style = "margin-right: 10px;"),
      actionButton(ns("decline_button"), "I decline", 
                   class = "btn btn-default")
    )
  )
}

# Server Module for Disclaimer
mod_disclaimer_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    # Show the disclaimer modal when the app starts
    showModal(modalDialog(
      mod_disclaimer_ui(id),
      title = "Disclaimer",
      footer = NULL,
      size = "m",
      easyClose = FALSE
    ))
    
    # Handle accept button click
    observeEvent(input$accept_button, {
      removeModal()
    })
    
    # Handle decline button click
    observeEvent(input$decline_button, {
      showModal(modalDialog(
        title = "One More Chance",
        div(
          h4("As Master Shifu would say, 'There is no secret ingredient. It's just you.'"),
          p("Be the change you want to see in the world! You still have a chance to accept and explore this dashboard."),
          div(
            style = "text-align: center; margin-top: 20px;",
            actionButton(session$ns("second_chance"), "I'll give it a try", 
                         class = "btn btn-success")
          )
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    })
    
    # Handle second chance button click
    observeEvent(input$second_chance, {
      removeModal()
    })
  })
}
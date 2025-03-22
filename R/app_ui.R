#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom htmltools tagList
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your page using bslib navbar layout
    bslib::page_navbar(
      title = "FAERS Data Explorer",
      # Main dashboard tab
      bslib::nav_panel(
        title = "Dashboard",
        
        # divide the page into two columns
        fluidRow(
          column(width = 6,
                 # Chart type selector in the main area
                 selectInput(
                   inputId = "chart_type",
                   label = "Select Data Category:",
                   choices = c(
                     "By Reporter Type" = "by_reporter_type",
                     "By Reporter" = "by_reporter",
                     "By Reporter Region" = "by_reporter_region", 
                     "By Report Seriousness" = "by_report_seriousness",
                     "By Age Group" = "by_age_group",
                     "By Sex" = "by_sex"
                   ),
                   selected = "by_reporter_type",
                   width = "100%"
                 )
                 # table output in the left column
          ),
          column(width = 6)
        ),
        fluidRow(
          column(width = 12,
                 # chart output in the right column
                 bslib::card(
                   # Dynamically update the card title based on chart type selection
                   bslib::card_header(
                     textOutput("table_title")  # Place the textOutput in the card_header
                   ),
                   bslib::card_body(
                     fluidRow(
                       column(width = 6,
                              DTOutput("table")
                       ),
                       column(width = 6,
                              plotlyOutput("stacked_bars")
                       )
                     )
                   )
                 )
                 
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyHackathon"
    )
  )
}

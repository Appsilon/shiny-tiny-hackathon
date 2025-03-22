# this script replicates the FDA Adverse Event Reporting System (FAERS) Public Dashboard.
# url: https://fis.fda.gov/sense/app/95239e26-e0be-42d9-a960-9a5f7f1c25ee/sheet/7a47a261-d58b-4203-a8aa-6d3021737452/state/analysis


# load the packages
library(tidyverse)
library(shiny)
library(bslib)
library(reactable)
library(glue)
library(plotly)


# load helper scripts
source("data_cleaning.R")

# define the ui
ui <- page_navbar(
  title = "FDA Adverse Event Reporting System (FAERS) Public Dashboard",
  inverse = TRUE,
  bg = "#438aca",
  theme = bs_theme(
    version = 5, 
    base_font = font_google("Archivo", local = FALSE)
  ),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  ),
  # home page
  nav_panel(
    title = "Home",
    shinyWidgets::radioGroupButtons( 
      inputId = "years_filter",
      label = NULL,
      choices = c("All Years", "Last 10 Years"),
      individual = TRUE
    ),
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Total Reports",
        value = scales::comma(30179725),
        showcase = bsicons::bs_icon("graph-up"),
        theme = "blue"
      ),
      value_box(
        title = "Serious Reports (excluding death)",
        value = scales::comma(16664479),
        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
        theme = "orange"
      ),
      value_box(
        title = "Death Reports",
        value = scales::comma(2722806),
        showcase = bsicons::bs_icon("x-circle-fill"),
        theme = "red"
      )
    ),
    selectInput(
      inputId = "report_filter",
      label = NULL,
      choices = c("Report Type","Reporter", "Reporter Region", "Report Serious", "Age Group", "Sex"),
      selected = "Report Type"
    ),
    layout_column_wrap(
      card(
        full_screen = TRUE,
        card_title(glue("Report received by:")),
        card_body(
          reactableOutput("table")
        )
      ),
      card(
        full_screen = TRUE,
        card_title(glue("Report received by:")),
        card_body(
          plotlyOutput("plot")
        )
      )
    )
  ),
  nav_spacer()
)

server <- function(input, output, session) {
  
  # selected variable
  variable <- reactive({
    if (input$report_filter == "Report Type") {
      report_by_report_type
    } else if (input$report_filter == "Reporter") {
      report_by_reporter
    } else if (input$report_filter == "Reporter Region") {
      report_by_reporter_region
    } else if (input$report_filter == "Report Serious") {
      report_by_seriousness
    } else if (input$report_filter == "Age Group") {
      report_by_age_group
    } else if (input$report_filter == "Sex") {
      report_by_sex
    } else {
      NA
    }
  })
  
  # table
  output$table <- renderReactable({
    reactable(variable())
  })
  
  # plot
  output$plot <- renderPlotly({
    if (input$report_filter == "Report Type") {
      plt <- ggplot(report_by_report_type, aes(x = Year, y = case_count, colour = report_type)) +
        geom_col()
      
      ggplotly(plt)
    }
  })
  
}

shinyApp(
  ui = ui, 
  server = server, 
  options = list(
    launch.browser = FALSE, 
    host = "127.0.0.1",
    port = 4406
  )
)
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr select filter between
#' @importFrom tidyr pivot_longer
#' @importFrom plotly renderPlotly plot_ly layout config
#' @importFrom DT renderDT datatable
#' @noRd
app_server <- function(input, output, session) {
  
  # Title-----------------------------------------------------------------------
  # Render the card title dynamically based on the selected chart type
  output$table_title <- renderText({
    req(input$chart_type)  # Make sure input is available
    
    # Update the title based on selected chart type
    chart_titles <- c(
      "by_reporter_type" = "Reports received by Reporter Type",
      "by_reporter" = "Reports received by Reporter",
      "by_reporter_region" = "Reports received by Reporter Region",
      "by_report_seriousness" = "Reports received by Report Seriousness",
      "by_age_group" = "Reports received by Age Group",
      "by_sex" = "Reports received by Sex"
    )
    
    # Return the title based on the selected chart type
    chart_titles[input$chart_type]
  })
  
  # Table-----------------------------------------------------------------------
  output$table <- renderDT({
    # make sure inputs are available
    req(input$chart_type, input$years)
    
    # Get the columns to use based on the selected chart type
    columns_to_use <- get_plot_columns(input$chart_type)
    
    # Filter the data to include only the specified columns (and 'year')
    table_data <- mock_data %>%
      # Use select_at to avoid errors if columns_to_use contains names not in the data
      select(c("year", "total", all_of(columns_to_use))) %>%
      # Use between() for year ranges and handle the 'All years' case properly
      filter(
        if (input$years == "All years") {
          year %in% 1968:2024
        } else {
          between(year, 2015, 2024)
        }
      )
      
    
    # Create the datatable
    datatable(
      table_data, 
      options = list(
        autoWidth = TRUE,
        scrollY = "300px",      # Set height for vertical scrolling
        scrollCollapse = TRUE,  # Collapse the table when shorter than scrollY
        paging = FALSE,         # Disable pagination
        info = FALSE,
        columnDefs = list(
          list(width = "100px", targets = 0)
        )
      )
    )
  })
  
  # Stacked Bards Plot----------------------------------------------------------
  output$stacked_bars <- renderPlotly({
    # make sure inputs are available
    req(input$chart_type, input$years)
    
    # Get the columns to use based on the selected chart type
    columns_to_use <- get_plot_columns(input$chart_type)
    
    # Create the stacked bars plot
    stacked_bars(columns_to_use, input$years)
  })
}

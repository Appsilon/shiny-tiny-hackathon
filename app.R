library(shiny)
library(highcharter) # Use highcharter instead of plotly
library(dplyr)
library(DT)
library(readxl)

source('aux_functions.R')

data <- read_excel("data.xlsx")

# Define the UI
ui <- fluidPage(
  # Include Google Fonts for a modern, sophisticated font (Poppins)
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
        font-family: 'Poppins', Arial, sans-serif;
      }

      html, body, #shiny-app {
        height: 100vh;
        width: 100vw;
        overflow: hidden;
      }

      /* Top Navigation Bar */
      .top-nav {
        background-color: #003087;
        color: white;
        padding: 10px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        height: 60px; /* Fixed height for calculation */
      }

      .top-nav h1 {
        font-size: 18px;
        font-weight: 600;
      }

      .top-nav img {
        height: 40px;
      }

      /* Secondary Navigation Bar */
      .secondary-nav {
        background-color: #e6f0fa;
        padding: 10px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        height: 50px; /* Fixed height for calculation */
      }

      .secondary-nav ul {
        list-style: none;
        display: flex;
        gap: 10px;
      }

      .secondary-nav ul li a {
        text-decoration: none;
        color: #333;
        padding: 5px 10px;
        background-color: #f0f0f0;
        border-radius: 3px;
        font-weight: 400;
      }

      .secondary-nav ul li a:hover {
        background-color: #ddd;
      }

      /* Icon Cards Container */
      .icon-cards {
        display: flex;
        justify-content: space-around;
        padding: 20px;
        background-color: #f9f9f9;
        height: 100px; /* Fixed height for calculation */
      }

      /* Individual Card Styling */
      .card {
        padding: 15px;
        text-align: center;
      }

      /* Card Label (Top Text) */
      .card .label {
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 600;
        color: #666;
      }

      /* Container for Icon and Value (Side by Side) */
      .card .icon-value-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      /* Card Icon (Left-Aligned) */
      .card .icon {
        font-size: 24px;
        color: #1e90ff;
      }

      /* Card Value (Right-Aligned) */
      .card .value {
        font-size: 24px;
        font-weight: 700;
      }

      /* Select Input and Buttons */
      .controls {
        display: flex;
        justify-content: space-between;
        padding: 10px 20px;
        background-color: #f0f0f0;
        height: 50px; /* Fixed height for calculation */
      }

      .controls select {
        padding: 5px;
        font-size: 14px;
      }

      .controls .buttons {
        display: flex;
        gap: 10px;
      }

      .controls button {
        padding: 5px 15px;
        background-color: #003087;
        color: white;
        border: none;
        border-radius: 3px;
        cursor: pointer;
        font-weight: 600;
      }

      .controls button:hover {
        background-color: #0056d2;
      }

      /* Main Panel */
      .main-panel {
        display: flex;
        padding: 20px;
        gap: 20px;
        height: calc(100vh - 60px - 50px - 100px - 50px - 7vh); /* Adjusted for other elements and bottom panel */
        width: 100%;
      }

      /* Data Table */
      .data-table {
        width: 40%;
        height: 100%;
        overflow-y: auto;
        border: 1px solid #ddd;
      }

      .data-table table {
        width: 100%;
        border-collapse: collapse;
      }

      .data-table th, .data-table td {
        padding: 10px;
        text-align: left;
        border-bottom: 1px solid #ddd;
      }

      .data-table th {
        background-color: #f0f0f0;
        position: sticky;
        top: 0;
      }

      /* Bar Plot */
      .bar-plot {
        width: 60%;
        height: 100%;
      }

      /* Bottom Panel */
      .bottom-panel {
        height: 7vh; /* 7% of the viewport height */
        width: 100%;
        background-color: #f0f0f0;
        padding: 10px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-size: 12px;
        color: #666;
      }

      .bottom-panel a {
        color: #003087;
        text-decoration: none;
      }

      /* Style the search bar */
      .dataTables_filter {
        margin-bottom: 10px;
      }

      .dataTables_filter input {
        padding: 5px;
        font-family: 'Poppins', sans-serif;
        font-size: 12px;
        border: 1px solid #ddd;
        border-radius: 3px;
      }

      /* Style the column visibility button */
      .dt-buttons {
        margin-bottom: 10px;
      }

      .dt-button {
        padding: 5px 15px;
        background-color: #003087;
        color: white;
        border: none;
        border-radius: 3px;
        font-family: 'Poppins', sans-serif;
        font-size: 12px;
        cursor: pointer;
      }

      .dt-button:hover {
        background-color: #0056d2;
      }
    "))
  ),
  
  # Top Navigation Bar
  tags$nav(class = "top-nav",
           tags$h1("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
           tags$a(href = "https://www.fda.gov/", tags$img(src = "logo.png", alt = "FDA Logo"))
  ),
  
  # Secondary Navigation Bar
  tags$nav(class = "secondary-nav",
           tags$ul(
             tags$li(tags$a(href = "#", "Home")),
             tags$li(tags$a(href = "#", "Search"))
           ),
           tags$ul(
             tags$li(tags$a(href = "#", "Disclaimer")),
             tags$li(tags$a(href = "#", "Report a Problem")),
             tags$li(tags$a(href = "#", "FAQ")),
             tags$li(tags$a(href = "#", "Site Feedback"))
           )
  ),
  
  # Icon Cards
  tags$div(class = "icon-cards",
           tags$div(class = "card",
                    tags$div(class = "label", "Total Reports"),
                    tags$div(class = "icon-value-container",
                             tags$div(class = "icon", "📊"),
                             tags$div(class = "value", "23,179,725")
                    )
           ),
           tags$div(class = "card",
                    tags$div(class = "label", "Serious Reports"),
                    tags$div(class = "icon-value-container",
                             tags$div(class = "icon", "⚠️"),
                             tags$div(class = "value", "16,664,479")
                    )
           ),
           tags$div(class = "card",
                    tags$div(class = "label", "Direct Reports"),
                    tags$div(class = "icon-value-container",
                             tags$div(class = "icon", "📈"),
                             tags$div(class = "value", "806")
                    )
           )
  ),
  
  # Select Input and Buttons
  tags$div(class = "controls",
           selectInput("reportType", label = NULL,
                       choices = c("Reports received by Report Type", "Other Option"),
                       selected = "Reports received by Report Type"
           ),
           tags$div(class = "buttons",
                    actionButton("allYears", "All Years"),
                    actionButton("last10Years", "Last 10 Years")
           )
  ),
  
  # Main Panel
  tags$div(class = "main-panel",
           # Data Table
           tags$div(class = "data-table",
                    DTOutput("dataTable")
           ),
           # Bar Plot
           tags$div(class = "bar-plot",
                    highchartOutput("barPlot", height = "100%")
           )
  ),
  
  # Bottom Panel (replaces .bottom-text)
  tags$div(class = "bottom-panel",
           tags$span("Date as of December 31, 2024"),
           tags$a(href = "#", "Vulnerability Disclosure Policy")
  )
)

server <- function(input, output, session) {
 
  
  # Render the Data Table
  output$dataTable <- renderDT({
    
    # Combine totals with the data
    display_data <- data
    
    # Format the table
    datatable(display_data,
              options = list(
                pageLength = 10,
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 'Bfrtip', # Add buttons, search, and filter
                buttons = c('colvis'), # Add column visibility button
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"), # Center-align all columns
                  list(targets = 0, searchable = FALSE) # Disable search on the totals row
                ),
                initComplete = JS(
                  "function(settings, json) {}"
                )
              ),
              rownames = FALSE,
              callback = JS(
                "table.on('draw', function() {
                  // Style the first row (totals) differently
                  $(table.row(0).node()).css({
                    'background-color': '#f0f0f0',
                    'font-weight': 'bold'
                  });
                });"
              ),
              extensions = c('Buttons') # Enable Buttons extension for column visibility
    ) %>%
      formatStyle(
        columns = 1:ncol(display_data),
        fontFamily = "Poppins",
        fontSize = "12px"
      ) %>%
      formatRound(columns = names(data), digits = 0) %>%
      formatStyle(
        columns = "Year",
        fontWeight = styleEqual("TOTAL REPORTS", "bold")
      )
    
    
    
  })
  
  
  data_filtered <- reactive({
    
    req(input$dataTable_state)
    
    cols_selected <- names(data)[c(unlist(sapply(input$dataTable_state$columns, function(col) col$visible)))]
    cols_selected <- cols_selected[cols_selected != "Total Reports"]
    
    data <- data %>%
      select(cols_selected)
    
  })
  
  
  output$barPlot <- renderHighchart({
    
    return(stacked_plot(data_filtered()))
  })
  
  
  
}

shinyApp(ui = ui, server = server)
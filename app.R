library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Generate mock FAERS data
set.seed(123)
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 19):current_year  # 20 years of data

# Categories with their possible values
categories <- list(
  "Report Type" = c("BSR", "Direct", "Expedited", "Non-Expedited"),
  "Reporter" = c("Consumer", "Healthcare", "Not Specified", "Other"),
  "Reporter Region" = c("Domestic", "Foreign", "Not Specified"),
  "Report Seriousness" = c("Death", "Non-Serious", "Serious"),
  "Age Group" = c("0-1 month", "2 months - 2 years", "3-11 years", "12-17 years", 
                  "18-64 years", "65-85 years", "More than 85 years", "Not Specified"),
  "Sex" = c("Female", "Male", "Not Specified")
)

# Generate data frame with random distributions
generate_mock_data <- function() {
  n_rows <- 10000  # Number of reports
  
  # Create data with random values
  data <- data.frame(
    Year = sample(years, n_rows, replace = TRUE, 
                  prob = c(rep(0.03, 10), rep(0.04, 5), rep(0.05, 5))), # More recent years have more reports
    "ReportType" = sample(categories[["Report Type"]], n_rows, replace = TRUE),
    "Reporter" = sample(categories[["Reporter"]], n_rows, replace = TRUE),
    "ReporterRegion" = sample(categories[["Reporter Region"]], n_rows, replace = TRUE),
    "ReportSeriousness" = sample(categories[["Report Seriousness"]], n_rows, replace = TRUE),
    "AgeGroup" = sample(categories[["Age Group"]], n_rows, replace = TRUE),
    "Sex" = sample(categories[["Sex"]], n_rows, replace = TRUE)
  )
  
  return(data)
}

# Generate once at startup
mock_data <- generate_mock_data()


ui <- page_sidebar(
  title = "FDA Adverse Event Reporting System (FAERS) Dashboard",
  sidebar = sidebar(
    selectInput("category", "Select Report Category:", 
                choices = c(
                  "Report Type"="ReportType", 
                  "Reporter"="Reporter", 
                  "Reporter Region"="ReporterRegion", 
                  "Report Seriousness"="ReportSeriousness",
                  "Age Group" = "AgeGroup",
                  "Sex" = "Sex"
                ),
                selected = "Report Type"),
    
    radioButtons("timeFilter", "Time Filter:",
                 choices = c("All Years", "Last 10 Years"),
                 selected = "All Years"),
    
    hr(),
    
    helpText("This dashboard displays FDA adverse event reporting data by different categories."),
    helpText("Select a category from the dropdown to see breakdowns by that dimension."),
    helpText("Note: This dashboard uses mock data for demonstration purposes.")
  ),
  
  
  layout_columns(
    value_box(
      title = "Total Reports",
      value = textOutput("totalReports"),
      showcase = bsicons::bs_icon("clipboard-data"),
      theme = "primary"
    ),
    value_box(
      title = "Most Common Category",
      value = textOutput("mostCommonCategory"),
      showcase = bsicons::bs_icon("bar-chart"),
      theme = "secondary"
    ),
    value_box(
      title = "Reports This Year",
      value = textOutput("reportsThisYear"),
      showcase = bsicons::bs_icon("calendar-event"),
      theme = "success"
    )
  ),
  
  
  card(
    card_header("Yearly Breakdown by Category"),
    plotlyOutput("barChart", height = "400px")
  ),
  
  card(
    card_header("Detailed Data Table"),
    DTOutput("dataTable")
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtered by time
  filtered_data <- reactive({
    data <- mock_data
    
    if (input$timeFilter == "Last 10 Years") {
      cutoff_year <- max(years) - 9
      data <- data %>% filter(Year >= cutoff_year)
    }
    
    return(data)
  })
  
  # Summary statistics
  output$totalReports <- renderText({
    nrow(filtered_data())
  })
  
  output$mostCommonCategory <- renderText({
    category_counts <- table(filtered_data()[[input$category]])
    names(which.max(category_counts))
  })
  
  output$reportsThisYear <- renderText({
    current_year_data <- filtered_data() %>% filter(Year == current_year)
    nrow(current_year_data)
  })
  
  # Bar chart
  output$barChart <- renderPlotly({
    # Get the selected category
    selected_category <- input$category
    
    # Aggregate data
    plot_data <- filtered_data() %>%
      group_by(Year, !!sym(selected_category)) %>%
      summarise(Count = n(), .groups = 'drop')
    
    # Create plot
    p <- ggplot(plot_data, aes(x = as.factor(Year), y = Count, fill = !!sym(selected_category))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste("Yearly Breakdown by", selected_category),
        x = "Year",
        y = "Number of Reports",
        fill = selected_category
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    ggplotly(p)
  })
  
  # Data table
  output$dataTable <- renderDT({
    selected_category <- input$category
    
    # Aggregate data for table
    table_data <- filtered_data() %>%
      group_by(Year, !!sym(selected_category)) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      tidyr::pivot_wider(names_from = !!sym(selected_category), values_from = Count, values_fill = 0)
    
    # Sort by year descending
    table_data <- table_data %>% arrange(desc(Year))
    
    # Format table
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        dom = 'tip',
        scrollX = TRUE
      ),
      caption = paste("Report Counts by Year and", selected_category),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)

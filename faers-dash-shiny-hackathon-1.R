library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(markdown)

# Sample data generation - In a real app, you would connect to the FDA API or use a downloaded dataset
generate_sample_data <- function(n = 1000) {
  set.seed(123)

  # Create dates spanning from 2010 to 2024
  dates <- seq(as.Date("2010-01-01"), as.Date("2024-10-01"), by = "day")

  # Create drug names
  drug_names <- c("Aspirin", "Ibuprofen", "Acetaminophen", "Lisinopril", "Atorvastatin",
                  "Metformin", "Levothyroxine", "Amlodipine", "Omeprazole", "Albuterol")

  # Create reaction types
  reaction_types <- c("Headache", "Nausea", "Dizziness", "Rash", "Fatigue",
                      "Vomiting", "Abdominal Pain", "Shortness of Breath", "Chest Pain", "Allergic Reaction")

  # Patient age groups
  age_groups <- c("<18", "18-44", "45-64", "65-74", "75+")

  # Patient sex
  sex <- c("Male", "Female", "Unknown")

  # Create severity levels
  severity <- c("Mild", "Moderate", "Severe", "Fatal")

  # Create sender types
  sender_types <- c("Healthcare Professional", "Patient", "Manufacturer", "Distributor", "Other")

  # Generate sample data
  data <- data.frame(
    date = sample(dates, n, replace = TRUE),
    drug_name = sample(drug_names, n, replace = TRUE),
    reaction = sample(reaction_types, n, replace = TRUE),
    age_group = sample(age_groups, n, replace = TRUE),
    sex = sample(sex, n, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    severity = sample(severity, n, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05)),
    sender_type = sample(sender_types, n, replace = TRUE),
    reporter_country = sample(c("United States", "Canada", "Mexico", "United Kingdom", "France", "Germany", "Japan", "Australia"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Add year and month columns
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$year_month <- format(data$date, "%Y-%m")

  return(data)
}

# Generate sample data
adverse_events <- generate_sample_data(10000)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "FDA Adverse Events Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Explore Data", tabName = "explore", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    br(),
    dateRangeInput("date_range", "Select Date Range",
                   start = min(adverse_events$date),
                   end = max(adverse_events$date)),
    selectizeInput("drug_select", "Select Drug(s)",
                   choices = c("All", unique(adverse_events$drug_name)),
                   selected = "All",
                   multiple = TRUE),
    pickerInput("reaction_select", "Select Reaction(s)",
                choices = c("All", unique(adverse_events$reaction)),
                selected = "All",
                multiple = TRUE,
                options = list(`actions-box` = TRUE)),
    checkboxGroupInput("severity_select", "Select Severity",
                       choices = unique(adverse_events$severity),
                       selected = unique(adverse_events$severity))
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_reports_box", width = 4),
                valueBoxOutput("serious_events_box", width = 4),
                valueBoxOutput("fatal_events_box", width = 4)
              ),
              fluidRow(
                box(title = "Reports Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("reports_over_time", height = 300), width = 12)
              ),
              fluidRow(
                box(title = "Top Reported Drugs", status = "primary", solidHeader = TRUE,
                    plotlyOutput("top_drugs_plot", height = 300), width = 6),
                box(title = "Top Reported Reactions", status = "primary", solidHeader = TRUE,
                    plotlyOutput("top_reactions_plot", height = 300), width = 6)
              ),
              fluidRow(
                box(title = "Reports by Patient Demographics", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6, plotlyOutput("age_group_plot", height = 250)),
                      column(6, plotlyOutput("sex_plot", height = 250))
                    ), width = 12)
              )
      ),

      # Explore Data tab
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Adverse Events Data", status = "primary", solidHeader = TRUE,
                    DTOutput("events_table"), width = 12)
              )
      ),

      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About This Dashboard", status = "primary", solidHeader = TRUE,
                    width = 12,
                    includeMarkdown("about.md")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    # Start with all data
    data <- adverse_events

    # Apply date filter
    data <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])

    # Apply drug filter
    if (!("All" %in% input$drug_select) && length(input$drug_select) > 0) {
      data <- data %>% filter(drug_name %in% input$drug_select)
    }

    # Apply reaction filter
    if (!("All" %in% input$reaction_select) && length(input$reaction_select) > 0) {
      data <- data %>% filter(reaction %in% input$reaction_select)
    }

    # Apply severity filter
    if (length(input$severity_select) > 0) {
      data <- data %>% filter(severity %in% input$severity_select)
    }

    return(data)
  })

  # Value boxes
  output$total_reports_box <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Reports",
      icon = icon("file-medical"),
      color = "blue"
    )
  })

  output$serious_events_box <- renderValueBox({
    serious_count <- filtered_data() %>%
      filter(severity %in% c("Severe", "Moderate")) %>%
      nrow()

    valueBox(
      serious_count,
      "Serious Events",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$fatal_events_box <- renderValueBox({
    fatal_count <- filtered_data() %>%
      filter(severity == "Fatal") %>%
      nrow()

    valueBox(
      fatal_count,
      "Fatal Events",
      icon = icon("skull-crossbones"),
      color = "red"
    )
  })

  # Reports over time plot
  output$reports_over_time <- renderPlotly({
    time_data <- filtered_data() %>%
      group_by(year_month) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(year_month)

    plot_ly(time_data, x = ~year_month, y = ~count, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#1f77b4"), marker = list(color = "#1f77b4")) %>%
      layout(title = "Number of Adverse Event Reports Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Number of Reports"))
  })

  # Top drugs plot
  output$top_drugs_plot <- renderPlotly({
    drug_data <- filtered_data() %>%
      group_by(drug_name) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)

    plot_ly(drug_data, x = ~reorder(drug_name, count), y = ~count, type = 'bar',
            marker = list(color = "#2ca02c")) %>%
      layout(title = "Top 10 Drugs by Adverse Event Reports",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Number of Reports"))
  })

  # Top reactions plot
  output$top_reactions_plot <- renderPlotly({
    reaction_data <- filtered_data() %>%
      group_by(reaction) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)

    plot_ly(reaction_data, x = ~reorder(reaction, count), y = ~count, type = 'bar',
            marker = list(color = "#d62728")) %>%
      layout(title = "Top 10 Adverse Reactions",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Number of Reports"))
  })

  # Age group plot
  output$age_group_plot <- renderPlotly({
    age_data <- filtered_data() %>%
      group_by(age_group) %>%
      summarise(count = n(), .groups = 'drop')

    plot_ly(age_data, labels = ~age_group, values = ~count, type = 'pie',
            hole = 0.4,
            marker = list(colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))) %>%
      layout(title = "Reports by Age Group")
  })

  # Sex plot
  output$sex_plot <- renderPlotly({
    sex_data <- filtered_data() %>%
      group_by(sex) %>%
      summarise(count = n(), .groups = 'drop')

    plot_ly(sex_data, labels = ~sex, values = ~count, type = 'pie',
            hole = 0.4,
            marker = list(colors = c("#1f77b4", "#ff7f0e", "#9467bd"))) %>%
      layout(title = "Reports by Sex")
  })

  # Data table
  output$events_table <- renderDT({
    filtered_data() %>%
      select(date, drug_name, reaction, age_group, sex, severity, sender_type, reporter_country) %>%
      datatable(options = list(
        pageLength = 10,
        searchHighlight = TRUE,
        scrollX = TRUE
      ))
  })
}

# Create an about.md file for the About tab
about_content <- "
# FDA Adverse Events Dashboard

This dashboard provides an interactive visualization of adverse event reports submitted to the FDA.

## Data Source

This is a simulation based on the structure of the FDA Adverse Event Reporting System (FAERS) data. In a production environment, this would connect to the FDA's API or use downloaded FAERS data.

## Features

- Filter adverse event reports by date range, drug, reaction type, and severity
- View trends over time
- Explore demographics of reported cases
- Analyze the most commonly reported drugs and reactions

## Purpose

This tool is designed to help healthcare professionals, researchers, and the public understand patterns in adverse event reporting.
"

# Write the about content to a file
writeLines(about_content, "about.md")

# Run the application
shinyApp(ui = ui, server = server)

# FDA-like Dashboard with Synthetic Data
# This code creates a dashboard similar to the FDA's SENSE dashboard
# but uses entirely synthetic/dummy data

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(tidyr)
library(lubridate)

# Generate synthetic data
set.seed(123)

# Create dummy data for adverse events
generate_ae_data <- function(n = 1000) {
  product_types <- c("Drugs", "Devices", "Biologics", "Food", "Tobacco", "Cosmetics")
  product_classes <- c("Class I", "Class II", "Class III", "OTC", "Prescription", "Supplement")
  report_types <- c("Expedited", "Periodic", "Direct", "Literature", "Study")
  outcome_types <- c("Death", "Life-threatening", "Hospitalization", "Disability", "Congenital Anomaly", "Other Serious")

  dates <- seq(as.Date("2015-01-01"), as.Date("2024-12-31"), by = "day")

  data.frame(
    date = sample(dates, n, replace = TRUE),
    product_type = sample(product_types, n, replace = TRUE),
    product_class = sample(product_classes, n, replace = TRUE),
    report_type = sample(report_types, n, replace = TRUE),
    outcome = sample(outcome_types, n, replace = TRUE, prob = c(0.05, 0.1, 0.3, 0.15, 0.05, 0.35)),
    age_group = sample(c("0-17", "18-44", "45-64", "65-74", "75+", "Unknown"), n, replace = TRUE),
    gender = sample(c("Male", "Female", "Unknown"), n, replace = TRUE),
    state = sample(state.abb, n, replace = TRUE),
    count = rpois(n, 10)
  ) %>%
    mutate(
      year = year(date),
      month = month(date),
      quarter = quarter(date)
    )
}

ae_data <- generate_ae_data(10000)

# Create the app
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "FDA Adverse Events Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Reports by Product", tabName = "products", icon = icon("pills")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Geographic", tabName = "geographic", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # Filters
    h4("Filters", style = "padding-left: 15px;"),

    dateRangeInput("date_range", "Date Range",
                   start = "2020-01-01", end = "2023-12-31"),

    selectInput("product_type", "Product Type",
                choices = c("All", unique(ae_data$product_type)),
                selected = "All"),

    selectInput("outcome", "Outcome",
                choices = c("All", unique(ae_data$outcome)),
                selected = "All"),

    actionButton("reset", "Reset Filters", icon = icon("sync"))
  ),

  dashboardBody(
    # Disclaimer alert
    tags$div(
      class = "alert alert-warning",
      tags$strong("DISCLAIMER: "),
      "This dashboard contains entirely synthetic/dummy data and is NOT official FDA data.
      It is created solely for demonstration purposes and should not be used for any official analysis or decision-making."
    ),

    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_reports", width = 4),
                valueBoxOutput("serious_events", width = 4),
                valueBoxOutput("death_reports", width = 4)
              ),

              fluidRow(
                box(title = "Trend of Adverse Events Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("trend_plot", height = 300), width = 8),
                box(title = "Events by Outcome", status = "primary", solidHeader = TRUE,
                    plotlyOutput("outcome_pie", height = 300), width = 4)
              ),

              fluidRow(
                box(title = "Reports by Product Type", status = "primary", solidHeader = TRUE,
                    plotlyOutput("product_bar", height = 300), width = 6),
                box(title = "Reports by Report Type", status = "primary", solidHeader = TRUE,
                    plotlyOutput("report_type_bar", height = 300), width = 6)
              )
      ),

      # Products tab
      tabItem(tabName = "products",
              fluidRow(
                box(title = "Product Type Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("product_dist", height = 300), width = 12)
              ),

              fluidRow(
                box(title = "Product Class by Outcome", status = "primary", solidHeader = TRUE,
                    plotlyOutput("product_outcome_heatmap", height = 400), width = 12)
              ),

              fluidRow(
                box(title = "Detailed Report Data", status = "primary", solidHeader = TRUE,
                    DTOutput("product_table"), width = 12)
              )
      ),

      # Demographics tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Age Group Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("age_dist", height = 300), width = 6),
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("gender_dist", height = 300), width = 6)
              ),

              fluidRow(
                box(title = "Age Group by Outcome", status = "primary", solidHeader = TRUE,
                    plotlyOutput("age_outcome", height = 400), width = 12)
              )
      ),

      # Geographic tab
      tabItem(tabName = "geographic",
              fluidRow(
                box(title = "Reports by State", status = "primary", solidHeader = TRUE,
                    plotlyOutput("state_map", height = 500), width = 12)
              ),

              fluidRow(
                box(title = "Top 10 States by Number of Reports", status = "primary", solidHeader = TRUE,
                    plotlyOutput("top_states", height = 300), width = 12)
              )
      ),

      # About tab
      tabItem(tabName = "about",
              box(title = "About This Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                  tags$div(
                    tags$h3("Synthetic Data Dashboard"),
                    tags$p("This dashboard is a demonstration that mimics the style and functionality of the FDA's SENSE dashboard."),
                    tags$p(tags$strong("IMPORTANT: "), "All data presented in this dashboard is synthetic/dummy data generated for demonstration purposes only."),
                    tags$p("The real FDA SENSE dashboard can be found at: ",
                           tags$a(href="https://fis.fda.gov/sense/app/95239e26-e0be-42d9-a960-9a5f7f1c25ee/sheet/8eef7d83-7945-4091-b349-e5c41ed49f99/state/analysis",
                                  "FDA SENSE Dashboard")),
                    tags$p("This demonstration was created to showcase Shiny dashboard capabilities only.")
                  )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    data <- ae_data

    # Apply date filter
    data <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])

    # Apply product type filter
    if (input$product_type != "All") {
      data <- data %>% filter(product_type == input$product_type)
    }

    # Apply outcome filter
    if (input$outcome != "All") {
      data <- data %>% filter(outcome == input$outcome)
    }

    return(data)
  })

  # Reset button
  observeEvent(input$reset, {
    updateDateRangeInput(session, "date_range", start = "2020-01-01", end = "2023-12-31")
    updateSelectInput(session, "product_type", selected = "All")
    updateSelectInput(session, "outcome", selected = "All")
  })

  # Value boxes
  output$total_reports <- renderValueBox({
    count <- sum(filtered_data()$count)
    valueBox(
      format(count, big.mark = ","), "Total Reports", icon = icon("file-medical"), color = "blue"
    )
  })

  output$serious_events <- renderValueBox({
    serious <- filtered_data() %>%
      filter(outcome %in% c("Hospitalization", "Disability", "Life-threatening")) %>%
      summarize(total = sum(count)) %>%
      pull(total)

    valueBox(
      format(serious, big.mark = ","), "Serious Events", icon = icon("exclamation-triangle"), color = "yellow"
    )
  })

  output$death_reports <- renderValueBox({
    deaths <- filtered_data() %>%
      filter(outcome == "Death") %>%
      summarize(total = sum(count)) %>%
      pull(total)

    valueBox(
      format(deaths, big.mark = ","), "Death Reports", icon = icon("heart-broken"), color = "red"
    )
  })

  # Trend plot
  output$trend_plot <- renderPlotly({
    trend_data <- filtered_data() %>%
      group_by(year, quarter) %>%
      summarize(total = sum(count)) %>%
      mutate(quarter_label = paste0(year, " Q", quarter))

    plot_ly(trend_data, x = ~quarter_label, y = ~total, type = "scatter", mode = "lines+markers") %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })

  # Outcome pie chart
  output$outcome_pie <- renderPlotly({
    outcome_data <- filtered_data() %>%
      group_by(outcome) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(outcome_data, labels = ~outcome, values = ~total, type = "pie",
            marker = list(colors = colorRampPalette(c("#0073C2", "#EFC000", "#868686", "#CD534C"))(length(unique(outcome_data$outcome))))) %>%
      layout(title = "")
  })

  # Product bar chart
  output$product_bar <- renderPlotly({
    product_data <- filtered_data() %>%
      group_by(product_type) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(product_data, x = ~reorder(product_type, -total), y = ~total, type = "bar",
            marker = list(color = "#0073C2")) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })

  # Report type bar chart
  output$report_type_bar <- renderPlotly({
    report_data <- filtered_data() %>%
      group_by(report_type) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(report_data, x = ~reorder(report_type, -total), y = ~total, type = "bar",
            marker = list(color = "#EFC000")) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })

  # Product distribution
  output$product_dist <- renderPlotly({
    prod_dist <- filtered_data() %>%
      group_by(product_type, product_class) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(prod_dist, x = ~product_class, y = ~total, color = ~product_type, type = "bar") %>%
      layout(title = "",
             xaxis = list(title = "Product Class"),
             yaxis = list(title = "Number of Reports"),
             barmode = "stack")
  })

  # Product outcome heatmap
  output$product_outcome_heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(product_class, outcome) %>%
      summarize(total = sum(count)) %>%
      ungroup() %>%
      complete(product_class, outcome, fill = list(total = 0))

    plot_ly(heatmap_data, x = ~product_class, y = ~outcome, z = ~total, type = "heatmap",
            colorscale = "Blues") %>%
      layout(title = "",
             xaxis = list(title = "Product Class"),
             yaxis = list(title = "Outcome"))
  })

  # Product table
  output$product_table <- renderDT({
    table_data <- filtered_data() %>%
      group_by(product_type, product_class, report_type, outcome) %>%
      summarize(total_reports = sum(count)) %>%
      arrange(desc(total_reports))

    datatable(table_data, options = list(pageLength = 10))
  })

  # Age distribution
  output$age_dist <- renderPlotly({
    age_data <- filtered_data() %>%
      group_by(age_group) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(age_data, x = ~age_group, y = ~total, type = "bar",
            marker = list(color = "#0073C2")) %>%
      layout(title = "",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Number of Reports"))
  })

  # Gender distribution
  output$gender_dist <- renderPlotly({
    gender_data <- filtered_data() %>%
      group_by(gender) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total))

    plot_ly(gender_data, labels = ~gender, values = ~total, type = "pie",
            marker = list(colors = c("#0073C2", "#EFC000", "#868686"))) %>%
      layout(title = "")
  })

  # Age by outcome
  output$age_outcome <- renderPlotly({
    age_outcome_data <- filtered_data() %>%
      group_by(age_group, outcome) %>%
      summarize(total = sum(count)) %>%
      ungroup()

    plot_ly(age_outcome_data, x = ~age_group, y = ~total, color = ~outcome, type = "bar") %>%
      layout(title = "",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Number of Reports"),
             barmode = "stack")
  })

  # State map
  output$state_map <- renderPlotly({
    state_data <- filtered_data() %>%
      group_by(state) %>%
      summarize(total = sum(count))

    # Create a dataframe with state codes and values
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showlakes = TRUE,
      lakecolor = toRGB("white")
    )

    plot_geo(state_data, locationmode = "USA-states") %>%
      add_trace(
        z = ~total, locations = ~state,
        color = ~total, colors = "Blues"
      ) %>%
      colorbar(title = "Number of Reports") %>%
      layout(
        title = "",
        geo = g
      )
  })

  # Top states
  output$top_states <- renderPlotly({
    top_states <- filtered_data() %>%
      group_by(state) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total)) %>%
      head(10)

    plot_ly(top_states, x = ~reorder(state, -total), y = ~total, type = "bar",
            marker = list(color = "#0073C2")) %>%
      layout(title = "",
             xaxis = list(title = "State"),
             yaxis = list(title = "Number of Reports"))
  })
}

shinyApp(ui, server)

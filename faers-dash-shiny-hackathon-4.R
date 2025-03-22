# FDA Adverse Events Dashboard with Synthetic Data
# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(tidyr)
library(shinyjs)
library(readr)

# Function to generate synthetic data
generate_synthetic_data <- function(n = 10000) {
  set.seed(123)

  # Define possible values for categorical variables
  product_types <- c("Drug", "Device", "Food", "Dietary Supplement", "Cosmetic")
  report_types <- c("Initial", "Follow-up", "Final")
  outcomes <- c("Death", "Life-threatening", "Hospitalization", "Disability", "Congenital Anomaly",
                "Required Intervention", "Other Serious")
  countries <- c("United States", "Canada", "United Kingdom", "Germany", "France", "Japan",
                 "Australia", "China", "Brazil", "India", "Mexico", "Other")

  # Generate dates spread over last 10 years
  dates <- sample(seq(as.Date('2013-01-01'), Sys.Date(), by="day"), n, replace=TRUE)

  # Create synthetic dataset
  data.frame(
    report_id = paste0("FAERS-", sample(100000:999999, n)),
    received_date = dates,
    report_year = year(dates),
    report_quarter = quarter(dates),
    product_type = sample(product_types, n, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)),
    report_type = sample(report_types, n, replace = TRUE),
    patient_age = sample(c(NA, 0:100), n, replace = TRUE, prob = c(0.1, rep(0.009, 101))),
    patient_gender = sample(c("Male", "Female", "Unknown"), n, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    country = sample(countries, n, replace = TRUE, prob = c(0.6, rep(0.036, 11))),
    outcome = sample(outcomes, n, replace = TRUE),
    seriousness = sample(c("Serious", "Non-serious"), n, replace = TRUE, prob = c(0.3, 0.7)),
    manufacturer_received_date = dates - sample(0:30, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Create synthetic data
synthetic_data <- generate_synthetic_data()

# UI Definition
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "FDA Adverse Event Reporting System Dashboard",
    titleWidth = 450
  ),

  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # Filters panel
    div(
      style = "padding: 15px;",
      h4("Filters", style = "font-weight: bold;"),

      dateRangeInput("date_range", "Date Range:",
                     start = min(synthetic_data$received_date),
                     end = max(synthetic_data$received_date)),

      selectInput("product_type", "Product Type:",
                  choices = c("All", unique(synthetic_data$product_type)),
                  selected = "All"),

      selectInput("seriousness", "Seriousness:",
                  choices = c("All", unique(synthetic_data$seriousness)),
                  selected = "All"),

      selectInput("outcome", "Outcome:",
                  choices = c("All", unique(synthetic_data$outcome)),
                  selected = "All"),

      selectInput("gender", "Patient Gender:",
                  choices = c("All", unique(synthetic_data$patient_gender)),
                  selected = "All"),

      sliderInput("age_range", "Age Range:",
                  min = 0, max = 100, value = c(0, 100)),

      actionButton("reset_filters", "Reset Filters",
                   icon = icon("refresh"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),

    # Export buttons
    div(
      style = "padding: 15px;",
      h4("Export Options", style = "font-weight: bold;"),
      downloadButton("download_data", "Download Data",
                     style = "width: 100%; margin-bottom: 10px;"),
      downloadButton("download_dashboard", "Download Dashboard",
                     style = "width: 100%;")
    )
  ),

  # Main panel
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #1d70b8;
        }
        .skin-blue .main-header .navbar {
          background-color: #1d70b8;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #1658a0;
        }
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .disclaimer {
          color: #666;
          font-style: italic;
          font-size: 12px;
          padding: 10px;
          background-color: #f9f9f9;
          border-left: 3px solid #1d70b8;
          margin-bottom: 20px;
        }
        .info-box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          transition: all 0.3s cubic-bezier(.25,.8,.25,1);
        }
        .info-box:hover {
          box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
        }
        .box {
          border-top: 3px solid #1d70b8;
        }
      "))
    ),

    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",

        # Disclaimer
        div(class = "disclaimer",
            strong("DISCLAIMER: "),
            "This dashboard displays synthetic (dummy) data generated for demonstration purposes only.
            The data does not represent actual adverse events and should not be used for regulatory decisions or analysis."),

        # Summary metrics
        fluidRow(
          valueBoxOutput("total_reports_box", width = 3),
          valueBoxOutput("serious_reports_box", width = 3),
          valueBoxOutput("death_reports_box", width = 3),
          valueBoxOutput("hospitalization_reports_box", width = 3)
        ),

        # Charts row 1
        fluidRow(
          # Reports Over Time
          box(
            title = "Reports Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("time_trend_plot", height = "300px")
          ),

          # Reports by Product Type
          box(
            title = "Reports by Product Type",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("product_type_plot", height = "300px")
          )
        ),

        # Charts row 2
        fluidRow(
          # Reports by Patient Age
          box(
            title = "Reports by Patient Age",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("age_distribution_plot", height = "300px")
          ),

          # Reports by Outcome
          box(
            title = "Reports by Outcome",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("outcome_plot", height = "300px")
          )
        ),

        # Charts row 3
        fluidRow(
          # Reports by Gender
          box(
            title = "Reports by Gender",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("gender_plot", height = "300px")
          ),

          # Reports by Country
          box(
            title = "Top 10 Countries",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("country_plot", height = "300px")
          )
        )
      ),

      # Tables tab
      tabItem(
        tabName = "tables",

        # Disclaimer for tables
        div(class = "disclaimer",
            strong("DISCLAIMER: "),
            "This dashboard displays synthetic (dummy) data generated for demonstration purposes only.
            The data does not represent actual adverse events and should not be used for regulatory decisions or analysis."),

        fluidRow(
          box(
            title = "Filtered Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        ),

        fluidRow(
          box(
            title = "Reports Summary by Year and Quarter",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("summary_table")
          )
        )
      ),

      # About tab
      tabItem(
        tabName = "about",
        box(
          title = "About This Dashboard",
          status = "primary",
          solidHeader = TRUE,
          width = 12,

          h3("FDA Adverse Events Reporting System (FAERS) Dashboard"),
          p("This is a demonstration dashboard mimicking the FDA's adverse event reporting system."),

          h4("Important Notes:"),
          tags$ul(
            tags$li(strong("Synthetic Data:"), "All data shown in this dashboard is synthetic and generated for demonstration purposes only."),
            tags$li(strong("Not for Clinical Use:"), "The information presented should not be used for regulatory decisions or clinical analysis."),
            tags$li(strong("Dashboard Features:"), "This dashboard demonstrates the visualization capabilities of R Shiny for creating interactive health data dashboards.")
          ),

          h4("How to Use This Dashboard:"),
          tags$ol(
            tags$li("Use the filters on the left sidebar to narrow down the data."),
            tags$li("View various visualizations on the Dashboard tab to understand trends and patterns."),
            tags$li("Explore the detailed data in the Data Tables tab."),
            tags$li("Download the filtered data or dashboard reports using the Download buttons.")
          ),

          h4("Download Options:"),
          tags$ul(
            tags$li(strong("Download Data:"), "Export the filtered dataset as a CSV file."),
            tags$li(strong("Download Dashboard:"), "Export a summary report of the current dashboard view as a PDF file.")
          )
        )
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    data <- synthetic_data

    # Apply date filter
    data <- data %>%
      filter(received_date >= input$date_range[1] & received_date <= input$date_range[2])

    # Apply product type filter
    if (input$product_type != "All") {
      data <- data %>% filter(product_type == input$product_type)
    }

    # Apply seriousness filter
    if (input$seriousness != "All") {
      data <- data %>% filter(seriousness == input$seriousness)
    }

    # Apply outcome filter
    if (input$outcome != "All") {
      data <- data %>% filter(outcome == input$outcome)
    }

    # Apply gender filter
    if (input$gender != "All") {
      data <- data %>% filter(patient_gender == input$gender)
    }

    # Apply age filter
    data <- data %>%
      filter(is.na(patient_age) | (patient_age >= input$age_range[1] & patient_age <= input$age_range[2]))

    return(data)
  })

  # Reset filters button
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range",
                         start = min(synthetic_data$received_date),
                         end = max(synthetic_data$received_date))
    updateSelectInput(session, "product_type", selected = "All")
    updateSelectInput(session, "seriousness", selected = "All")
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "gender", selected = "All")
    updateSliderInput(session, "age_range", value = c(0, 100))
  })

  # Summary metrics
  output$total_reports_box <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()), big.mark = ","),
      "Total Reports",
      icon = icon("file-medical"),
      color = "blue"
    )
  })

  output$serious_reports_box <- renderValueBox({
    serious_count <- filtered_data() %>%
      filter(seriousness == "Serious") %>%
      nrow()

    valueBox(
      format(serious_count, big.mark = ","),
      "Serious Reports",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$death_reports_box <- renderValueBox({
    death_count <- filtered_data() %>%
      filter(outcome == "Death") %>%
      nrow()

    valueBox(
      format(death_count, big.mark = ","),
      "Death Reports",
      icon = icon("heartbeat"),
      color = "red"
    )
  })

  output$hospitalization_reports_box <- renderValueBox({
    hosp_count <- filtered_data() %>%
      filter(outcome == "Hospitalization") %>%
      nrow()

    valueBox(
      format(hosp_count, big.mark = ","),
      "Hospitalization Reports",
      icon = icon("hospital"),
      color = "purple"
    )
  })

  # Time trend plot
  output$time_trend_plot <- renderPlotly({
    # Aggregate data by month
    time_data <- filtered_data() %>%
      mutate(month = floor_date(received_date, "month")) %>%
      group_by(month) %>%
      summarize(count = n())

    p <- ggplot(time_data, aes(x = month, y = count)) +
      geom_line(color = "#1d70b8", size = 1) +
      geom_point(color = "#1d70b8", size = 3) +
      theme_minimal() +
      labs(x = "Month", y = "Number of Reports") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>%
      layout(hovermode = "closest")
  })

  # Product type plot
  output$product_type_plot <- renderPlotly({
    product_data <- filtered_data() %>%
      group_by(product_type) %>%
      summarize(count = n()) %>%
      arrange(desc(count))

    p <- ggplot(product_data, aes(x = reorder(product_type, -count), y = count, fill = product_type)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      labs(x = "Product Type", y = "Number of Reports") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Age distribution plot
  output$age_distribution_plot <- renderPlotly({
    age_data <- filtered_data() %>%
      filter(!is.na(patient_age)) %>%
      mutate(age_group = cut(patient_age,
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                             labels = c("0-10", "11-20", "21-30", "31-40", "41-50",
                                        "51-60", "61-70", "71-80", "81-90", "91-100"),
                             include.lowest = TRUE)) %>%
      group_by(age_group) %>%
      summarize(count = n())

    p <- ggplot(age_data, aes(x = age_group, y = count, fill = age_group)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      labs(x = "Age Group", y = "Number of Reports") +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Outcome plot
  output$outcome_plot <- renderPlotly({
    outcome_data <- filtered_data() %>%
      group_by(outcome) %>%
      summarize(count = n()) %>%
      arrange(desc(count))

    p <- ggplot(outcome_data, aes(x = reorder(outcome, -count), y = count, fill = outcome)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Reds") +
      theme_minimal() +
      labs(x = "Outcome", y = "Number of Reports") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Gender plot
  output$gender_plot <- renderPlotly({
    gender_data <- filtered_data() %>%
      group_by(patient_gender) %>%
      summarize(count = n())

    colors <- c("Male" = "#1d70b8", "Female" = "#d53880", "Unknown" = "#6f777b")

    p <- plot_ly(gender_data, labels = ~patient_gender, values = ~count,
                 type = 'pie', hole = 0.4,
                 marker = list(colors = colors)) %>%
      layout(title = "",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    p
  })

  # Country plot
  output$country_plot <- renderPlotly({
    country_data <- filtered_data() %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)

    p <- ggplot(country_data, aes(x = reorder(country, count), y = count, fill = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "#cce5ff", high = "#005cb8") +
      theme_minimal() +
      labs(x = "", y = "Number of Reports") +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Data table
  output$data_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(report_id, received_date, product_type, report_type, patient_gender,
               patient_age, country, outcome, seriousness),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })

  # Summary table
  output$summary_table <- renderDT({
    summary_data <- filtered_data() %>%
      group_by(report_year, report_quarter) %>%
      summarize(
        total_reports = n(),
        serious_reports = sum(seriousness == "Serious"),
        death_reports = sum(outcome == "Death"),
        hospitalization_reports = sum(outcome == "Hospitalization"),
        .groups = 'drop'
      ) %>%
      arrange(desc(report_year), desc(report_quarter))

    summary_data$period <- paste0("Q", summary_data$report_quarter, " ", summary_data$report_year)

    datatable(
      summary_data %>%
        select(period, total_reports, serious_reports, death_reports, hospitalization_reports),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })

  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("fda-adverse-events-data-", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # Download dashboard
  output$download_dashboard <- downloadHandler(
    filename = function() {
      paste("fda-adverse-events-dashboard-", format(Sys.Date(), "%Y-%m-%d"), ".html", sep = "")
    },
    content = function(file) {
      # Create a simple HTML report with dashboard screenshots
      # In a real application, you might use R Markdown to generate a PDF/HTML report

      # Simple HTML report
      report_content <- paste0(
        "<!DOCTYPE html>
        <html>
        <head>
          <title>FDA Adverse Events Dashboard Report</title>
          <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            h1, h2 { color: #1d70b8; }
            .disclaimer { font-style: italic; color: #666; border-left: 3px solid #1d70b8; padding-left: 10px; }
            table { border-collapse: collapse; width: 100%; margin: 20px 0; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #f2f2f2; }
            .summary { background-color: #f9f9f9; padding: 15px; border-radius: 5px; margin: 20px 0; }
          </style>
        </head>
        <body>
          <h1>FDA Adverse Events Dashboard Report</h1>
          <p>Generated on: ", format(Sys.time(), "%B %d, %Y %H:%M:%S"), "</p>

          <div class='disclaimer'>
            <strong>DISCLAIMER:</strong> This report displays synthetic (dummy) data generated for demonstration purposes only.
            The data does not represent actual adverse events and should not be used for regulatory decisions or analysis.
          </div>

          <h2>Summary Statistics</h2>
          <div class='summary'>
            <p><strong>Total Reports:</strong> ", format(nrow(filtered_data()), big.mark = ","), "</p>
            <p><strong>Time Period:</strong> ", format(input$date_range[1]), " to ", format(input$date_range[2]), "</p>
            <p><strong>Product Type:</strong> ", input$product_type, "</p>
            <p><strong>Serious Reports:</strong> ",
        format(sum(filtered_data()$seriousness == "Serious"), big.mark = ","),
        " (", round(100 * sum(filtered_data()$seriousness == "Serious") / nrow(filtered_data()), 1), "%)</p>
            <p><strong>Death Reports:</strong> ",
        format(sum(filtered_data()$outcome == "Death"), big.mark = ","),
        " (", round(100 * sum(filtered_data()$outcome == "Death") / nrow(filtered_data()), 1), "%)</p>
          </div>

          <h2>Report Details</h2>
          <p>This report is based on the following filter criteria:</p>
          <ul>
            <li>Date Range: ", format(input$date_range[1]), " to ", format(input$date_range[2]), "</li>
            <li>Product Type: ", input$product_type, "</li>
            <li>Seriousness: ", input$seriousness, "</li>
            <li>Outcome: ", input$outcome, "</li>
            <li>Patient Gender: ", input$gender, "</li>
            <li>Age Range: ", input$age_range[1], " to ", input$age_range[2], "</li>
          </ul>

          <h2>Yearly Summary</h2>
          <table>
            <tr>
              <th>Year</th>
              <th>Quarter</th>
              <th>Total Reports</th>
              <th>Serious Reports</th>
              <th>Death Reports</th>
            </tr>"
      )

      # Add table rows for yearly summary
      yearly_data <- filtered_data() %>%
        group_by(report_year, report_quarter) %>%
        summarize(
          total_reports = n(),
          serious_reports = sum(seriousness == "Serious"),
          death_reports = sum(outcome == "Death"),
          .groups = 'drop'
        ) %>%
        arrange(desc(report_year), desc(report_quarter))

      for(i in 1:nrow(yearly_data)) {
        report_content <- paste0(
          report_content,
          "<tr>
            <td>", yearly_data$report_year[i], "</td>
            <td>Q", yearly_data$report_quarter[i], "</td>
            <td>", format(yearly_data$total_reports[i], big.mark = ","), "</td>
            <td>", format(yearly_data$serious_reports[i], big.mark = ","), "</td>
            <td>", format(yearly_data$death_reports[i], big.mark = ","), "</td>
          </tr>"
        )
      }

      report_content <- paste0(
        report_content,
        "  </table>

          <p>Note: This is a generated report based on the current dashboard view.
          For more detailed analysis, please use the interactive dashboard.</p>
        </body>
        </html>"
      )

      writeLines(report_content, file)
    },
    contentType = "text/html"
  )
}

# Run the app
shinyApp(ui = ui, server = server)

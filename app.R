library(shiny)
library(DT)
library(plotly)
library(bslib)
library(dplyr)
library(shinyvalidate)
library(bsicons)

# Custom CSS for additional styling
custom_css <- "
.value-box .value {
  font-weight: 600 !important;
}
.card-header {
  background-color: #f8f9fa;
  border-bottom: 1px solid #dee2e6;
}
.nav-pills {
  margin-bottom: 20px;
}
"

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cerulean"),
  tags$head(tags$style(custom_css)),
  
  # Header Section
  div(class = "header",
      fluidRow(
        column(2, img(src = "fda_logo.png", class = "img-fluid", style = "max-height: 60px;")),
        column(10, h1("FDA Adverse Events Reporting System (FAERS) Dashboard",
                      class = "display-6 mb-0"))
      ),
      hr(class = "my-3")
  ),
  
  # Key Metrics Section
  fluidRow(
    layout_columns(
      value_box(
        title = "Total Reports (2004-2024)",
        value = textOutput("totalReportsSum"),
        showcase = bs_icon("database", size = "2em"),
        theme = "primary",
        class = "border-0"
      ),
      value_box(
        title = "Expedited Reports",
        value = textOutput("expeditedReportsSum"),
        showcase = bs_icon("lightning-charge", size = "2em"),
        theme = "warning",
        class = "border-0"
      ),
      value_box(
        title = "Direct Reports",
        value = textOutput("directReportsSum"),
        showcase = bs_icon("send-exclamation", size = "2em"),
        theme = "danger",
        class = "border-0"
      ),
      value_box(
        title = "Non-Expedited Reports",
        value = textOutput("nonExpeditedReportsSum"),
        showcase = bs_icon("clock-history", size = "2em"),
        theme = "success",
        class = "border-0"
      )
    )
  ),
  
  # Main Content Section
  navset_pill(
    nav_panel("Data Explorer",
              card(
                card_header("Analysis Controls",
                            popover(
                              title = "Filter Information",
                              bs_icon("info-circle"),
                              "Select criteria to explore FAERS data from 2004-2024"
                            )),
                card_body(
                  fluidRow(
                    column(3,
                           selectInput("yearFilter", "Reporting Year",
                                       choices = c("All Years", 2004:2024),
                                       selected = "All Years")),
                    column(3,
                           selectInput("reportTypeFilter", "Report Type",
                                       choices = c("All Types", "Expedited", "Non Expedited", "Direct", "BSR"))),
                    column(3,
                           actionButton("resetFilters", "Reset Filters",
                                        class = "btn-outline-secondary mt-4"))
                  )
                )
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Report Summary Table"),
                  card_body(DTOutput("reportsTable"))
                ),
                card(
                  card_header("Trend Analysis"),
                  card_body(
                    plotlyOutput("trendChart"),
                    div(class = "text-muted mt-2",
                        "Hover over data points to view detailed information")
                  )
                )
              )
    ),
    
    nav_panel("Data Insights",
              card(
                card_header("Annual Report Composition"),
                layout_sidebar(
                  sidebar = sidebar(
                    selectInput("insightYear", "Select Year:",
                                choices = 2004:2024, width = "100%"),
                    hr(),
                    uiOutput("yearMetrics")
                  ),
                  card_body(
                    plotlyOutput("compositionChart"),
                    div(class = "text-center mt-3",
                        tags$small("Click legend items to toggle visibility"))
                  )
                )
              )
    ),
    
    nav_panel("About",
              card(
                card_header("About This Dashboard"),
                card_body(
                  markdown("
                 ### FAERS Public Dashboard Guide
                 This interactive dashboard provides access to anonymized adverse event reports
                 submitted to the FDA Adverse Event Reporting System (FAERS).

                 **Key Features:**
                 - Explore reports from 2004-2024
                 - Filter data by year and report type
                 - View trend analysis and report composition
                 - Export data tables

                 **Data Sources:**
                 - [FDA FAERS Public Dashboard](https://www.fda.gov/drugs/questions-and-answers-fdas-adverse-event-reporting-system-faers/fda-adverse-event-reporting-system-faers-public-dashboard)
                 - Updated quarterly with new data
                 ")
                ),
                card_footer(class = "text-muted",
                            "For research purposes only. Consult official sources for regulatory decisions.")
              )
    )
  )
)

server <- function(input, output, session) {
  # Data Initialization
  drugNames <- c("Acetaminophen", "Lisinopril", "Atorvastatin", "Levothyroxine",
                 "Metformin", "Amlodipine", "Metoprolol", "Omeprazole",
                 "Simvastatin", "Gabapentin")
  
  faers_data <- reactiveVal(
    tibble(
      Year = rep(2004:2024, each = 1),
      Total_Reports = sample(1000:10000, 21, replace = TRUE),
      Expedited = sample(1000:5000, 21, replace = TRUE),
      Non_Expedited = sample(1000:5000, 21, replace = TRUE),
      Direct = sample(1000:5000, 21, replace = TRUE),
      BSR = sample(1000:5000, 21, replace = TRUE),
      Drug_Name = sample(drugNames, 21, replace = TRUE),
      Report_Type = sample(c("Expedited", "Non Expedited", "Direct", "BSR"), 21, replace = TRUE)
    ) %>% mutate(Total_Reports = Expedited + Non_Expedited + Direct + BSR)
  )
  
  # Reactive Data Filtering
  filtered_data <- reactive({
    data <- faers_data()
    
    if (input$yearFilter != "All Years") {
      data <- filter(data, Year == as.integer(input$yearFilter))
    }
    
    if (input$reportTypeFilter != "All Types") {
      data <- filter(data, Report_Type == input$reportTypeFilter)
    }
    
    data
  }) %>% bindEvent(input$yearFilter, input$reportTypeFilter)
  
  # Summary Metrics
  output$totalReportsSum <- renderText(format(sum(faers_data()$Total_Reports), big.mark = ","))
  output$expeditedReportsSum <- renderText(format(sum(faers_data()$Expedited), big.mark = ","))
  output$nonExpeditedReportsSum <- renderText(format(sum(faers_data()$Non_Expedited), big.mark = ","))
  output$directReportsSum <- renderText(format(sum(faers_data()$Direct), big.mark = ","))
  
  # Data Table
  output$reportsTable <- renderDT({
    validate(need(nrow(filtered_data()) > 0, "No data available for selected filters"))
    
    datatable(
      filtered_data(),
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        order = list(list(0, 'desc'))
      ),
      rownames = FALSE,
      class = "compact stripe"
    ) %>% formatCurrency("Total_Reports", currency = "", digits = 0)
  })
  
  # Trend Chart (stacked bar chart)
  output$trendChart <- renderPlotly({
    data <- faers_data()
    
    plot_ly(data, x = ~Year) %>%
      add_bars(y = ~Expedited, name = "Expedited", marker = list(color = '#ff7f0e')) %>%
      add_bars(y = ~Non_Expedited, name = "Non-Expedited", marker = list(color = '#2ca02c')) %>%
      add_bars(y = ~Direct, name = "Direct", marker = list(color = '#1f77b4')) %>%
      add_bars(y = ~BSR, name = "BSR", marker = list(color = '#d62728')) %>%
      layout(
        title = "Reporting Trends Over Time",
        xaxis = list(title = "Year", fixedrange = TRUE),
        yaxis = list(title = "Number of Reports", fixedrange = TRUE),
        hovermode = "x unified",
        showlegend = TRUE,
        barmode = 'stack'
      )
  })
  
  # Composition Chart
  output$compositionChart <- renderPlotly({
    year_data <- filter(faers_data(), Year == input$insightYear)
    
    plot_ly(
      type = "pie",
      labels = c("Expedited", "Non-Expedited", "Direct", "BSR"),
      values = c(year_data$Expedited, year_data$Non_Expedited,
                 year_data$Direct, year_data$BSR),
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728'))
    )
  })
  
  # Reset Filters
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "yearFilter", selected = "All Years")
    updateSelectInput(session, "reportTypeFilter", selected = "All Types")
  })
  
  # Year Metrics Sidebar
  output$yearMetrics <- renderUI({
    year_data <- filter(faers_data(), Year == input$insightYear)
    
    tagList(
      h5(paste("Key Metrics for", input$insightYear)),
      div(class = "mb-2", strong("Total Reports:"), format(year_data$Total_Reports, big.mark = ",")),
      div(class = "mb-2", strong("Most Reported Drug:"), year_data$Drug_Name),
      div(class = "mb-2", strong("Most Common Type:"), year_data$Report_Type)
    )
  })
}

shinyApp(ui = ui, server = server)
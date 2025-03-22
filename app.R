#
# app.R
#

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(ggplot2)
library(lubridate)
library(bsicons)  # For nice icons in value boxes (install if not available)

# ----------------------
# 1) MOCK DATA CREATION
# ----------------------

set.seed(123)

n_samples <- 5000
adverse_events <- data.frame(
  # Date range extended to start at 2008 if you want older years:
  date = sample(seq(as.Date('2008-01-01'), as.Date('2023-12-31'), by="day"), 
                n_samples, replace=TRUE),
  
  # Example event outcomes:
  event_outcome = sample(
    c("Hospitalization", "Life-Threatening", "Disability", "Death", 
      "Required Intervention", "Other Serious", "Non-Serious"), 
    n_samples, replace=TRUE, 
    prob=c(0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.3)
  ),
  
  # Example patient demographics:
  patient_age = sample(1:100, n_samples, replace=TRUE),
  patient_gender = sample(c("Male", "Female", "Unknown"), 
                          n_samples, replace=TRUE, prob=c(0.45, 0.5, 0.05)),
  
  # Example reporter types:
  reporter_type = sample(
    c("Physician", "Consumer", "Pharmacist", 
      "Other Health Professional", "Lawyer"), 
    n_samples, replace=TRUE
  ),
  
  # Example drug info:
  drug_name = sample(
    c("Acetaminophen", "Ibuprofen", "Aspirin", "Lisinopril", "Atorvastatin",
      "Metformin", "Omeprazole", "Amlodipine", "Metoprolol", "Simvastatin",
      "Levothyroxine", "Gabapentin", "Losartan", "Sertraline", "Fluoxetine"), 
    n_samples, replace=TRUE
  ),
  drug_indication = sample(
    c("Pain", "Fever", "Hypertension", "Hyperlipidemia", "Diabetes", 
      "Acid Reflux", "Hypothyroidism", "Anxiety", "Depression", "Heart Disease"), 
    n_samples, replace=TRUE
  ),
  
  # Example reaction terms:
  reaction_term = sample(
    c("Nausea", "Headache", "Dizziness", "Rash", "Vomiting", "Abdominal Pain", 
      "Fatigue", "Diarrhea", "Pruritus", "Insomnia", "Dyspnea", "Chest Pain", 
      "Edema", "Anaphylaxis", "Liver Injury", "Renal Dysfunction"), 
    n_samples, replace=TRUE
  ),
  
  # Example location:
  state = sample(state.abb, n_samples, replace=TRUE),
  country = sample(
    c("United States", "Canada", "United Kingdom", "Germany", "France", "Japan",
      "Australia", "Other Foreign"), 
    n_samples, replace=TRUE, 
    prob=c(0.7, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.02)
  )
)

# Add a new "report_type" to replicate the stacked bar chart categories
adverse_events$report_type <- sample(
  c("Expedited","Non-Expedited","Direct","BSR"), 
  n_samples, replace=TRUE, 
  prob=c(0.2,0.5,0.2,0.1)
)

# Derived fields
adverse_events$year <- lubridate::year(adverse_events$date)
adverse_events$quarter <- paste0("Q", lubridate::quarter(adverse_events$date))
adverse_events$year_quarter <- paste(adverse_events$year, adverse_events$quarter)

# We can define an "age_group" for demonstration
adverse_events$age_group <- cut(
  adverse_events$patient_age, 
  breaks = c(0, 17, 44, 64, 100),
  labels = c("Pediatric (0-17)", "Adult (18-44)", "Middle Age (45-64)", "Senior (65+)"),
  right = TRUE
)

# ----------------------
# 2) USER INTERFACE
# ----------------------

ui <- page_navbar(
  title = span(
    # Example FDA logo (replace with actual link in production):
    img(
      src = "https://www.fda.gov/media/91293/download", 
      height = "30px", 
      style = "margin-right: 10px;", 
      alt = "FDA Logo"
    ),
    "FDA Adverse Events Reporting System (FAERS) Dashboard"
  ),
  bg = "#2c6aa0",  # FDA blue color
  theme = bs_theme(bootswatch = "flatly", primary = "#2c6aa0"),
  window_title = "FAERS Dashboard",
  
  # Links on right side of navbar
  nav_spacer(),
  nav_panel("Disclaimer", icon = icon("exclamation-circle"),
            href = "https://www.fda.gov/disclaimer", target = "_blank"),
  nav_panel("FAQ", icon = icon("question-circle"),
            href = "https://www.fda.gov/faqs", target = "_blank"),
  nav_menu(
    title = "Resources",
    icon = icon("book"),
    nav_panel("Report a Problem", icon = icon("flag"),
              href = "https://www.fda.gov/safety/report-problem", target = "_blank"),
    nav_panel("Site Feedback", icon = icon("comment"),
              href = "https://www.fda.gov/about-fda/website-policies/site-feedback", target = "_blank"),
    nav_panel("User Guide", icon = icon("file-alt"),
              href = "https://www.fda.gov/consumers/consumer-updates", target = "_blank")
  ),
  
  # Sidebar with filters
  sidebar = sidebar(
    width = 300,
    accordion(
      accordion_panel(
        "Date Filters",
        icon = icon("calendar"),
        dateRangeInput("dateRange", "Select Date Range:",
                       start = as.Date("2008-01-01"),
                       end = as.Date("2023-12-31"),
                       format = "mm/dd/yyyy"),
        selectInput("quarter", "Select Quarter:",
                    choices = c("All", unique(adverse_events$quarter)),
                    selected = "All")
      ),
      accordion_panel(
        "Patient Demographics",
        icon = icon("user"),
        selectInput("ageGroup", "Age Group:",
                    choices = c("All", levels(adverse_events$age_group)),
                    selected = "All"),
        selectInput("gender", "Gender:",
                    choices = c("All", unique(adverse_events$patient_gender)),
                    selected = "All")
      ),
      accordion_panel(
        "Event Details",
        icon = icon("stethoscope"),
        selectInput("drugName", "Drug/Product:",
                    choices = c("All", sort(unique(adverse_events$drug_name))),
                    selected = "All"),
        selectInput("outcome", "Event Outcome:",
                    choices = c("All", sort(unique(adverse_events$event_outcome))),
                    selected = "All"),
        selectInput("reaction", "Reaction:",
                    choices = c("All", sort(unique(adverse_events$reaction_term))),
                    selected = "All")
      ),
      accordion_panel(
        "Reporter Info",
        icon = icon("user-md"),
        selectInput("reporterType", "Reporter Type:",
                    choices = c("All", sort(unique(adverse_events$reporter_type))),
                    selected = "All"),
        selectInput("country", "Country:",
                    choices = c("All", sort(unique(adverse_events$country))),
                    selected = "All"),
        selectInput("state", "State (US only):",
                    choices = c("All", sort(unique(adverse_events$state))),
                    selected = "All")
      )
    ),
    
    hr(),
    actionButton("applyFilters", "Apply Filters", class = "btn-primary", width = "100%"),
    br(), br(),
    actionButton("resetFilters", "Reset All Filters", class = "btn-outline-secondary", width = "100%"),
    hr(),
    downloadButton("downloadData", "Download Data", class = "btn-info", width = "100%"),
    br(), br(),
    p("Data displayed is for demo purposes only and does not represent actual FAERS data.", 
      style = "font-size: 0.8em; color: #666;")
  ),
  
  # MAIN TABS
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    
    # === 3 Value Boxes ===
    layout_columns(
      value_box(
        title = "Total Reports",
        value = textOutput("box_total_reports"),
        showcase = bs_icon("database"),
        theme = "primary"
      ),
      value_box(
        title = "Serious Reports (Excluding Death)",
        value = textOutput("box_serious_excl_death"),
        showcase = bs_icon("exclamation-circle"),
        theme = "warning"
      ),
      value_box(
        title = "Death Reports",
        value = textOutput("box_death_reports"),
        showcase = bs_icon("radioactive"),
        theme = "danger"
      )
    ),
    
    # === Stacked Bar Chart ===
    layout_columns(
      card(
        card_header("Reports Received by Report Type"),
        plotlyOutput("reportTypePlot", height = "400px")
      )
    ),
    
    # Keep the data table or any other content if you like:
    layout_columns(
      card(
        card_header("Detailed Reports Data (Preview)"),
        DTOutput("table_data")
      )
    )
  ),
  
  nav_panel(
    title = "Search",
    icon = icon("search"),
    
    card(
      card_header("Advanced Search"),
      layout_columns(
        textInput("searchText", "Search Term:", placeholder = "Enter drug name, reaction, etc."),
        actionButton("runSearch", "Search", icon = icon("search"), class = "btn-primary")
      ),
      br(),
      DTOutput("searchResults")
    )
  ),
  
  nav_panel(
    title = "Explore Data",
    icon = icon("database"),
    
    card(
      card_header("Full Detailed Data"),
      DTOutput("detailedTable")
    )
  ),
  
  nav_panel(
    title = "Help",
    icon = icon("question"),
    
    card(
      card_header("How to Use This Dashboard"),
      layout_columns(
        card(
          card_header("Filtering Data"),
          p("Use the sidebar to filter data by various criteria:"),
          tags$ul(
            tags$li("Date filters allow temporal analysis."),
            tags$li("Patient demographics let you focus on specific population segments."),
            tags$li("Event details help analyze specific drugs or outcomes."),
            tags$li("Reporter information provides context on who reported events.")
          ),
          p("Click 'Apply Filters' to update all visualizations with your selections.")
        ),
        card(
          card_header("Understanding Visualizations"),
          p("The dashboard provides multiple ways to analyze adverse event data:"),
          tags$ul(
            tags$li("Summary metrics give an overview of key statistics."),
            tags$li("A stacked bar chart shows the distribution of reports by year and type."),
            tags$li("Data tables provide access to individual report details.")
          )
        )
      )
    )
  )
)

# ----------------------
# 3) SERVER LOGIC
# ----------------------

server <- function(input, output, session) {
  
  # Store current filter selections in a reactiveValues
  rv <- reactiveValues(
    dateRange = c(as.Date("2008-01-01"), as.Date("2023-12-31")),
    quarter = "All",
    ageGroup = "All",
    gender = "All",
    drugName = "All",
    outcome = "All",
    reaction = "All",
    reporterType = "All",
    country = "All",
    state = "All"
  )
  
  # When "Apply Filters" is clicked, update the reactiveValues
  observeEvent(input$applyFilters, {
    rv$dateRange <- input$dateRange
    rv$quarter <- input$quarter
    rv$ageGroup <- input$ageGroup
    rv$gender <- input$gender
    rv$drugName <- input$drugName
    rv$outcome <- input$outcome
    rv$reaction <- input$reaction
    rv$reporterType <- input$reporterType
    rv$country <- input$country
    rv$state <- input$state
  })
  
  # When "Reset All Filters" is clicked, reset everything to defaults
  observeEvent(input$resetFilters, {
    updateDateRangeInput(session, "dateRange", 
                         start = as.Date("2008-01-01"), 
                         end = as.Date("2023-12-31"))
    updateSelectInput(session, "quarter", selected = "All")
    updateSelectInput(session, "ageGroup", selected = "All")
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "drugName", selected = "All")
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "reaction", selected = "All")
    updateSelectInput(session, "reporterType", selected = "All")
    updateSelectInput(session, "country", selected = "All")
    updateSelectInput(session, "state", selected = "All")
    
    rv$dateRange <- c(as.Date("2008-01-01"), as.Date("2023-12-31"))
    rv$quarter <- "All"
    rv$ageGroup <- "All"
    rv$gender <- "All"
    rv$drugName <- "All"
    rv$outcome <- "All"
    rv$reaction <- "All"
    rv$reporterType <- "All"
    rv$country <- "All"
    rv$state <- "All"
  })
  
  # Reactive expression to return filtered data
  filteredData <- reactive({
    df <- adverse_events
    
    # Filter by date range
    df <- df[df$date >= rv$dateRange[1] & df$date <= rv$dateRange[2], ]
    
    # Filter by quarter
    if (rv$quarter != "All") {
      df <- df[df$quarter == rv$quarter, ]
    }
    
    # Filter by age group
    if (rv$ageGroup != "All") {
      df <- df[df$age_group == rv$ageGroup, ]
    }
    
    # Filter by gender
    if (rv$gender != "All") {
      df <- df[df$patient_gender == rv$gender, ]
    }
    
    # Filter by drug
    if (rv$drugName != "All") {
      df <- df[df$drug_name == rv$drugName, ]
    }
    
    # Filter by outcome
    if (rv$outcome != "All") {
      df <- df[df$event_outcome == rv$outcome, ]
    }
    
    # Filter by reaction
    if (rv$reaction != "All") {
      df <- df[df$reaction_term == rv$reaction, ]
    }
    
    # Filter by reporter type
    if (rv$reporterType != "All") {
      df <- df[df$reporter_type == rv$reporterType, ]
    }
    
    # Filter by country
    if (rv$country != "All") {
      df <- df[df$country == rv$country, ]
    }
    
    # Filter by state
    if (rv$state != "All") {
      df <- df[df$state == rv$state, ]
    }
    
    df
  })
  
  # Download handler for the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("faers_data_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  # === 3 Value Boxes (Reactive) ===
  # 1) Total Reports
  output$box_total_reports <- renderText({
    nrow(filteredData())
  })
  
  # 2) Serious Reports (Excluding Death)
  #    We'll define "Serious" as anything not "Non-Serious",
  #    but also exclude "Death" from that count:
  output$box_serious_excl_death <- renderText({
    df <- filteredData()
    sum(df$event_outcome %in% c("Hospitalization","Life-Threatening",
                                "Disability","Required Intervention",
                                "Other Serious"))
  })
  
  # 3) Death Reports
  output$box_death_reports <- renderText({
    sum(filteredData()$event_outcome == "Death")
  })
  
  # === Stacked Bar Chart: "Reports Received by Report Type" ===
  output$reportTypePlot <- renderPlotly({
    df <- filteredData()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title="No data"))
    }
    # Group by year + report_type, count how many
    df_by_year <- df %>%
      group_by(year, report_type) %>%
      summarize(count = n(), .groups = "drop")
    
    # Create a stacked bar chart
    plot_ly(
      data = df_by_year,
      x = ~year,
      y = ~count,
      color = ~report_type,
      type = "bar",
      text = ~paste(
        "Year:", year,
        "<br>Report Type:", report_type,
        "<br>Report Count:", count
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        title = "Reports Received by Report Type",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Report Count")
      )
  })
  
  # === Data table (Preview) ===
  output$table_data <- renderDT({
    datatable(filteredData(), 
              options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  # === Full Detailed Table (Explore Data tab) ===
  output$detailedTable <- renderDT({
    datatable(filteredData(), 
              options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  # === Search logic ===
  searchData <- reactiveVal(data.frame())
  
  observeEvent(input$runSearch, {
    txt <- tolower(input$searchText)
    if (nzchar(txt)) {
      # Filter rows where any relevant field contains the search text
      df <- adverse_events %>%
        filter(
          grepl(txt, tolower(drug_name)) |
            grepl(txt, tolower(reaction_term)) |
            grepl(txt, tolower(event_outcome)) |
            grepl(txt, tolower(drug_indication))
        )
      searchData(df)
    } else {
      searchData(data.frame())  # empty if no input
    }
  })
  
  output$searchResults <- renderDT({
    datatable(searchData(), 
              options = list(pageLength=10, autoWidth=TRUE, scrollX=TRUE))
  })
}

# Run the Shiny app
shinyApp(ui, server)
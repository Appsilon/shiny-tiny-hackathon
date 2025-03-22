library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(markdown)


# Sample data generation - In a real app, you would connect to the FDA API or download their data
generate_fda_data <- function(n = 10000) {
  set.seed(123)

  # Date range (2010-2024)
  dates <- seq(as.Date("2010-01-01"), as.Date("2024-10-01"), by = "day")

  # Drug names (focus on common drugs reported in FAERS)
  drug_names <- c(
    "HUMIRA", "XARELTO", "JANUVIA", "ELIQUIS", "KEYTRUDA",
    "ENBREL", "OZEMPIC", "OPDIVO", "JARDIANCE", "EYLEA",
    "REVLIMID", "RITUXAN", "TRULICITY", "REMICADE", "IMBRUVICA"
  )

  # Reaction types
  reaction_types <- c(
    "Fatigue", "Headache", "Nausea", "Dyspnea", "Diarrhea",
    "Pain", "Dizziness", "Vomiting", "Rash", "Fever",
    "Arthralgia", "Cough", "Pruritus", "Anemia", "Asthenia",
    "Neutropenia", "Thrombocytopenia", "Death", "Hypertension", "Depression"
  )

  # Patient demographics
  age_groups <- c("<18", "18-44", "45-64", "65-74", "75-85", ">85")
  sex <- c("Male", "Female", "Unknown")

  # Serious outcomes (based on FDA criteria)
  serious_outcomes <- c(
    "Death", "Life-Threatening", "Hospitalization",
    "Disability", "Congenital Anomaly", "Other Serious", "None"
  )

  # Reporter types
  reporter_types <- c(
    "Physician", "Pharmacist", "Other Health Professional",
    "Consumer", "Lawyer", "Manufacturer"
  )

  # Country distribution (weighted toward US as in real FAERS data)
  countries <- c(
    "United States", "Japan", "United Kingdom", "France", "Germany",
    "Canada", "Italy", "Spain", "Australia", "Brazil", "Other"
  )
  country_weights <- c(0.6, 0.1, 0.05, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.02, 0.05)

  # Generate data
  data <- data.frame(
    report_id = paste0("FDA-", sample(1000000:9999999, n, replace = FALSE)),
    received_date = sample(dates, n, replace = TRUE),
    drug_name = sample(drug_names, n, replace = TRUE),
    primary_reaction = sample(reaction_types, n, replace = TRUE),
    age_group = sample(age_groups, n, replace = TRUE),
    sex = sample(sex, n, replace = TRUE, prob = c(0.45, 0.48, 0.07)),
    serious_outcome = sample(serious_outcomes, n, replace = TRUE, prob = c(0.05, 0.08, 0.15, 0.05, 0.02, 0.15, 0.5)),
    reporter_type = sample(reporter_types, n, replace = TRUE),
    reporter_country = sample(countries, n, replace = TRUE, prob = country_weights),
    initial_or_followup = sample(c("Initial", "Follow-up"), n, replace = TRUE, prob = c(0.8, 0.2)),
    stringsAsFactors = FALSE
  )

  # Add time-based fields
  data$year <- year(data$received_date)
  data$quarter <- paste0("Q", quarter(data$received_date))
  data$year_quarter <- paste(data$year, data$quarter)
  data$month <- month(data$received_date, label = TRUE, abbr = TRUE)
  data$year_month <- format(data$received_date, "%Y-%m")

  # Add seriousness flag based on outcome
  data$is_serious <- data$serious_outcome != "None"

  # Add random patient weight and other demographics to make it more realistic
  data$patient_weight_kg <- round(rnorm(n, mean = 75, sd = 15), 1)
  data$concomitant_drugs_count <- sample(0:10, n, replace = TRUE, prob = c(0.2, rep(0.08, 10)))

  # Create additional reactions (common co-occurring symptoms)
  secondary_reactions <- lapply(1:n, function(i) {
    # Exclude the primary reaction
    available_reactions <- setdiff(reaction_types, data$primary_reaction[i])
    # Select 0-5 additional reactions
    num_reactions <- sample(0:5, 1, prob = c(0.3, 0.3, 0.2, 0.1, 0.05, 0.05))
    if(num_reactions > 0) {
      paste(sample(available_reactions, num_reactions), collapse = ", ")
    } else {
      NA_character_
    }
  })
  data$secondary_reactions <- unlist(secondary_reactions)

  return(data)
}

# Generate sample data
adverse_events <- generate_fda_data(12000)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "FDA Adverse Events Dashboard"),
  dashboardSidebar(
    # Main menu
    sidebarMenu(
      id = "sidebar",
      menuItem("Summary Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Report Listing", tabName = "reports", icon = icon("list")),
      menuItem("Drug Analysis", tabName = "drug_analysis", icon = icon("pills")),
      menuItem("Demographic Analysis", tabName = "demographics", icon = icon("users")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # Filters section
    div(
      style = "padding: 15px;",
      h4("Filter Options", style = "font-weight: bold;"),
      dateRangeInput("date_range", "Report Date Range",
                     start = as.Date("2022-01-01"),
                     end = max(adverse_events$received_date)),

      pickerInput("drug_select", "Select Drug(s)",
                  choices = c("All", sort(unique(adverse_events$drug_name))),
                  selected = "All",
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),

      pickerInput("reaction_select", "Select Primary Reaction(s)",
                  choices = c("All", sort(unique(adverse_events$primary_reaction))),
                  selected = "All",
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),

      radioButtons("serious_filter", "Serious Outcomes",
                   choices = c("All Reports" = "all",
                               "Serious Only" = "serious",
                               "Non-serious Only" = "non_serious"),
                   selected = "all"),

      pickerInput("country_select", "Reporter Country",
                  choices = c("All", sort(unique(adverse_events$reporter_country))),
                  selected = "All",
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),

      actionButton("apply_filters", "Apply Filters",
                   icon = icon("filter"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%;"),

      actionButton("reset_filters", "Reset Filters",
                   icon = icon("sync"),
                   style = "color: #333; background-color: #fff; border-color: #ccc; width: 100%; margin-top: 5px;")
    )
  ),

  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_reports_box", width = 3),
                valueBoxOutput("serious_events_box", width = 3),
                valueBoxOutput("drugs_involved_box", width = 3),
                valueBoxOutput("death_events_box", width = 3)
              ),

              fluidRow(
                box(title = "Reports Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("reports_time_trend", height = 300), width = 12)
              ),

              fluidRow(
                box(title = "Top 10 Reported Drugs", status = "primary", solidHeader = TRUE,
                    plotlyOutput("top_drugs_plot", height = 300), width = 6),

                box(title = "Top 10 Reported Reactions", status = "primary", solidHeader = TRUE,
                    plotlyOutput("top_reactions_plot", height = 300), width = 6)
              ),

              fluidRow(
                box(title = "Serious Outcomes", status = "primary", solidHeader = TRUE,
                    plotlyOutput("serious_outcomes_plot", height = 300), width = 12)
              )
      ),

      # Reports Tab
      tabItem(tabName = "reports",
              box(title = "Individual Case Reports", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("reports_table"))
      ),

      # Drug Analysis Tab
      tabItem(tabName = "drug_analysis",
              fluidRow(
                box(title = "Drug Comparison", status = "primary", solidHeader = TRUE, width = 12,
                    column(width = 4,
                           pickerInput("drug_compare", "Select Drugs to Compare",
                                       choices = sort(unique(adverse_events$drug_name)),
                                       selected = unique(adverse_events$drug_name)[1:3],
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, `max-options` = 5))
                    ),
                    column(width = 8,
                           plotlyOutput("drug_comparison_plot", height = 300)
                    )
                )
              ),

              fluidRow(
                box(title = "Reactions by Selected Drugs", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("drug_reactions_plot", height = 400))
              ),

              fluidRow(
                box(title = "Serious Outcomes by Drug", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("drug_outcomes_plot", height = 350))
              )
      ),

      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Reports by Age Group and Sex", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(width = 6, plotlyOutput("age_group_plot", height = 300)),
                      column(width = 6, plotlyOutput("sex_plot", height = 300))
                    )
                )
              ),

              fluidRow(
                box(title = "Reports by Country", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("country_plot", height = 350)),

                box(title = "Reports by Reporter Type", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("reporter_plot", height = 350))
              )
      ),

      # About Tab
      tabItem(tabName = "about",
              box(title = "About the FDA Adverse Events Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                  HTML("
              <h3>Overview</h3>
              <p>This dashboard provides an interactive visualization of adverse event reports submitted to the FDA through the FDA Adverse Event Reporting System (FAERS).</p>

              <h3>Data Source</h3>
              <p>This is a simulation based on the structure of the FDA FAERS data. In a production environment, this would connect to the FDA's API or use downloaded FAERS data.</p>

              <h3>Features</h3>
              <ul>
                <li>Filter reports by date range, drug, reaction type, seriousness, and country</li>
                <li>View trends over time and top reported drugs and reactions</li>
                <li>Analyze reports by demographics including age, sex, and country</li>
                <li>Compare different drugs and their associated reactions</li>
                <li>Access individual case reports</li>
              </ul>

              <h3>Usage Notes</h3>
              <p>FAERS data has limitations and biases. Reports do not necessarily indicate causation between a drug and an adverse event. The data represents spontaneous reports and is not normalized for drug usage rates in the population.</p>

              <h3>FDA Disclaimer</h3>
              <p>FAERS data does not undergo extensive validation. There is no certainty that the reported event was actually due to the product. FDA does not require that a causal relationship between a product and event be proven.</p>
            ")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactiveVal(adverse_events %>% filter(received_date >= as.Date("2022-01-01")))

  # Apply filters when button is clicked
  observeEvent(input$apply_filters, {
    data <- adverse_events

    # Date filter
    data <- data %>% filter(received_date >= input$date_range[1] & received_date <= input$date_range[2])

    # Drug filter
    if (!("All" %in% input$drug_select) && length(input$drug_select) > 0) {
      data <- data %>% filter(drug_name %in% input$drug_select)
    }

    # Reaction filter
    if (!("All" %in% input$reaction_select) && length(input$reaction_select) > 0) {
      data <- data %>% filter(primary_reaction %in% input$reaction_select)
    }

    # Serious filter
    if (input$serious_filter == "serious") {
      data <- data %>% filter(is_serious == TRUE)
    } else if (input$serious_filter == "non_serious") {
      data <- data %>% filter(is_serious == FALSE)
    }

    # Country filter
    if (!("All" %in% input$country_select) && length(input$country_select) > 0) {
      data <- data %>% filter(reporter_country %in% input$country_select)
    }

    filtered_data(data)
  })

  # Reset filters
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range",
                         start = as.Date("2022-01-01"),
                         end = max(adverse_events$received_date))
    updatePickerInput(session, "drug_select", selected = "All")
    updatePickerInput(session, "reaction_select", selected = "All")
    updateRadioButtons(session, "serious_filter", selected = "all")
    updatePickerInput(session, "country_select", selected = "All")

    filtered_data(adverse_events %>% filter(received_date >= as.Date("2022-01-01")))
  })

  # Value boxes
  output$total_reports_box <- renderValueBox({
    valueBox(
      formatC(nrow(filtered_data()), format="d", big.mark=","),
      "Total Reports",
      icon = icon("file-medical"),
      color = "blue"
    )
  })

  output$serious_events_box <- renderValueBox({
    serious_count <- filtered_data() %>%
      filter(is_serious == TRUE) %>%
      nrow()

    valueBox(
      formatC(serious_count, format="d", big.mark=","),
      "Serious Reports",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$drugs_involved_box <- renderValueBox({
    unique_drugs <- filtered_data() %>%
      distinct(drug_name) %>%
      nrow()

    valueBox(
      unique_drugs,
      "Unique Drugs",
      icon = icon("pills"),
      color = "green"
    )
  })

  output$death_events_box <- renderValueBox({
    death_count <- filtered_data() %>%
      filter(serious_outcome == "Death") %>%
      nrow()

    valueBox(
      formatC(death_count, format="d", big.mark=","),
      "Fatal Reports",
      icon = icon("skull-crossbones"),
      color = "red"
    )
  })

  # Time trend plot
  output$reports_time_trend <- renderPlotly({
    time_data <- filtered_data() %>%
      mutate(year_quarter = factor(year_quarter, levels = unique(year_quarter[order(year, quarter)]))) %>%
      group_by(year_quarter) %>%
      summarise(
        total_reports = n(),
        serious_reports = sum(is_serious),
        death_reports = sum(serious_outcome == "Death"),
        .groups = 'drop'
      )

    # Create plot with multiple traces
    plot_ly(time_data, x = ~year_quarter) %>%
      add_trace(y = ~total_reports, name = 'All Reports',
                type = 'scatter', mode = 'lines+markers', line = list(color = '#1f77b4')) %>%
      add_trace(y = ~serious_reports, name = 'Serious Reports',
                type = 'scatter', mode = 'lines+markers', line = list(color = '#ff7f0e')) %>%
      add_trace(y = ~death_reports, name = 'Fatal Reports',
                type = 'scatter', mode = 'lines+markers', line = list(color = '#d62728')) %>%
      layout(title = "Adverse Event Reports by Quarter",
             xaxis = list(title = "Quarter", tickangle = 45),
             yaxis = list(title = "Number of Reports"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center"),
             hovermode = "x unified")
  })

  # Top drugs plot
  output$top_drugs_plot <- renderPlotly({
    drug_data <- filtered_data() %>%
      group_by(drug_name) %>%
      summarise(
        total_count = n(),
        serious_count = sum(is_serious),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_count)) %>%
      head(10) %>%
      mutate(drug_name = factor(drug_name, levels = drug_name[order(total_count)]))

    plot_ly(drug_data, y = ~drug_name) %>%
      add_trace(x = ~total_count, name = 'All Reports', type = 'bar', orientation = 'h',
                marker = list(color = '#1f77b4')) %>%
      add_trace(x = ~serious_count, name = 'Serious Reports', type = 'bar', orientation = 'h',
                marker = list(color = '#ff7f0e')) %>%
      layout(title = "Top 10 Drugs by Number of Reports",
             barmode = 'group',
             xaxis = list(title = "Number of Reports"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", x = 0.5, xanchor = "center"),
             margin = list(l = 120))
  })

  # Top reactions plot
  output$top_reactions_plot <- renderPlotly({
    reaction_data <- filtered_data() %>%
      group_by(primary_reaction) %>%
      summarise(
        total_count = n(),
        serious_count = sum(is_serious),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_count)) %>%
      head(10) %>%
      mutate(primary_reaction = factor(primary_reaction, levels = primary_reaction[order(total_count)]))

    plot_ly(reaction_data, y = ~primary_reaction) %>%
      add_trace(x = ~total_count, name = 'All Reports', type = 'bar', orientation = 'h',
                marker = list(color = '#1f77b4')) %>%
      add_trace(x = ~serious_count, name = 'Serious Reports', type = 'bar', orientation = 'h',
                marker = list(color = '#ff7f0e')) %>%
      layout(title = "Top 10 Reactions by Number of Reports",
             barmode = 'group',
             xaxis = list(title = "Number of Reports"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", x = 0.5, xanchor = "center"),
             margin = list(l = 120))
  })

  # Serious outcomes plot
  output$serious_outcomes_plot <- renderPlotly({
    outcome_data <- filtered_data() %>%
      filter(serious_outcome != "None") %>%
      group_by(serious_outcome) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))

    plot_ly(outcome_data, x = ~reorder(serious_outcome, count), y = ~count, type = 'bar',
            marker = list(color = '#ff7f0e')) %>%
      layout(title = "Distribution of Serious Outcomes",
             xaxis = list(title = "Outcome Type"),
             yaxis = list(title = "Number of Reports"))
  })

  # Reports table
  output$reports_table <- renderDT({
    report_data <- filtered_data() %>%
      select(
        report_id, received_date, drug_name, primary_reaction, secondary_reactions,
        age_group, sex, serious_outcome, reporter_type, reporter_country
      )

    datatable(
      report_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(width = '120px', targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9)),
          list(width = '250px', targets = 4)
        )
      ),
      rownames = FALSE,
      filter = 'top',
      escape = FALSE
    ) %>%
      formatStyle('serious_outcome',
                  backgroundColor = styleEqual(
                    c("Death", "Life-Threatening", "Hospitalization", "Disability", "Congenital Anomaly", "Other Serious", "None"),
                    c("#d62728", "#ff7f0e", "#ffbb78", "#ffbb78", "#ff9896", "#ffbb78", "#ffffff")
                  ))
  })

  # Drug comparison plot
  output$drug_comparison_plot <- renderPlotly({
    req(input$drug_compare)

    if(length(input$drug_compare) == 0) return(NULL)

    drug_time_data <- filtered_data() %>%
      filter(drug_name %in% input$drug_compare) %>%
      mutate(year_quarter = factor(year_quarter, levels = unique(year_quarter[order(year, quarter)]))) %>%
      group_by(drug_name, year_quarter) %>%
      summarise(count = n(), .groups = 'drop')

    plot <- plot_ly()

    colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd')

    for(i in 1:length(input$drug_compare)) {
      drug <- input$drug_compare[i]
      color_idx <- (i-1) %% length(colors) + 1

      drug_data <- drug_time_data %>% filter(drug_name == drug)

      plot <- plot %>% add_trace(
        data = drug_data,
        x = ~year_quarter,
        y = ~count,
        name = drug,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colors[color_idx])
      )
    }

    plot %>% layout(
      title = "Reports Over Time by Selected Drugs",
      xaxis = list(title = "Quarter", tickangle = 45),
      yaxis = list(title = "Number of Reports"),
      hovermode = "x unified"
    )
  })

  # Drug reactions plot
  output$drug_reactions_plot <- renderPlotly({
    req(input$drug_compare)

    if(length(input$drug_compare) == 0) return(NULL)

    # Get top 5 reactions for each selected drug
    drug_reactions <- filtered_data() %>%
      filter(drug_name %in% input$drug_compare) %>%
      group_by(drug_name, primary_reaction) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(drug_name) %>%
      arrange(desc(count), .by_group = TRUE) %>%
      slice_head(n = 5) %>%
      ungroup()

    plot_ly(drug_reactions, x = ~drug_name, y = ~count, color = ~primary_reaction,
            type = 'bar', text = ~primary_reaction) %>%
      layout(title = "Top 5 Reactions by Selected Drugs",
             xaxis = list(title = "Drug"),
             yaxis = list(title = "Number of Reports"),
             barmode = 'stack',
             legend = list(title = list(text = "Reaction")))
  })

  # Drug outcomes plot
  output$drug_outcomes_plot <- renderPlotly({
    req(input$drug_compare)

    if(length(input$drug_compare) == 0) return(NULL)

    drug_outcomes <- filtered_data() %>%
      filter(drug_name %in% input$drug_compare, serious_outcome != "None") %>%
      group_by(drug_name, serious_outcome) %>%
      summarise(count = n(), .groups = 'drop')

    plot_ly(drug_outcomes, x = ~drug_name, y = ~count, color = ~serious_outcome,
            type = 'bar', text = ~serious_outcome) %>%
      layout(title = "Serious Outcomes by Selected Drugs",
             xaxis = list(title = "Drug"),
             yaxis = list(title = "Number of Reports"),
             barmode = 'stack',
             legend = list(title = list(text = "Outcome")))
  })

  # Age group plot
  output$age_group_plot <- renderPlotly({
    age_data <- filtered_data() %>%
      group_by(age_group) %>%
      summarise(
        count = n(),
        serious_count = sum(is_serious),
        .groups = 'drop'
      ) %>%
      mutate(
        age_group = factor(age_group, levels = c("<18", "18-44", "45-64", "65-74", "75-85", ">85"))
      )

    plot_ly(age_data, x = ~age_group) %>%
      add_trace(y = ~count, name = 'All Reports', type = 'bar',
                marker = list(color = '#1f77b4')) %>%
      add_trace(y = ~serious_count, name = 'Serious Reports', type = 'bar',
                marker = list(color = '#ff7f0e')) %>%
      layout(title = "Reports by Age Group",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Number of Reports"),
             barmode = 'group',
             legend = list(orientation = "h", x = 0.5, xanchor = "center"))
  })

  # Sex plot
  output$sex_plot <- renderPlotly({
    sex_data <- filtered_data() %>%
      group_by(sex) %>%
      summarise(
        count = n(),
        serious_count = sum(is_serious),
        .groups = 'drop'
      )

    plot_ly(sex_data, x = ~sex) %>%
      add_trace(y = ~count, name = 'All Reports', type = 'bar',
                marker = list(color = '#1f77b4')) %>%
      add_trace(y = ~serious_count, name = 'Serious Reports', type = 'bar',
                marker = list(color = '#ff7f0e')) %>%
      layout(title = "Reports by Sex",
             xaxis = list(title = "Sex"),
             yaxis = list(title = "Number of Reports"),
             barmode = 'group',
             legend = list(orientation = "h", x = 0.5, xanchor = "center"))
  })

  # Country plot
  output$country_plot <- renderPlotly({
    country_data <- filtered_data() %>%
      group_by(reporter_country) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)

    plot_ly(country_data, labels = ~reporter_country, values = ~count, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = RColorBrewer::brewer.pal(10, "Paired"))) %>%
      layout(title = "Top 10 Reporter Countries")
  })

  # Reporter type plot
  output$reporter_plot <- renderPlotly({
    reporter_data <- filtered_data() %>%
      group_by(reporter_type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))

    plot_ly(reporter_data, labels = ~reporter_type, values = ~count, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = RColorBrewer::brewer.pal(6, "Set3"))) %>%
      layout(title = "Reports by Reporter Type")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

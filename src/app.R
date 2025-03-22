
# Script created with Perplexity AI and Claude Sonnet
# FAERS Dashboard Shiny Application

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(shinyWidgets)
library(openxlsx)

# Load mock data
# source("./src/mock_data.R")
mock_faers_data <- readRDS("./data/mock_faers_data.rds")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "FAERS Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Adverse Events", tabName = "events", icon = icon("pills")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Geographic Distribution", tabName = "geography", icon = icon("globe")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Disproportionality Analysis", tabName = "disproportionality_analysis", icon = icon("balance-scale"))
    ),
    
    dateRangeInput("date_range", "Date Range:",
                   start = min(mock_faers_data$date_received),
                   end = max(mock_faers_data$date_received)),
    
    checkboxGroupInput("age_groups", "Age Groups:",
                       choices = unique(mock_faers_data$age_group),
                       selected = unique(mock_faers_data$age_group)),
    
    checkboxGroupInput("sex", "Sex:",
                       choices = unique(mock_faers_data$patient_sex),
                       selected = unique(mock_faers_data$patient_sex)),
    
    pickerInput("drug_name", "Drug Name:",
                choices = unique(mock_faers_data$drug_name),
                selected = unique(mock_faers_data$drug_name)[1:3],
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_reports"),
                valueBoxOutput("unique_drugs"),
                valueBoxOutput("unique_events")
              ),
              fluidRow(
                box(plotlyOutput("reports_over_time"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("top_drugs"), width = 6),
                box(plotlyOutput("top_events"), width = 6)
              )
      ),
      
      tabItem(tabName = "events",
              fluidRow(
                box(plotlyOutput("events_by_drug"), width = 12, height = 500)
              ),
              fluidRow(
                box(plotlyOutput("outcomes_by_event"), width = 12)
              )
      ),
      
      tabItem(tabName = "demographics",
              fluidRow(
                box(plotlyOutput("age_distribution"), width = 6),
                box(plotlyOutput("sex_distribution"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("age_by_event"), width = 12)
              )
      ),
      
      tabItem(tabName = "geography",
              fluidRow(
                box(leafletOutput("event_map"), width = 12, height = 500)
              ),
              fluidRow(
                box(plotlyOutput("events_by_country"), width = 12)
              )
      ),
      
      tabItem(tabName = "explorer",
              fluidRow(
                box(DTOutput("data_table"), width = 12)
              ),
              fluidRow(
                box(
                  title = "Download Filtered Data",
                  downloadButton("download_data", "Download CSV"),
                  downloadButton("download_data_excel", "Download Excel"),
                  width = 12
                )
              )
      ),
      
      tabItem(tabName = "disproportionality_analysis",
              fluidRow(
                box(DTOutput("disproportionality_analysis"), width = 12)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    mock_faers_data %>%
      filter(
        date_received >= input$date_range[1],
        date_received <= input$date_range[2],
        age_group %in% input$age_groups,
        patient_sex %in% input$sex,
        drug_name %in% input$drug_name
      )
  })
  
  output$total_reports <- renderValueBox({
    valueBox(
      nrow(filtered_data()), "Total Reports",
      icon = icon("file-medical"), color = "blue"
    )
  })
  
  output$unique_drugs <- renderValueBox({
    valueBox(
      length(unique(filtered_data()$drug_name)), "Unique Drugs",
      icon = icon("capsules"), color = "purple"
    )
  })
  
  output$unique_events <- renderValueBox({
    valueBox(
      length(unique(filtered_data()$adverse_event)), "Unique Adverse Events",
      icon = icon("exclamation-triangle"), color = "red"
    )
  })
  
  output$reports_over_time <- renderPlotly({
    time_data <- filtered_data() %>%
      count(quarter) %>%
      arrange(quarter)
    
    plot_ly(time_data, x = ~quarter, y = ~n, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Reports Over Time",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Number of Reports"))
  })
  
  output$top_drugs <- renderPlotly({
    drug_counts <- filtered_data() %>%
      count(drug_name, sort = TRUE) %>%
      head(10)
    
    plot_ly(drug_counts, x = ~reorder(drug_name, n), y = ~n, type = "bar") %>%
      layout(title = "Top 10 Drugs by Reports",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })
  
  output$top_events <- renderPlotly({
    event_counts <- filtered_data() %>%
      count(adverse_event, sort = TRUE) %>%
      head(10)
    
    plot_ly(event_counts, x = ~reorder(adverse_event, n), y = ~n, type = "bar") %>%
      layout(title = "Top 10 Adverse Events",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })
  
  output$events_by_drug <- renderPlotly({
    event_drug_data <- filtered_data() %>%
      count(drug_name, adverse_event) %>%
      group_by(drug_name) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) %>%
      ungroup()
    
    plot_ly(event_drug_data, x = ~drug_name, y = ~n, color = ~adverse_event, type = "bar") %>%
      layout(title = "Top Adverse Events by Drug",
             xaxis = list(title = "Drug", categoryorder = "total descending"),
             yaxis = list(title = "Number of Reports"),
             barmode = "stack",
             legend = list(title = list(text = "Adverse Event")))
  })
  
  output$outcomes_by_event <- renderPlotly({
    outcome_event_data <- filtered_data() %>%
      count(adverse_event, outcome) %>%
      group_by(adverse_event) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      arrange(desc(total)) %>%
      filter(adverse_event %in% (distinct(., adverse_event) %>% slice_head(n = 10) %>% pull(adverse_event)))
    
    plot_ly(outcome_event_data, x = ~adverse_event, y = ~n, color = ~outcome, type = "bar") %>%
      layout(title = "Outcomes by Top Adverse Events",
             xaxis = list(title = "Adverse Event", categoryorder = "total descending"),
             yaxis = list(title = "Number of Reports"),
             barmode = "stack",
             legend = list(title = list(text = "Outcome")))
  })
  
  output$age_distribution <- renderPlotly({
    age_data <- filtered_data() %>%
      filter(!is.na(patient_age))
    
    plot_ly(age_data, x = ~patient_age, type = "histogram", 
            marker = list(color = "steelblue", line = list(color = "white", width = 0.5))) %>%
      layout(title = "Age Distribution",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Number of Reports"))
  })
  
  output$sex_distribution <- renderPlotly({
    sex_data <- filtered_data() %>%
      count(patient_sex) %>%
      arrange(desc(n))
    
    plot_ly(sex_data, labels = ~patient_sex, values = ~n, type = "pie",
            textinfo = "label+percent",
            insidetextorientation = "radial") %>%
      layout(title = "Distribution by Sex",
             showlegend = TRUE)
  })
  
  output$age_by_event <- renderPlotly({
    age_event_data <- filtered_data() %>%
      filter(!is.na(patient_age)) %>%
      count(age_group, adverse_event) %>%
      group_by(adverse_event) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      arrange(desc(total)) %>%
      filter(adverse_event %in% (distinct(., adverse_event) %>% slice_head(n = 10) %>% pull(adverse_event)))
    
    plot_ly(age_event_data, x = ~adverse_event, y = ~n, color = ~age_group, type = "bar") %>%
      layout(title = "Age Groups by Top Adverse Events",
             xaxis = list(title = "Adverse Event", categoryorder = "total descending"),
             yaxis = list(title = "Number of Reports"),
             barmode = "stack",
             legend = list(title = list(text = "Age Group")))
  })
  
  output$event_map <- renderLeaflet({
    map_data <- filtered_data() %>%
      filter(country == "United States", !is.na(state)) %>%
      count(state) %>%
      mutate(state_name = state.name[match(state, state.abb)])
    
    state_coords <- data.frame(
      state = state.abb,
      lat = state.center$y,
      lng = state.center$x
    )
    
    map_data <- map_data %>%
      left_join(state_coords, by = "state")
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, 
        lat = ~lat,
        radius = ~sqrt(n) * 3,
        color = "blue",
        fillOpacity = 0.7,
        popup = ~paste0("<b>", state_name, "</b><br>Reports: ", n)
      ) %>%
      setView(lng = -98, lat = 39.5, zoom = 4)
  })
  
  output$events_by_country <- renderPlotly({
    country_data <- filtered_data() %>%
      count(country) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    plot_ly(country_data, x = ~reorder(country, n), y = ~n, type = "bar",
            marker = list(color = "darkblue")) %>%
      layout(title = "Reports by Country",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Reports"))
  })
  
  output$data_table <- renderDT({
    filtered_data() %>%
      select(report_id, date_received, drug_name, adverse_event, 
             patient_sex, age_group, outcome)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("faers-data-", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_data_excel <- downloadHandler(
    filename = function() {
      paste("faers-data-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data(), file)
    }
  )
  
  output$disproportionality_analysis <- renderDT({
    drug_event <- filtered_data() %>%
      group_by(drug_name, adverse_event) %>%
      summarize(count_drug_event = n(), .groups = "drop")
    
    total_events <- filtered_data() %>%
      group_by(adverse_event) %>%
      summarize(total_event = n(), .groups = "drop")
    
    total_drugs <- filtered_data() %>%
      group_by(drug_name) %>%
      summarize(total_drug = n(), .groups = "drop")
    
    total_reports <- nrow(filtered_data())
    
    disproportionality <- drug_event %>%
      left_join(total_events, by = "adverse_event") %>%
      left_join(total_drugs, by = "drug_name") %>%
      mutate(
        a = count_drug_event,
        b = total_drug - count_drug_event,
        c = total_event - count_drug_event,
        d = total_reports - a - b - c,
        ROR = (a/b)/(c/d),
        lower_CI = exp(log(ROR) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
        upper_CI = exp(log(ROR) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
      ) %>%
      filter(count_drug_event >= 3) %>%
      select(drug_name, adverse_event, count_drug_event, ROR, lower_CI, upper_CI) %>%
      arrange(desc(ROR))
    
    disproportionality
  })
}

shinyApp(ui, server)

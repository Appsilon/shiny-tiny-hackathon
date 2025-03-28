library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(RColorBrewer)
library(nnet)

# Function to simulate FAERS data
load_faers_data <- function() {
  set.seed(123)
  
  # Updated years range from 1996 to 2024
  years <- 1996:2024
  
  # Updated demographic categories with ordered age groups
  age_groups <- c("0-1 Month", "2 Months-2 Years", "3-11 Years", "12-17 Years", "18-64 Years", "65-85 Years", "More than 85 Years", "Not Specified")
  sexes <- c("Male", "Female", "Not Specified")
  reporters <- c("Healthcare Professional", "Consumer", "Not Specified", "Other")
  seriousness <- c("Death", "Serious", "Non-Serious")
  regions <- c("Domestic", "Foreign", "Not Specified")
  report_types <- c("Direct", "Expedited", "BSR", "Non-Expedited")
  
  # Create sample drug classes
  drug_classes <- c("Analgesics", "Antibiotics", "Antidepressants", "Antidiabetics", 
                    "Antihypertensives", "Anticoagulants", "Antipsychotics", 
                    "Antivirals", "Bronchodilators", "Corticosteroids")
  
  # Create sample data for adverse events
  n_reports <- sample(100000:200000, 1)
  faers_data <- data.frame(
    report_id = 1:n_reports,
    year = sample(years, n_reports, replace = TRUE, 
                  prob = c(rep(0.02, 15), rep(0.03, 5), rep(0.04, 5), rep(0.05, 4))), # Gradually increasing probability over the years
    quarter = sample(1:4, n_reports, replace = TRUE),
    age_group = factor(sample(age_groups, n_reports, replace = TRUE, 
                              prob = c(0.03, 0.05, 0.07, 0.05, 0.4, 0.2, 0.05, 0.15)),
                       levels = age_groups), # Set factor with correct order
    sex = sample(sexes, n_reports, replace = TRUE, 
                 prob = c(0.4, 0.55, 0.05)),
    reporter = sample(reporters, n_reports, replace = TRUE, 
                      prob = c(0.5, 0.35, 0.1, 0.05)),
    seriousness = sample(seriousness, n_reports, replace = TRUE, 
                         prob = c(0.1, 0.3, 0.6)),
    region = sample(regions, n_reports, replace = TRUE, 
                    prob = c(0.6, 0.35, 0.05)),
    report_type = sample(report_types, n_reports, replace = TRUE,
                         prob = c(0.15, 0.5, 0.2, 0.15)),
    drug_class = sample(drug_classes, n_reports, replace = TRUE)
  )
  
  # Add date field
  faers_data$date <- as.Date(paste0(faers_data$year, "-", faers_data$quarter * 3, "-15"))
  
  return(faers_data)
}



# Create dashboard tab UI
dashboard_tab <- page_fluid(
  # Summary stats
  layout_columns(
    col_widths = c(4, 4, 4),
    gap = "1rem",
    value_box(
      title = "Total Reports",
      value = textOutput("total_reports"),
      showcase = bsicons::bs_icon("clipboard-data"),
      theme = "primary"
    ),
    value_box(
      title = "Serious Reports (excl. Death)",
      value = textOutput("serious_reports"),
      showcase = bsicons::bs_icon("exclamation-triangle"),
      theme = "warning"
    ),
    value_box(
      title = "Death Reports",
      value = textOutput("death_reports"),
      showcase = bsicons::bs_icon("heart-pulse"),
      theme = "danger"
    )
  ),
  
  # Filter section with multiple filters 
  card(
    card_header("Data Filters"),
    card_body(
      layout_columns(
        col_widths = c(6, 6),
        radioButtons("year_range", "Time Period:",
                     choices = c("All Years", "Last 10 Years"),
                     selected = "All Years",
                     inline = TRUE),
        selectInput("chart_view", "Chart Visualization By:",
                    choices = c("Report Type", "Age", "Sex", "Reporter", "Seriousness", "Region"),
                    selected = "Report Type")
      )
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    gap = "1rem",
    card(
      full_screen = TRUE,
      card_header("Reports Breakdown by Year"),
      card_body(plotlyOutput("yearly_chart", height = "400px")),
      height = "500px"
    ),
    card(
      full_screen = TRUE,
      card_header("Data Table"),
      card_body(DTOutput("report_table")),
      height = "500px"
    )
  ),
  
  card(
    card_header("Dashboard Information"),
    card_body(
      p("This dashboard provides a visual representation of FDA Adverse Events Reporting System (FAERS) data. 
        It allows users to filter and analyze reports by various demographic and clinical categories."),
      p("Note: This is a simulated recreation and does not represent actual FDA data.")
    )
  )
)

# Create prediction tab UI 
prediction_tab <- page_fluid(
  card(
    card_header("Statistical Prediction Model"),
    card_body(
      p("This model predicts the probability of different adverse event types based on selected covariates."),
      p("The prediction uses a multinomial logistic regression to model the relationship between patient/report characteristics and event outcomes."),
      hr(),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("model_sex", "Sex:",
                    choices = c("Male", "Female", "Not Specified"),
                    selected = "Male"),
        selectInput("model_age", "Age Group:",
                    choices = c("0-1 Month", "2 Months-2 Years", "3-11 Years", "12-17 Years", 
                                "18-64 Years", "65-85 Years", "More than 85 Years", "Not Specified"),
                    selected = "18-64 Years"),
        selectInput("model_reporter", "Reporter:",
                    choices = c("Healthcare Professional", "Consumer", "Not Specified", "Other"),
                    selected = "Healthcare Professional")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("model_report_type", "Report Type:",
                    choices = c("Direct", "Expedited", "BSR", "Non-Expedited"),
                    selected = "Expedited"),
        selectInput("model_region", "Reporter Region:",
                    choices = c("Domestic", "Foreign", "Not Specified"),
                    selected = "Domestic"),
        selectInput("model_drug", "Drug Class:",
                    choices = c("Analgesics", "Antibiotics", "Antidepressants", "Antidiabetics", 
                                "Antihypertensives", "Anticoagulants", "Antipsychotics", 
                                "Antivirals", "Bronchodilators", "Corticosteroids"),
                    selected = "Antibiotics")
      ),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("model_covariates", "Include Predictors:", 
                    choices = c("Sex", "Age Group", "Reporter", "Report Type", "Region", "Drug Class"),
                    selected = c("Sex", "Age Group", "Reporter"),
                    multiple = TRUE),
        selectInput("model_outcome", "Predict Outcome:",
                    choices = c("Seriousness (all categories)", "Death vs Others", "Serious vs Non-Serious"),
                    selected = "Seriousness (all categories)")
      ),
      actionButton("run_model", "Run Model", class = "btn-primary", icon = icon("calculator"))
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    gap = "1rem",
    card(
      full_screen = TRUE,
      card_header("Predicted Probabilities"),
      card_body(
        h4(textOutput("prediction_header")),
        hr(),
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "Predicted Death Risk",
            value = textOutput("pred_death"),
            showcase = bsicons::bs_icon("exclamation-diamond"),
            theme = "danger"
          ),
          value_box(
            title = "Predicted Serious Event Risk",
            value = textOutput("pred_serious"),
            showcase = bsicons::bs_icon("exclamation-triangle"),
            theme = "warning" 
          ),
          value_box(
            title = "Predicted Non-Serious Event Risk",
            value = textOutput("pred_nonserious"),
            showcase = bsicons::bs_icon("check-circle"),
            theme = "success"
          )
        ),
        plotlyOutput("prediction_chart", height = "300px")
      ),
      height = "550px"
    ),
    card(
      full_screen = TRUE,
      card_header("Model Details"),
      card_body(
        tabsetPanel(
          tabPanel("Model Summary", 
                   div(style = "height: 450px; overflow-y: auto;", 
                       verbatimTextOutput("model_summary"))),
          tabPanel("Feature Importance", plotOutput("importance_plot", height = "450px")),
          tabPanel("Model Performance", plotOutput("performance_plot", height = "450px"))
        )
      ),
      height = "550px"
    )
  ),
  
  card(
    card_header("Model Information"),
    card_body(
      p("This prediction model estimates the probabilities of different adverse event severities based on patient and report characteristics."),
      p("The model uses multinomial logistic regression, a statistical approach for predicting categorical outcomes with multiple classes."),
      p("Note: This is a simplified model based on simulated data and should not be used for real clinical decisions.")
    )
  )
)


# UI with bslib and two tabs
ui <- page_navbar(
  title = span(
    img(src = "https://raw.githubusercontent.com/dgalgom/shiny-tiny-hackathon/main/FDA_Logo.jpg", height = "30px", style = "margin-right: 10px"),
    "FDA Adverse Events Reporting System (FAERS)"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean",
    primary = "#0073B1",
    secondary = "#5C6670",
    success = "#26A69A",
    info = "#42A5F5",
    warning = "#FFA726",
    danger = "#EF5350"
  ),
  
  # Dashboard Tab
  nav_panel(
    title = "Dashboard",
    icon = bsicons::bs_icon("speedometer2"),
    
    # Summary stats
    layout_columns(
      col_widths = c(4, 4, 4),
      gap = "1rem",
      value_box(
        title = "Total Reports",
        value = textOutput("total_reports"),
        showcase = bsicons::bs_icon("clipboard-data"),
        theme = "primary"
      ),
      value_box(
        title = "Serious Reports (excl. Death)",
        value = textOutput("serious_reports"),
        showcase = bsicons::bs_icon("exclamation-triangle"),
        theme = "warning"
      ),
      value_box(
        title = "Death Reports",
        value = textOutput("death_reports"),
        showcase = bsicons::bs_icon("heart-pulse"),
        theme = "danger"
      )
    ),
    
    # Filter section with multiple filters 
    card(
      card_header("Data Filters"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          radioButtons("year_range", "Time Period:",
                       choices = c("All Years", "Last 10 Years"),
                       selected = "All Years",
                       inline = TRUE),
          selectInput("chart_view", "Chart Visualization By:",
                      choices = c("Report Type", "Age", "Sex", "Reporter", "Seriousness", "Region"),
                      selected = "Report Type")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      gap = "1rem",
      card(
        full_screen = TRUE,
        card_header("Reports Breakdown by Year"),
        card_body(plotlyOutput("yearly_chart", height = "400px")),
        height = "500px"
      ),
      card(
        full_screen = TRUE,
        card_header("Data Table"),
        card_body(DTOutput("report_table")),
        height = "500px"
      )
    ),
    
    card(
      card_header("Dashboard Information"),
      card_body(
        p("This dashboard provides a visual representation of FDA Adverse Events Reporting System (FAERS) data. 
          It allows users to filter and analyze reports by various demographic and clinical categories."),
        p("Note: This is a simulated recreation and does not represent actual FDA data.")
      )
    )
  ),
  
  # Prediction Model Tab
  nav_panel(
    title = "Prediction Model",
    icon = bsicons::bs_icon("graph-up"),
    
    card(
      card_header("Statistical Prediction Model"),
      card_body(
        p("This model predicts the probability of different adverse event types based on selected covariates."),
        p("The prediction uses a multinomial logistic regression to model the relationship between patient/report characteristics and event outcomes."),
        hr(),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("model_sex", "Sex:",
                      choices = c("Male", "Female", "Not Specified"),
                      selected = "Male"),
          selectInput("model_age", "Age Group:",
                      choices = c("0-1 Month", "2 Months-2 Years", "3-11 Years", "12-17 Years", 
                                  "18-64 Years", "65-85 Years", "More than 85 Years", "Not Specified"),
                      selected = "18-64 Years"),
          selectInput("model_reporter", "Reporter:",
                      choices = c("Healthcare Professional", "Consumer", "Not Specified", "Other"),
                      selected = "Healthcare Professional")
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("model_report_type", "Report Type:",
                      choices = c("Direct", "Expedited", "BSR", "Non-Expedited"),
                      selected = "Expedited"),
          selectInput("model_region", "Reporter Region:",
                      choices = c("Domestic", "Foreign", "Not Specified"),
                      selected = "Domestic"),
          selectInput("model_drug", "Drug Class:",
                      choices = c("Analgesics", "Antibiotics", "Antidepressants", "Antidiabetics", 
                                  "Antihypertensives", "Anticoagulants", "Antipsychotics", 
                                  "Antivirals", "Bronchodilators", "Corticosteroids"),
                      selected = "Antibiotics")
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectInput("model_covariates", "Include Predictors:", 
                      choices = c("Sex", "Age Group", "Reporter", "Report Type", "Region", "Drug Class"),
                      selected = c("Sex", "Age Group", "Reporter"),
                      multiple = TRUE),
          selectInput("model_outcome", "Predict Outcome:",
                      choices = c("Seriousness (all categories)", "Death vs Others", "Serious vs Non-Serious"),
                      selected = "Seriousness (all categories)")
        ),
        br(),
        
        actionButton("run_model", "Run Model", class = "btn-primary", icon = icon("calculator"))
      ),
      height = "650px"
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      gap = "1rem",
      card(
        full_screen = TRUE,
        card_header("Predicted Probabilities"),
        card_body(
          h4(textOutput("prediction_header")),
          hr(),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Predicted Death Risk",
              value = textOutput("pred_death"),
              theme = "danger"
            ),
            value_box(
              title = "Predicted Serious Event Risk",
              value = textOutput("pred_serious"),
              theme = "warning" 
            ),
            value_box(
              title = "Predicted Non-Serious Event Risk",
              value = textOutput("pred_nonserious"),
              theme = "success"
            )
          ),
          plotlyOutput("prediction_chart", height = "300px")
        ),
        height = "650px"
      ),
      card(
        full_screen = TRUE,
        card_header("Model Details"),
        card_body(
          tabsetPanel(
            tabPanel("Model Summary", 
                     div(style = "height: 600px; overflow-y: auto;", 
                         verbatimTextOutput("model_summary"))),
            tabPanel("Feature Importance", plotOutput("importance_plot", height = "450px")),
            tabPanel("Model Performance", plotOutput("performance_plot", height = "450px"))
          )
        ),
        height = "650px"
      )
    ),
    
    card(
      card_header("Model Information"),
      card_body(
        p("This prediction model estimates the probabilities of different adverse event severities based on patient and report characteristics."),
        p("The model uses multinomial logistic regression, a statistical approach for predicting categorical outcomes with multiple classes."),
        p("Note: This is a simplified model based on simulated data and should not be used for real clinical decisions.")
      )
    )
  )
)


# server
server <- function(input, output, session) {
  # Load sample data
  all_faers_data <- reactive({
    load_faers_data()
  })
  
  # Apply filters dynamically
  faers_data <- reactive({
    data <- all_faers_data()
    
    # Apply year filter
    if (input$year_range == "Last 10 Years") {
      current_year <- max(data$year)
      data <- data %>% filter(year > (current_year - 10))
    }
    
    # Apply all the individual filters
    if (length(input$filter_report_type) > 0) {
      data <- data %>% filter(report_type %in% input$filter_report_type)
    }
    
    if (length(input$filter_reporter) > 0) {
      data <- data %>% filter(reporter %in% input$filter_reporter)
    }
    
    if (length(input$filter_region) > 0) {
      data <- data %>% filter(region %in% input$filter_region)
    }
    
    if (length(input$filter_seriousness) > 0) {
      data <- data %>% filter(seriousness %in% input$filter_seriousness)
    }
    
    if (length(input$filter_sex) > 0) {
      data <- data %>% filter(sex %in% input$filter_sex)
    }
    
    if (length(input$filter_age) > 0) {
      data <- data %>% filter(age_group %in% input$filter_age)
    }
    
    return(data)
  })
  
  # Summary statistics
  output$total_reports <- renderText({
    formatC(nrow(faers_data()), format = "d", big.mark = ",")
  })
  
  output$serious_reports <- renderText({
    serious_count <- faers_data() %>%
      filter(seriousness == "Serious") %>%
      nrow()
    formatC(serious_count, format = "d", big.mark = ",")
  })
  
  output$death_reports <- renderText({
    death_count <- faers_data() %>%
      filter(seriousness == "Death") %>%
      nrow()
    formatC(death_count, format = "d", big.mark = ",")
  })
  
  # Data table
  output$report_table <- renderDT({
    display_data <- faers_data() %>%
      select(year, quarter, report_type, age_group, sex, reporter, seriousness, region, drug_class) %>%
      arrange(desc(year), desc(quarter))
    
    datatable(
      display_data,
      options = list(
        pageLength = 8,
        searchHighlight = TRUE,
        ordering = TRUE,
        dom = '<"top"f>rt<"bottom"ip>',
        scrollX = TRUE,
        autoWidth = TRUE,
        class = 'cell-border stripe'
      ),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatStyle(
        'seriousness',
        backgroundColor = styleEqual(
          c('Death', 'Serious', 'Non-Serious'),
          c('#FFCDD2', '#FFECB3', '#E1F5FE')
        )
      )
  })
  
  # Yearly chart
  output$yearly_chart <- renderPlotly({
    # Prepare data for chart - group by year and selected filter type
    chart_column <- case_when(
      input$chart_view == "Report Type" ~ "report_type",
      input$chart_view == "Age" ~ "age_group",
      input$chart_view == "Sex" ~ "sex",
      input$chart_view == "Reporter" ~ "reporter",
      input$chart_view == "Seriousness" ~ "seriousness",
      input$chart_view == "Region" ~ "region",
      TRUE ~ "report_type"  # Default
    )
    
    chart_data <- faers_data() %>%
      group_by(year, .data[[chart_column]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(year)
    
    # Select color palette based on filter type
    color_palette <- switch(input$chart_view,
                            "Report Type" = colorRampPalette(brewer.pal(8, "Blues"))(length(unique(chart_data[[chart_column]]))),
                            "Age" = colorRampPalette(brewer.pal(8, "YlOrRd"))(length(unique(chart_data[[chart_column]]))),
                            "Sex" = colorRampPalette(brewer.pal(8, "Set1"))(length(unique(chart_data[[chart_column]]))),
                            "Reporter" = colorRampPalette(brewer.pal(8, "Greens"))(length(unique(chart_data[[chart_column]]))),
                            "Seriousness" = colorRampPalette(brewer.pal(8, "RdPu"))(length(unique(chart_data[[chart_column]]))),
                            "Region" = colorRampPalette(brewer.pal(8, "Purples"))(length(unique(chart_data[[chart_column]]))),
                            colorRampPalette(brewer.pal(9, "Blues"))(length(unique(chart_data[[chart_column]])))  # Default
    )
    
    # For certain categories with few values, use a fixed color set that's more distinct
    if (input$chart_view == "Sex" || input$chart_view == "Seriousness" || input$chart_view == "Region") {
      distinct_palettes <- list(
        "Sex" = c("#4575B4", "#D73027", "#A9A9A9"),  # Blue, Red, Gray
        "Seriousness" = c("#D73027", "#FEE090", "#91BFDB"),  # Red, Yellow, Blue
        "Region" = c("#1B9E77", "#7570B3", "#A9A9A9")  # Green, Purple, Gray
      )
      
      # Use a distinct palette if available
      if (length(unique(chart_data[[chart_column]])) <= length(distinct_palettes[[input$chart_view]])) {
        color_palette <- distinct_palettes[[input$chart_view]][1:length(unique(chart_data[[chart_column]]))]
      }
    }
    
    p <- plot_ly(chart_data, x = ~year, y = ~count, type = "bar", color = ~.data[[chart_column]],
                 colors = color_palette) %>%
      layout(
        barmode = "stack",
        title = "",
        xaxis = list(title = "Year", tickmode = "array", tickvals = sort(unique(chart_data$year))),
        yaxis = list(title = "Number of Reports"),
        legend = list(title = list(text = input$chart_view)),
        hovermode = "compare",
        margin = list(l = 50, r = 20, b = 50, t = 30),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    return(p)
  })
  
  # PREDICTION MODEL SERVER CODE
  
  # Reactive value to store model results
  model_results <- reactiveVal(NULL)
  
  # Train the model when the user clicks the run button
  observeEvent(input$run_model, {
    # Get data
    data <- all_faers_data()
    
    # Prepare formula based on selected covariates
    selected_covariates <- input$model_covariates
    
    # Map UI selections to data frame column names
    covariate_mapping <- c(
      "Sex" = "sex",
      "Age Group" = "age_group",
      "Reporter" = "reporter",
      "Report Type" = "report_type",
      "Region" = "region",
      "Drug Class" = "drug_class"
    )
    
    # Convert selected UI names to actual column names
    selected_cols <- covariate_mapping[selected_covariates]
    
    # Create formula based on outcome type
    if (input$model_outcome == "Seriousness (all categories)") {
      # Multinomial model for all three categories
      formula_str <- paste("seriousness ~", paste(selected_cols, collapse = " + "))
      model_type <- "multinomial"
      
      # Train the model with selected covariates
      model <- multinom(as.formula(formula_str), data = data, trace = FALSE)
      
      # Make prediction for the specific case
      newdata <- data.frame(
        sex = input$model_sex,
        age_group = input$model_age,
        reporter = input$model_reporter,
        report_type = input$model_report_type,
        region = input$model_region,
        drug_class = input$model_drug
      )
      
      # Get predicted probabilities
      pred_probs <- predict(model, newdata = newdata, type = "probs")
      
      # Extract model coefficients for feature importance
      coefs <- coef(model)
      
      # Store results
      model_results(list(
        model = model,
        predictions = pred_probs,
        model_type = model_type,
        coefs = coefs,
        newdata = newdata
      ))
      
    } else if (input$model_outcome == "Death vs Others") {
      # Binary model for Death vs all others
      data$is_death <- ifelse(data$seriousness == "Death", 1, 0)
      formula_str <- paste("is_death ~", paste(selected_cols, collapse = " + "))
      model_type <- "binary_death"
      
      # Train logistic regression
      model <- glm(as.formula(formula_str), data = data, family = binomial)
      
      # Make prediction for the specific case
      newdata <- data.frame(
        sex = input$model_sex,
        age_group = input$model_age,
        reporter = input$model_reporter,
        report_type = input$model_report_type,
        region = input$model_region,
        drug_class = input$model_drug
      )
      
      # Get predicted probabilities
      pred_probs <- predict(model, newdata = newdata, type = "response")
      
      # Extract model coefficients for feature importance
      coefs <- coef(model)
      
      # Store results
      model_results(list(
        model = model,
        predictions = c(1-pred_probs, pred_probs, 0), # Death, Non-death (split into serious/non-serious as 0)
        model_type = model_type,
        coefs = coefs,
        newdata = newdata
      ))
      
    } else {  # "Serious vs Non-Serious"
      # Binary model for Serious vs Non-serious (excluding deaths)
      data_subset <- data %>% filter(seriousness != "Death")
      data_subset$is_serious <- ifelse(data_subset$seriousness == "Serious", 1, 0)
      
      formula_str <- paste("is_serious ~", paste(selected_cols, collapse = " + "))
      model_type <- "binary_serious"
      
      # Train logistic regression
      model <- glm(as.formula(formula_str), data = data_subset, family = binomial)
      
      # Make prediction for the specific case
      newdata <- data.frame(
        sex = input$model_sex,
        age_group = input$model_age,
        reporter = input$model_reporter,
        report_type = input$model_report_type,
        region = input$model_region,
        drug_class = input$model_drug
      )
      
      # Get predicted probabilities
      pred_probs <- predict(model, newdata = newdata, type = "response")
      
      # Extract model coefficients for feature importance
      coefs <- coef(model)
      
      # Store results
      model_results(list(
        model = model,
        predictions = c(0, pred_probs, 1-pred_probs), # Death (0), Serious, Non-serious
        model_type = model_type,
        coefs = coefs,
        newdata = newdata
      ))
    }
  })
  
  # Display prediction header
  output$prediction_header <- renderText({
    if (is.null(model_results())) {
      return("Run the model to see predictions")
    }
    
    paste("Prediction for", input$model_sex, "patient,", input$model_age, "using", input$model_drug)
  })
  
  # Display prediction probabilities
  output$pred_death <- renderText({
    if (is.null(model_results())) {
      return("N/A")
    }
    
    result <- model_results()
    
    if (result$model_type == "multinomial") {
      probs <- result$predictions[1]
      return(paste0(round(probs * 100, 1), "%"))
    } else if (result$model_type == "binary_death") {
      probs <- result$predictions[2]
      return(paste0(round(probs * 100, 1), "%"))
    } else {
      return("0%") # For serious vs non-serious model, death prediction is 0
    }
  })
  
  output$pred_serious <- renderText({
    if (is.null(model_results())) {
      return("N/A")
    }
    
    result <- model_results()
    
    if (result$model_type == "multinomial") {
      probs <- result$predictions[2]
      return(paste0(round(probs * 100, 1), "%"))
    } else if (result$model_type == "binary_death") {
      return("N/A") # Death vs others doesn't predict serious directly
    } else {
      probs <- result$predictions[2]
      return(paste0(round(probs * 100, 1), "%"))
    }
  })
  
  output$pred_nonserious <- renderText({
    if (is.null(model_results())) {
      return("N/A")
    }
    
    result <- model_results()
    
    if (result$model_type == "multinomial") {
      probs <- result$predictions[3]
      return(paste0(round(probs * 100, 1), "%"))
    } else if (result$model_type == "binary_death") {
      return("N/A") # Death vs others doesn't predict non-serious directly
    } else {
      probs <- result$predictions[3]
      return(paste0(round(probs * 100, 1), "%"))
    }
  })
  
  # Prediction chart
  output$prediction_chart <- renderPlotly({
    if (is.null(model_results())) {
      return(NULL)
    }
    
    result <- model_results()
    
    # Set up data for plotting
    if (result$model_type == "multinomial") {
      plot_data <- data.frame(
        category = c("Death", "Serious", "Non-Serious"),
        probability = as.numeric(result$predictions)
      )
    } else if (result$model_type == "binary_death") {
      plot_data <- data.frame(
        category = c("Death", "Not Death"),
        probability = c(result$predictions[2], result$predictions[1])
      )
    } else {
      plot_data <- data.frame(
        category = c("Serious", "Non-Serious"),
        probability = c(result$predictions[2], result$predictions[3])
      )
    }
    
    # Set colors based on categories
    colors <- c("Death" = "#EF5350", "Serious" = "#FFA726", "Non-Serious" = "#26A69A",
                "Not Death" = "#42A5F5")
    
    # Create the plot
    p <- plot_ly(plot_data, x = ~category, y = ~probability, type = "bar",
                 marker = list(color = colors[plot_data$category])) %>%
      layout(
        yaxis = list(title = "Probability", tickformat = ".0%", range = c(0, 1)),
        xaxis = list(title = ""),
        margin = list(l = 50, r = 20, b = 50, t = 30)
      )
    
    return(p)
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    if (is.null(model_results())) {
      return("Run model to see results")
    }
    
    result <- model_results()
    summary(result$model)
  })
  
  # Feature importance plot
  output$importance_plot <- renderPlot({
    if (is.null(model_results())) {
      return(NULL)
    }
    
    result <- model_results()
    
    # Extract feature importance
    if (result$model_type == "multinomial") {
      # For multinomial, take absolute values of coefficients for each outcome
      coefs_df <- data.frame(result$coefs)
      importance <- data.frame(
        feature = colnames(coefs_df)[-1],  # Exclude intercept
        importance = apply(coefs_df[, -1], 2, function(x) mean(abs(x)))
      )
    } else {
      # For binary models, use absolute coefficient values
      coefs <- result$coefs[-1]  # Exclude intercept
      importance <- data.frame(
        feature = names(coefs),
        importance = abs(coefs)
      )
    }
    
    # Format feature names for display
    importance$feature <- gsub("^.*\\.", "", importance$feature)
    
    # Create the importance plot
    ggplot(importance, aes(x = reorder(feature, importance), y = importance)) +
      geom_col(fill = "#0073B1") +
      coord_flip() +
      labs(x = "", y = "Relative Importance") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        panel.grid.major.y = element_blank()
      )
  })
  
  # Performance plot (simplified for demo - in real app would use cross-validation)
  output$performance_plot <- renderPlot({
    if (is.null(model_results())) {
      return(NULL)
    }
    
    result <- model_results()
    model_type <- result$model_type
    
    # For demo purpose, create a confusion matrix visualization
    # In a real app, use cross-validation metrics
    
    # Create a simulated confusion matrix
    if (model_type == "multinomial") {
      conf_mat <- matrix(c(
        80, 15, 5,
        20, 65, 15,
        5, 25, 70
      ), nrow = 3, byrow = TRUE)
      
      rownames(conf_mat) <- c("Death", "Serious", "Non-Serious")
      colnames(conf_mat) <- c("Death", "Serious", "Non-Serious")
      
      # Convert to data frame for plotting
      conf_df <- as.data.frame(as.table(conf_mat))
      names(conf_df) <- c("Actual", "Predicted", "Count")
      
      # Calculate percentages
      row_sums <- rowSums(conf_mat)
      conf_df$Percent <- conf_df$Count / rep(row_sums, each = 3) * 100
      conf_df$Percent_label <- sprintf("%.1f%%", conf_df$Percent)
      
      # Create heatmap
      ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Percent)) +
        geom_tile() +
        geom_text(aes(label = Percent_label), color = "white", size = 5) +
        scale_fill_gradient(low = "#42A5F5", high = "#0D47A1") +
        labs(title = "Confusion Matrix (Simulated)", x = "Predicted", y = "Actual") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold")
        )
      
    } else if (model_type == "binary_death") {
      # For binary death model
      conf_mat <- matrix(c(
        85, 15,
        20, 80
      ), nrow = 2, byrow = TRUE)
      
      rownames(conf_mat) <- c("Not Death", "Death")
      colnames(conf_mat) <- c("Not Death", "Death")
      
      # Convert to data frame for plotting
      conf_df <- as.data.frame(as.table(conf_mat))
      names(conf_df) <- c("Actual", "Predicted", "Count")
      
      # Calculate percentages
      row_sums <- rowSums(conf_mat)
      conf_df$Percent <- conf_df$Count / rep(row_sums, each = 2) * 100
      conf_df$Percent_label <- sprintf("%.1f%%", conf_df$Percent)
      
      # Create heatmap
      ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Percent)) +
        geom_tile() +
        geom_text(aes(label = Percent_label), color = "white", size = 5) +
        scale_fill_gradient(low = "#EF9A9A", high = "#B71C1C") +
        labs(title = "Confusion Matrix (Simulated)", x = "Predicted", y = "Actual") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold")
        )
      
    } else {
      # For serious vs non-serious model
      conf_mat <- matrix(c(
        75, 25,
        30, 70
      ), nrow = 2, byrow = TRUE)
      
      rownames(conf_mat) <- c("Non-Serious", "Serious")
      colnames(conf_mat) <- c("Non-Serious", "Serious")
      
      # Convert to data frame for plotting
      conf_df <- as.data.frame(as.table(conf_mat))
      names(conf_df) <- c("Actual", "Predicted", "Count")
      
      # Calculate percentages
      row_sums <- rowSums(conf_mat)
      conf_df$Percent <- conf_df$Count / rep(row_sums, each = 2) * 100
      conf_df$Percent_label <- sprintf("%.1f%%", conf_df$Percent)
      
      # Create heatmap
      ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Percent)) +
        geom_tile() +
        geom_text(aes(label = Percent_label), color = "white", size = 5) +
        scale_fill_gradient(low = "#FFE082", high = "#FF8F00") +
        labs(title = "Confusion Matrix (Simulated)", x = "Predicted", y = "Actual") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold")
        )
    }
  })
}

shinyApp(ui, server)
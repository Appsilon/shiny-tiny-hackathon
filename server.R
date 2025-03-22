# server.R
library(shiny)
library(shinydashboard)
library(highcharter)
library(reactable)
library(dplyr)
library(htmltools)
library(DT)
library(plotly)
source("R/load_data.R")
source("R/disclaimer.R")

# Define server logic
server <- function(input, output, session) {

  # Initialize modules using moduleServer instead of callModule
  mod_disclaimer_server("disclaimer")

  # Function to show modals dynamically
  show_modal <- function(title, content) {
    showModal(
      modalDialog(
        title = title,
        HTML(content),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }

  # Home Modal: Fun message when clicking "Home"
  observeEvent(input$home_link, {
    show_modal("Welcome Home!", 
               "<p>You're already home! Why go anywhere else?</p> 
                <p><em>\"There’s no place like home!\"</em> - Dorothy, The Wizard of Oz</p>")
  })
  
  # Search Modal: Inspirational search quote
  observeEvent(input$search_link, {
    show_modal("Searching...", 
               "<p>\"Ah, my friend, the true search is not out there... it is within!\"</p> 
                <p><strong>- Master Oogway, Kung Fu Panda</strong></p>")
  })
  
  # Handle FAQ Click
  observeEvent(input$faq_link, {
    show_modal("Frequently Asked Questions",
               "<p>The FAERS public dashboard is a new, user-friendly and interactive web-based tool that was created to give the public the ability to query the FDA FAERS database and improve transparency.</p>")
  })
  
  # Handle Disclaimer Click
  observeEvent(input$disclaimer_link, {
    show_modal("Disclaimer",
               "<p>This tool provides access to FDA Adverse Event Reporting System (FAERS) data. The presence of an event in FAERS data does not indicate causation. Consult healthcare professionals before making any conclusions.</p>")
  })
  
  # Handle Site Feedback Click
  observeEvent(input$site_feedback_link, {
    show_modal("Site Feedback",
               "<p>We value your feedback! Please let us know if you have suggestions or encounter any issues using this dashboard.</p>")
  })
  
  # Handle Report a Problem Click
  observeEvent(input$report_problem_link, {
    show_modal("Report a Problem",
               "<p>If you encounter any issues with the dashboard, please describe the problem in detail. Our team will review and address it as soon as possible.</p>")
  })
  
  # Load data using module
  faers_data <- callModule(mod_load_data, "data")
  
  # Filter data based on inputs
  filtered_data <- reactive({
    data <- faers_data()
    
    # Apply filter by report type if selected
    if (!is.null(input$report_type_filter) && input$report_type_filter != "All") {
      data <- data %>% filter(Report_Type == input$report_type_filter)
    }
    
    # Apply year filter if provided
    if (!is.null(input$year_filter) && input$year_filter != "") {
      data <- data %>% filter(grepl(input$year_filter, Year))
    }
    
    # Apply report type search if provided
    if (!is.null(input$report_type_search) && input$report_type_search != "") {
      data <- data %>% filter(grepl(input$report_type_search, Report_Type, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  # Update report type filter options once data is loaded
  observe({
    req(faers_data())
    report_types <- c("All", unique(faers_data()$Report_Type))
    updateSelectInput(session, "report_type_filter", choices = report_types)
  })
  
  # Last 10 years filter
  observeEvent(input$last_10_years, {
    req(faers_data())
    max_year <- max(faers_data()$Year)
    years_to_keep <- (max_year - 9):max_year
    updateSelectInput(session, "report_type_filter", selected = "All")
    # Additional filtering will be handled by filtered_data reactive
  })
  
  # All years filter
  observeEvent(input$all_years, {
    updateSelectInput(session, "report_type_filter", selected = "All")
    # Reset any other filters as needed
  })
  
  # KPI Outputs
  output$total_reports_text <- renderText({
    format(sum(faers_data()$Report_Count, na.rm = TRUE), big.mark = ",")
  })
  
  output$serious_reports_text <- renderText({
    # Assuming "Serious" corresponds to Expedited + Non-Expedited in the FDA dashboard
    serious_data <- faers_data() %>% 
      filter(Report_Type %in% c("Expedited", "Non-Expedited"))
    format(sum(serious_data$Report_Count, na.rm = TRUE), big.mark = ",")
  })
  
  output$death_reports_text <- renderText({
    # Assuming "Death" corresponds to BSR in the FDA dashboard
    death_data <- faers_data() %>% 
      filter(Report_Type == "BSR")
    format(sum(death_data$Report_Count, na.rm = TRUE), big.mark = ",")
  })
  
  # Data Table Output
  output$reports_table <- renderDT({
    # Group data by year and report type
    summary_data <- filtered_data() %>%
      # Convert Year to character first
      mutate(Year = as.character(Year)) %>%
      group_by(Year) %>%
      summarize(
        `Total Reports` = sum(Report_Count),
        Expedited = sum(Report_Count[Report_Type == "Expedited"]),
        `Non-Expedited` = sum(Report_Count[Report_Type == "Non-Expedited"]),
        Direct = sum(Report_Count[Report_Type == "Direct"]),
        BSR = sum(Report_Count[Report_Type == "BSR"])
      ) %>%
      arrange(desc(Year))
    
    # Create total_row with the same column structure as summary_data
    total_row <- summary_data %>%
      summarize(
        Year = "Total Reports",
        `Total Reports` = sum(`Total Reports`),
        Expedited = sum(Expedited),
        `Non-Expedited` = sum(`Non-Expedited`),
        Direct = sum(Direct),
        BSR = sum(BSR)
      )
    
    # Use bind_rows for safer data frame combining
    summary_data <- bind_rows(total_row, summary_data)
    
    datatable(summary_data,
              options = list(
                pageLength = 10,
                dom = 't',
                ordering = FALSE
              ),
              rownames = FALSE) %>%
      formatStyle(
        'Year',
        target = 'row',
        backgroundColor = styleEqual(c("Total Reports"), c('#f5f5f5'))
      ) %>%
      formatCurrency(
        columns = 2:6,
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      )
  })
  
  # Plot Output with scrollable bar chart
  output$reports_plot <- renderPlotly({
    # Prepare data for stacked bar chart
    plot_data <- filtered_data() %>%
      group_by(Year, Report_Type) %>%
      summarize(Report_Count = sum(Report_Count), .groups = 'drop')
    
    # Define colors for report types
    report_colors <- c("BSR" = "#337ab7", "Direct" = "#5bc0de", 
                      "Expedited" = "#d9534f", "Non-Expedited" = "#5cb85c")
    
    # Create stacked bar chart
    p <- ggplot(plot_data, aes(x = as.factor(Year), y = Report_Count, fill = Report_Type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = report_colors) +
      labs(x = "", y = "Report Count") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      ) +
      scale_y_continuous(labels = comma)
    
    # Convert to plotly and add scrolling capability
    ggplotly(p) %>%
      layout(
        legend = list(orientation = "h", y = 1.1),
        xaxis = list(
          rangeslider = list(visible = TRUE),
          # Set a fixed range to show a certain number of bars at a time
          range = c(0, 10)  # Show first 10 years by default
        ),
        updatemenus = list(
          list(
            type = "buttons",
            direction = "right",
            x = 0.1,
            y = 1.1,
            buttons = list(
              list(
                method = "relayout",
                args = list(list(xaxis.range = c(0, 10))),
                label = "First 10 Years"
              ),
              list(
                method = "relayout", 
                args = list(list(xaxis.range = c(10, 20))),
                label = "Next 10 Years"
              ),
              list(
                method = "relayout",
                args = list(list(xaxis.autorange = TRUE)),
                label = "All Years"
              )
            )
          )
        )
      ) %>%
      config(scrollZoom = TRUE)  # Enable scroll zoom functionality
  })
  
  # FAQ & Disclaimer Outputs
  output$faq_content <- renderUI({ mod_faq_ui("faq") })
  output$disclaimer_content <- renderUI({ mod_disclaimer_ui("disclaimer") })
}

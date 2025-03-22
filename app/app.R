library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)
library(lubridate)
library(scales)
library(tidyr)
library(writexl)
library(rmarkdown)
library(knitr)


# UI modules for each tab
##################################### Dashboard Overview UI
dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    # First row - Value boxes only
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        title = "Total Reports",
        value = textOutput(ns("total_reports")),
        showcase = bsicons::bs_icon("bar-chart-fill"),
        theme = value_box_theme(bg = "#4E73DF", fg = "white")
      ),
      value_box(
        title = "Serious Reports",
        value = textOutput(ns("serious_reports")),
        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
        theme = value_box_theme(bg = "#F6C23E", fg = "white")
      ),
      value_box(
        title = "Deaths",
        value = textOutput(ns("death_reports")),
        showcase = bsicons::bs_icon("heart-fill"),
        theme = value_box_theme(bg = "#E74A3B", fg = "white")
      ),
      value_box(
        title = "Hospitalizations",
        value = textOutput(ns("hosp_reports")),
        showcase = bsicons::bs_icon("hospital-fill"),
        theme = value_box_theme(bg = "#36B9CC", fg = "white")
      )
    ),
    
    # Second row - Charts
    layout_columns(
      col_widths = c(8, 4),
      fill = FALSE,
      card(
        card_header("Reports Over Time"),
        plotlyOutput(ns("time_trend_plot"), height = "300px")
      ),
      card(
        card_header("Top Reactions"),
        plotlyOutput(ns("top_reactions_plot"), height = "300px")
      )
    ),
    
    # Third row - More charts
    layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      card(
        card_header("Age Distribution"),
        plotlyOutput(ns("age_distribution"), height = "300px")
      ),
      card(
        card_header("Top Drugs"),
        plotlyOutput(ns("top_drugs"), height = "300px")
      ),
      card(
        card_header("Outcome Distribution"),
        plotlyOutput(ns("outcome_distribution"), height = "300px")
      )
    )
  )
}

##################################### Reports Over Time UI
reportsOverTimeUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      card(
        card_header("Reports by Year"),
        plotlyOutput(ns("yearly_trend"), height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Reports by Quarter"),
          plotlyOutput(ns("quarterly_trend"), height = "350px")
        ),
        card(
          card_header("Reports by Month"),
          plotlyOutput(ns("monthly_trend"), height = "350px")
        )
      )
    )
  )
}

##################################### Demographics UI
demographicsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Gender Distribution"),
          plotlyOutput(ns("gender_distribution"), height = "350px")
        ),
        card(
          card_header("Age Distribution"),
          plotlyOutput(ns("age_distribution"), height = "350px")
        )
      ),
      card(
        card_header("Reports by Country"),
        plotlyOutput(ns("country_distribution"), height = "350px")
      )
    )
  )
}

##################################### Drug Analysis UI
drugAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top Drugs by Reports"),
          plotlyOutput(ns("top_drugs_detailed"), height = "350px")
        ),
        card(
          card_header("Drug Indication Distribution"),
          plotlyOutput(ns("drug_indication"), height = "350px")
        )
      ),
      card(
        card_header("Drug-Reaction Heatmap"),
        plotlyOutput(ns("drug_reaction_heatmap"), height = "450px")
      )
    )
  )
}

##################################### Individual Reports UI
individualReportsUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Individual Case Reports",
        div(
          class = "float-end",
          downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm btn-outline-primary me-1"),
          downloadButton(ns("download_excel"), "Download Excel", class = "btn-sm btn-outline-success")
        )
      ),
      DTOutput(ns("case_reports_table"))
    )
  )
}


##################################### Custom Report UI
customReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 8),
      # Left column - configuration options
      card(
        card_header("Report Configuration"),
        card_body(
          h4("Select elements to include in your report:"),
          checkboxInput(ns("include_summary"), "Include Summary Statistics", value = TRUE),
          
          h5("Time Trends:"),
          checkboxGroupInput(ns("time_trends"), "", 
                             choices = c("Reports Over Time" = "time_trend_plot",
                                         "Yearly Trend" = "yearly_trend", 
                                         "Quarterly Trend" = "quarterly_trend",
                                         "Monthly Trend" = "monthly_trend"),
                             selected = "time_trend_plot"),
          
          h5("Demographics:"),
          checkboxGroupInput(ns("demographics"), "", 
                             choices = c("Age Distribution" = "age_distribution",
                                         "Gender Distribution" = "gender_distribution",
                                         "Country Distribution" = "country_distribution"),
                             selected = "age_distribution"),
          
          h5("Drug Analysis:"),
          checkboxGroupInput(ns("drug_analysis"), "", 
                             choices = c("Top Drugs" = "top_drugs",
                                         "Top Reactions" = "top_reactions_plot",
                                         "Drug Indications" = "drug_indication",
                                         "Drug-Reaction Heatmap" = "drug_reaction_heatmap"),
                             selected = "top_drugs"),
          
          h5("Report Options:"),
          textInput(ns("report_title"), "Report Title", value = "FAERS Custom Report"),
          textInput(ns("report_subtitle"), "Subtitle", value = paste("Generated on", format(Sys.Date(), "%B %d, %Y"))),
          textAreaInput(ns("report_notes"), "Additional Notes", rows = 3,
                        placeholder = "Add any additional notes or context for this report..."),
          br(),
          downloadButton(ns("generate_report"), "Generate PDF Report", class = "btn-primary btn-lg w-100")
        )
      ),
      
      # Right column - preview
      card(
        card_header("Report Preview"),
        card_body(
          h4(textOutput(ns("preview_title"))),
          h5(textOutput(ns("preview_subtitle"))),
          hr(),
          h5("This report will include:"),
          uiOutput(ns("preview_contents")),
          hr(),
          div(
            class = "alert alert-info",
            icon("info-circle"), 
            "The final PDF will contain the plots based on your current filter selections. 
            Make sure to set appropriate filters before generating the report."
          )
        )
      )
    )
  )
}



##################################### About UI
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("About FAERS Dashboard"),
      card_body(
        h3("FDA Adverse Event Reporting System (FAERS) Dashboard"),
        p("This dashboard is a mock representation of the FDA Adverse Event Reporting System (FAERS) 
          Public Dashboard. It's designed to display data about adverse events reported to the FDA 
          for drugs and biological products."),
        p("FAERS contains information on adverse event and medication error reports submitted to FDA. 
          The database is designed to support the FDA's post-marketing safety surveillance program 
          for drug and therapeutic biologic products."),
        p("Note: This is a mock dashboard created for Appsilon's Tiny Shiny AI Hackathon on March 22, 2025. 
          The data shown here is not real FAERS data but simulated for demonstration purposes."),
        h4("Features of this Dashboard:"),
        tags$ul(
          tags$li("Interactive visualizations of adverse event reports"),
          tags$li("Filtering capabilities by drug, reaction, time period, and more"),
          tags$li("Demographic analysis of reported cases"),
          tags$li("Detailed drug analysis and drug-reaction correlations"),
          tags$li("Access to individual case report details")
        ),
        h4("Technologies Used:"),
        tags$ul(
          tags$li("R Shiny for the interactive web application"),
          tags$li("bslib for the modern UI components"),
          tags$li("Plotly for interactive visualizations"),
          tags$li("DT for interactive tables"),
          tags$li("AI assistance for development (Anthropic Claude)")
        )
      )
    )
  )
}

##################################### Filter Sidebar UI
filterSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      accordion_panel(
        "Filters",
        dateRangeInput(
          ns("date_range"), 
          "Date Range:",
          start = "2020-01-01",
          end = Sys.Date()
        ),
        selectizeInput(
          ns("drug_filter"), 
          "Select Drug:", 
          choices = c("All", "Aspirin", "Lisinopril", "Atorvastatin"), 
          selected = "All", 
          multiple = TRUE
        ),
        selectizeInput(
          ns("reaction_filter"), 
          "Select Reaction:", 
          choices = c("All", "Headache", "Nausea", "Dizziness"), 
          selected = "All", 
          multiple = TRUE
        ),
        selectInput(
          ns("seriousness_filter"), 
          "Seriousness:", 
          choices = c("All", "Serious", "Non-serious"), 
          selected = "All"
        ),
        sliderInput(
          ns("age_range"), 
          "Age Range:", 
          min = 0, 
          max = 100, 
          value = c(0, 100), 
          step = 1
        )
      )
    )
  )
}



################################################# Server modules
########################################## Dashboard Overview server
dashboardServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Value boxes
    output$total_reports <- renderText({
      format(nrow(filtered_data()), big.mark = ",")
    })
    
    output$serious_reports <- renderText({
      serious_count <- filtered_data() %>%
        filter(seriousness == "Serious") %>%
        nrow()
      format(serious_count, big.mark = ",")
    })
    
    output$death_reports <- renderText({
      death_count <- filtered_data() %>%
        filter(outcome == "Death") %>%
        nrow()
      format(death_count, big.mark = ",")
    })
    
    output$hosp_reports <- renderText({
      hosp_count <- filtered_data() %>%
        filter(outcome == "Hospitalization") %>%
        nrow()
      format(hosp_count, big.mark = ",")
    })
    
    # Time trend plot
    output$time_trend_plot <- renderPlotly({
      time_data <- filtered_data() %>%
        count(date_received) %>%
        arrange(date_received)
      
      plot_ly(time_data, x = ~date_received, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#4E73DF')) %>%
        layout(title = "Reports Over Time",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Top reactions plot
    output$top_reactions_plot <- renderPlotly({
      top_reactions <- filtered_data() %>%
        count(reaction, sort = TRUE) %>%
        top_n(10, n) %>%
        mutate(reaction = reorder(reaction, n))
      
      plot_ly(top_reactions, x = ~n, y = ~reaction, type = 'bar', orientation = 'h',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Top 10 Adverse Events",
               xaxis = list(title = "Number of Reports"),
               yaxis = list(title = ""),
               margin = list(l = 120, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Age distribution plot
    output$age_distribution <- renderPlotly({
      age_data <- filtered_data() %>%
        filter(!is.na(patient_age)) %>%
        mutate(age_group = cut(patient_age, breaks = seq(0, 100, by = 10),
                               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                          "50-59", "60-69", "70-79", "80-89", "90+"))) %>%
        count(age_group)
      
      plot_ly(age_data, x = ~age_group, y = ~n, type = 'bar',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Age Distribution",
               xaxis = list(title = "Age Group"),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Top drugs plot
    output$top_drugs <- renderPlotly({
      top_drugs <- filtered_data() %>%
        count(drug_name, sort = TRUE) %>%
        top_n(10, n) %>%
        mutate(drug_name = reorder(drug_name, n))
      
      plot_ly(top_drugs, x = ~n, y = ~drug_name, type = 'bar', orientation = 'h',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Top 10 Drugs",
               xaxis = list(title = "Number of Reports"),
               yaxis = list(title = ""),
               margin = list(l = 120, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Outcome distribution plot
    output$outcome_distribution <- renderPlotly({
      outcome_data <- filtered_data() %>%
        count(outcome, sort = TRUE) %>%
        mutate(outcome = reorder(outcome, n))
      
      plot_ly(outcome_data, x = ~n, y = ~outcome, type = 'bar', orientation = 'h',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Outcome Distribution",
               xaxis = list(title = "Number of Reports"),
               yaxis = list(title = ""),
               margin = list(l = 120, r = 50, b = 50, t = 80, pad = 4))
    })
  })
}

########################################## Reports Over Time server
reportsOverTimeServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Yearly trend plot
    output$yearly_trend <- renderPlotly({
      yearly_data <- filtered_data() %>%
        count(year) %>%
        arrange(year)
      
      plot_ly(yearly_data, x = ~year, y = ~n, type = 'bar',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Reports by Year",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Quarterly trend plot
    output$quarterly_trend <- renderPlotly({
      quarterly_data <- filtered_data() %>%
        count(year, quarter) %>%
        arrange(year, quarter) %>%
        mutate(quarter_label = paste0(year, " Q", quarter))
      
      plot_ly(quarterly_data, x = ~quarter_label, y = ~n, type = 'bar',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Reports by Quarter",
               xaxis = list(title = "Quarter"),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Monthly trend plot
    output$monthly_trend <- renderPlotly({
      monthly_data <- filtered_data() %>%
        count(year, month) %>%
        arrange(year, month) %>%
        mutate(month_label = paste0(year, "-", sprintf("%02d", month)))
      
      plot_ly(monthly_data, x = ~month_label, y = ~n, type = 'bar',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Reports by Month",
               xaxis = list(title = "Month", tickangle = 45),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 100, t = 80, pad = 4))
    })
  })
}

########################################## Demographics server
demographicsServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Gender plot
    output$gender_distribution <- renderPlotly({
      gender_data <- filtered_data() %>%
        count(patient_sex) %>%
        mutate(percentage = n / sum(n) * 100)
      
      colors <- c('#4E73DF', '#E74A3B', '#858796')
      
      plot_ly(gender_data, labels = ~patient_sex, values = ~n, type = 'pie',
              marker = list(colors = colors),
              textinfo = 'label+percent', 
              insidetextorientation = 'radial') %>%
        layout(title = "Gender Distribution",
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Age distribution
    output$age_distribution <- renderPlotly({
      age_data <- filtered_data() %>%
        filter(!is.na(patient_age))
      
      plot_ly(x = ~age_data$patient_age, type = "histogram", 
              marker = list(color = '#4E73DF', line = list(color = "white", width = 0.5))) %>%
        layout(title = "Age Distribution",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Count"),
               bargap = 0.1,
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Country distribution
    output$country_distribution <- renderPlotly({
      country_data <- filtered_data() %>%
        count(country, sort = TRUE) %>%
        mutate(country = reorder(country, n))
      
      plot_ly(country_data, x = ~country, y = ~n, type = 'bar',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Reports by Country",
               xaxis = list(title = "Country"),
               yaxis = list(title = "Number of Reports"),
               margin = list(l = 50, r = 50, b = 50, t = 80, pad = 4))
    })
  })
}

########################################## Drug Analysis server
drugAnalysisServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Top drugs detailed plot
    output$top_drugs_detailed <- renderPlotly({
      top_drugs <- filtered_data() %>%
        count(drug_name, sort = TRUE) %>%
        top_n(15, n) %>%
        mutate(drug_name = reorder(drug_name, n))
      
      plot_ly(top_drugs, x = ~n, y = ~drug_name, type = 'bar', orientation = 'h',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Top 15 Drugs by Number of Reports",
               xaxis = list(title = "Number of Reports"),
               yaxis = list(title = ""),
               margin = list(l = 120, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Drug indication plot
    output$drug_indication <- renderPlotly({
      indication_data <- filtered_data() %>%
        count(drug_indication, sort = TRUE) %>%
        mutate(drug_indication = reorder(drug_indication, n))
      
      plot_ly(indication_data, x = ~n, y = ~drug_indication, type = 'bar', orientation = 'h',
              marker = list(color = '#4E73DF')) %>%
        layout(title = "Drug Indication Distribution",
               xaxis = list(title = "Number of Reports"),
               yaxis = list(title = ""),
               margin = list(l = 120, r = 50, b = 50, t = 80, pad = 4))
    })
    
    # Drug-reaction heatmap
    output$drug_reaction_heatmap <- renderPlotly({
      # Get top 10 drugs and top 10 reactions
      top_drugs <- filtered_data() %>%
        count(drug_name, sort = TRUE) %>%
        top_n(10, n) %>%
        pull(drug_name)
      
      top_reactions <- filtered_data() %>%
        count(reaction, sort = TRUE) %>%
        top_n(10, n) %>%
        pull(reaction)
      
      # Create heatmap data
      heatmap_data <- filtered_data() %>%
        filter(drug_name %in% top_drugs, reaction %in% top_reactions) %>%
        count(drug_name, reaction) %>%
        tidyr::spread(reaction, n, fill = 0)
      
      # Convert to matrix for heatmap
      drug_names <- heatmap_data$drug_name
      heatmap_matrix <- as.matrix(heatmap_data[, -1])
      rownames(heatmap_matrix) <- drug_names
      
      # Create heatmap
      plot_ly(
        x = colnames(heatmap_matrix),
        y = rownames(heatmap_matrix),
        z = heatmap_matrix,
        type = "heatmap",
        colorscale = "Blues"
      ) %>%
        layout(
          title = "Drug-Reaction Heatmap",
          xaxis = list(title = "Reaction"),
          yaxis = list(title = "Drug"),
          margin = list(l = 120, r = 50, b = 100, t = 80, pad = 4)
        )
    })
  })
}

########################################## Individual Reports server
individualReportsServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Case reports table
    output$case_reports_table <- renderDT({
      datatable(
        filtered_data() %>%
          select(report_id, date_received, patient_age, patient_sex, drug_name, 
                 reaction, seriousness, outcome),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "500px"
        ),
        rownames = FALSE
      )
    })
    
    # Download handlers
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("faers-case-reports-", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        data_to_save <- filtered_data() %>%
          select(report_id, date_received, patient_age, patient_sex, drug_name, 
                 reaction, seriousness, outcome)
        
        write.csv(data_to_save, file, row.names = FALSE)
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("faers-case-reports-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
      },
      content = function(file) {
        data_to_save <- filtered_data() %>%
          select(report_id, date_received, patient_age, patient_sex, drug_name, 
                 reaction, seriousness, outcome)
        
        # Need writexl package
        if (!requireNamespace("writexl", quietly = TRUE)) {
          install.packages("writexl")
        }
        writexl::write_xlsx(data_to_save, file)
      }
    )
  })
}

##################################### Custom Report Server
customReportServer <- function(id, filtered_data, all_plots) {
  moduleServer(id, function(input, output, session) {
    # Preview title and subtitle
    output$preview_title <- renderText({
      input$report_title
    })
    
    output$preview_subtitle <- renderText({
      input$report_subtitle
    })
    
    # Preview of report contents
    output$preview_contents <- renderUI({
      items <- c()
      
      if(input$include_summary) {
        items <- c(items, "Summary Statistics")
      }
      
      if(length(input$time_trends) > 0) {
        time_trend_names <- names(which(c("Reports Over Time", "Yearly Trend", "Quarterly Trend", "Monthly Trend") %in% 
                                          names(input$time_trends[input$time_trends])))
        items <- c(items, paste("Time Trends:", paste(time_trend_names, collapse = ", ")))
      }
      
      if(length(input$demographics) > 0) {
        demographic_names <- names(which(c("Age Distribution", "Gender Distribution", "Country Distribution") %in% 
                                           names(input$demographics[input$demographics])))
        items <- c(items, paste("Demographics:", paste(demographic_names, collapse = ", ")))
      }
      
      if(length(input$drug_analysis) > 0) {
        drug_names <- names(which(c("Top Drugs", "Top Reactions", "Drug Indications", "Drug-Reaction Heatmap") %in% 
                                    names(input$drug_analysis[input$drug_analysis])))
        items <- c(items, paste("Drug Analysis:", paste(drug_names, collapse = ", ")))
      }
      
      # Create list items
      tags$ul(
        lapply(items, function(item) {
          tags$li(item)
        })
      )
    })
    
    # Generate PDF report
    output$generate_report <- downloadHandler(
      filename = function() {
        sanitized_title <- gsub("[^a-zA-Z0-9]", "-", input$report_title)
        paste0(sanitized_title, "-", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
      },
      content = function(file) {
        # Temporary file to save the report to
        temp_report <- tempfile(fileext = ".Rmd")
        
        # Create the R Markdown code
        rmd_content <- paste0(
          "---\n",
          "title: \"", input$report_title, "\"\n",
          "subtitle: \"", input$report_subtitle, "\"\n",
          "date: \"", format(Sys.Date(), "%B %d, %Y"), "\"\n",
          "output: pdf_document\n",
          "---\n\n",
          
          "```{r setup, include=FALSE}\n",
          "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
          "library(dplyr)\n",
          "library(ggplot2)\n",
          "library(plotly)\n",
          "```\n\n"
        )
        
        # Add notes if provided
        if (nchar(input$report_notes) > 0) {
          rmd_content <- paste0(
            rmd_content,
            "## Notes\n\n",
            input$report_notes, "\n\n"
          )
        }
        
        # Add summary statistics if selected
        if (input$include_summary) {
          rmd_content <- paste0(
            rmd_content,
            "## Summary Statistics\n\n",
            "Total Reports: ", nrow(filtered_data()), "\n\n",
            "Serious Reports: ", filtered_data() %>% filter(seriousness == "Serious") %>% nrow(), "\n\n",
            "Deaths: ", filtered_data() %>% filter(outcome == "Death") %>% nrow(), "\n\n",
            "Hospitalizations: ", filtered_data() %>% filter(outcome == "Hospitalization") %>% nrow(), "\n\n"
          )
        }
        
        # Function to add plots
        add_plots <- function(plot_ids, section_title) {
          if (length(plot_ids) > 0) {
            section_content <- paste0("## ", section_title, "\n\n")
            
            for (plot_id in plot_ids) {
              # Get the display name for the plot
              plot_display_name <- switch(plot_id,
                                          "time_trend_plot" = "Reports Over Time",
                                          "yearly_trend" = "Yearly Trend",
                                          "quarterly_trend" = "Quarterly Trend",
                                          "monthly_trend" = "Monthly Trend",
                                          "age_distribution" = "Age Distribution",
                                          "gender_distribution" = "Gender Distribution",
                                          "country_distribution" = "Country Distribution",
                                          "top_drugs" = "Top Drugs",
                                          "top_reactions_plot" = "Top Reactions",
                                          "drug_indication" = "Drug Indications",
                                          "drug_reaction_heatmap" = "Drug-Reaction Heatmap",
                                          plot_id)
              
              section_content <- paste0(
                section_content,
                "### ", plot_display_name, "\n\n",
                "```{r ", plot_id, ", fig.height=5, fig.width=7}\n",
                "print(all_plots$", plot_id, "())\n",
                "```\n\n"
              )
            }
            
            return(section_content)
          }
          return("")
        }
        
        # Add time trend plots
        if (length(input$time_trends) > 0) {
          rmd_content <- paste0(
            rmd_content,
            add_plots(input$time_trends, "Time Trends")
          )
        }
        
        # Add demographic plots
        if (length(input$demographics) > 0) {
          rmd_content <- paste0(
            rmd_content,
            add_plots(input$demographics, "Demographics")
          )
        }
        
        # Add drug analysis plots
        if (length(input$drug_analysis) > 0) {
          rmd_content <- paste0(
            rmd_content,
            add_plots(input$drug_analysis, "Drug Analysis")
          )
        }
        
        # Write the R Markdown content to the temp file
        writeLines(rmd_content, temp_report)
        
        # Render the report
        rmarkdown::render(temp_report, output_file = file)
      }
    )
  })
}


########################################## About server
aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for about (likely empty)
  })
}

########################################## Filter Sidebar server
filterSidebarServer <- function(id, mock_data) {
  moduleServer(id, function(input, output, session) {
    # Reactive filtered data based on user inputs
    filtered_data <- reactive({
      data <- mock_data
      
      # Filter by date range
      if (!is.null(input$date_range)) {
        data <- data %>% 
          filter(date_received >= input$date_range[1] & date_received <= input$date_range[2])
      }
      
      # Filter by drug
      if (!"All" %in% input$drug_filter && length(input$drug_filter) > 0) {
        data <- data %>% filter(drug_name %in% input$drug_filter)
      }
      
      # Filter by reaction
      if (!"All" %in% input$reaction_filter && length(input$reaction_filter) > 0) {
        data <- data %>% filter(reaction %in% input$reaction_filter)
      }
      
      # Filter by seriousness
      if (input$seriousness_filter != "All") {
        data <- data %>% filter(seriousness == input$seriousness_filter)
      }
      
      # Filter by age range
      data <- data %>% 
        filter(is.na(patient_age) | (patient_age >= input$age_range[1] & patient_age <= input$age_range[2]))
      
      return(data)
    })
    
    return(filtered_data)
  })
}

############################################# app.R - Main UI and Server
# Generate mock data function
generate_mock_data <- function(n = 5000) {
  # Common drugs
  drugs <- c("Aspirin", "Lisinopril", "Atorvastatin", "Metformin", 
             "Levothyroxine", "Amlodipine", "Metoprolol", "Albuterol", 
             "Omeprazole", "Losartan", "Gabapentin", "Hydrochlorothiazide",
             "Sertraline", "Simvastatin", "Montelukast")
  
  # Common reactions
  reactions <- c("Headache", "Nausea", "Dizziness", "Fatigue", "Rash", 
                 "Vomiting", "Diarrhea", "Abdominal Pain", "Insomnia", 
                 "Cough", "Shortness of Breath", "Chest Pain", "Back Pain",
                 "Muscle Pain", "Joint Pain", "Allergic Reaction", 
                 "High Blood Pressure", "Low Blood Pressure", "Anxiety", "Depression")
  
  # Generate data
  data <- data.frame(
    report_id = paste0("RPT", sample(100000:999999, n, replace = FALSE)),
    date_received = sample(seq(as.Date('2020-01-01'), as.Date('2024-12-31'), by="day"), n, replace = TRUE),
    patient_age = sample(c(NA, 0:100), n, replace = TRUE, prob = c(0.05, rep(0.95/101, 101))),
    patient_sex = sample(c("Male", "Female", "Unknown"), n, replace = TRUE, prob = c(0.47, 0.50, 0.03)),
    patient_weight = round(rnorm(n, mean = 70, sd = 15), 1),
    drug_name = sample(drugs, n, replace = TRUE),
    drug_indication = sample(c("Hypertension", "Hyperlipidemia", "Diabetes", "Hypothyroidism", 
                               "Asthma", "Pain", "Depression", "Anxiety", "Acid Reflux", 
                               "Heart Disease", "Allergies", "Other"), n, replace = TRUE),
    reaction = sample(reactions, n, replace = TRUE),
    seriousness = sample(c("Serious", "Non-serious"), n, replace = TRUE, prob = c(0.25, 0.75)),
    outcome = sample(c("Hospitalization", "Life-threatening", "Death", "Disability", 
                       "Congenital Anomaly", "Other Serious", "Recovered", "Recovering", 
                       "Not Recovered", "Unknown"), n, replace = TRUE),
    reporter_type = sample(c("Physician", "Pharmacist", "Consumer", "Other Health Professional", 
                             "Lawyer", "Unknown"), n, replace = TRUE),
    country = sample(c("US", "CA", "GB", "FR", "DE", "JP", "AU", "BR", "MX", "IN", "Other"), 
                     n, replace = TRUE, prob = c(0.6, rep(0.4/10, 10)))
  )
  
  # Add a year and month column for easier aggregation
  data$year <- year(data$date_received)
  data$month <- month(data$date_received)
  data$quarter <- quarter(data$date_received)
  
  # Return the data
  return(data)
}






############################################# app.R


# UI
ui <- page_navbar(
  title = span(
    img(src = "logo.png", height = "50px", alt = "Logo"),
    "FAERS Dashboard"
  ),
  # Add favicon
  window_title = "FDA Adverse Event Reporting System Dashboard",
  theme = bs_theme(bootswatch = "pulse", primary = '#89CFF0',"navbar-bg" = "#F0FFFF"),
  # Footer bar
  footer = div(
  style = "padding: 5px; text-align: center; position : fixed-bottom; background-color: #F0FFFF; border-top: 1px solid #dee2e6;",
  "© 2025 - By Reda TRANKIL"
  ),
  id = "navbar",
  #position = "fixed-top",
  # Add favicon here with the other named parameters
  header = tags$head(tags$link(rel = "icon", href = "favicon.ico")),
  
  
  # Sidebar with filters
  sidebar = sidebar(
    filterSidebarUI("filters")
  ),
  
  # Main navigation tabs
  nav_panel("Dashboard", dashboardUI("dashboard")),
  nav_panel("Reports Over Time", reportsOverTimeUI("reports_time")),
  nav_panel("Demographics", demographicsUI("demographics")),
  nav_panel("Drug Analysis", drugAnalysisUI("drugs")),
  nav_panel("Individual Reports", individualReportsUI("reports")),
  nav_panel("Generate Report", customReportUI("report_generator")),
  nav_panel("About", aboutUI("about"))

  
)
# Server
server <- function(input, output, session) {
  # Generate mock data
  mock_data <- generate_mock_data(10000)
  
  # Get filtered data from filters module
  filtered_data <- filterSidebarServer("filters", mock_data)
  
  # Initialize all module servers
  dashboardServer("dashboard", filtered_data)
  reportsOverTimeServer("reports_time", filtered_data)
  demographicsServer("demographics", filtered_data)
  drugAnalysisServer("drugs", filtered_data)
  individualReportsServer("reports", filtered_data)
  aboutServer("about")
  
  # Create a reactive list to store all plots for reporting
  all_plots <- reactiveValues()
  
  # Store dashboard plots
  all_plots$time_trend_plot <- reactive({
    time_data <- filtered_data() %>%
      count(date_received) %>%
      arrange(date_received)
    
    ggplot(time_data, aes(x = date_received, y = n)) +
      geom_line(color = '#4E73DF') +
      labs(title = "Reports Over Time",
           x = "Date",
           y = "Number of Reports") +
      theme_minimal()
  })
  
  all_plots$top_reactions_plot <- reactive({
    top_reactions <- filtered_data() %>%
      count(reaction, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(reaction = reorder(reaction, n))
    
    ggplot(top_reactions, aes(x = n, y = reaction)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Top 10 Adverse Events",
           x = "Number of Reports",
           y = "") +
      theme_minimal()
  })
  
  all_plots$age_distribution <- reactive({
    age_data <- filtered_data() %>%
      filter(!is.na(patient_age)) %>%
      mutate(age_group = cut(patient_age, breaks = seq(0, 100, by = 10),
                             labels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                        "50-59", "60-69", "70-79", "80-89", "90+"))) %>%
      count(age_group)
    
    ggplot(age_data, aes(x = age_group, y = n)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Age Distribution",
           x = "Age Group",
           y = "Number of Reports") +
      theme_minimal()
  })
  
  all_plots$top_drugs <- reactive({
    top_drugs <- filtered_data() %>%
      count(drug_name, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(drug_name = reorder(drug_name, n))
    
    ggplot(top_drugs, aes(x = n, y = drug_name)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Top 10 Drugs",
           x = "Number of Reports",
           y = "") +
      theme_minimal()
  })
  
  # Store reports over time plots
  all_plots$yearly_trend <- reactive({
    yearly_data <- filtered_data() %>%
      count(year) %>%
      arrange(year)
    
    ggplot(yearly_data, aes(x = as.factor(year), y = n)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Reports by Year",
           x = "Year",
           y = "Number of Reports") +
      theme_minimal()
  })
  
  all_plots$quarterly_trend <- reactive({
    quarterly_data <- filtered_data() %>%
      count(year, quarter) %>%
      arrange(year, quarter) %>%
      mutate(quarter_label = paste0(year, " Q", quarter))
    
    ggplot(quarterly_data, aes(x = quarter_label, y = n)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Reports by Quarter",
           x = "Quarter",
           y = "Number of Reports") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  all_plots$monthly_trend <- reactive({
    monthly_data <- filtered_data() %>%
      count(year, month) %>%
      arrange(year, month) %>%
      mutate(month_label = paste0(year, "-", sprintf("%02d", month)))
    
    ggplot(monthly_data, aes(x = month_label, y = n)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Reports by Month",
           x = "Month",
           y = "Number of Reports") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Store demographic plots
  all_plots$gender_distribution <- reactive({
    gender_data <- filtered_data() %>%
      count(patient_sex) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ggplot(gender_data, aes(x = "", y = n, fill = patient_sex)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c('#4E73DF', '#E74A3B', '#858796')) +
      labs(title = "Gender Distribution", fill = "Gender") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  all_plots$country_distribution <- reactive({
    country_data <- filtered_data() %>%
      count(country, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(country = reorder(country, n))
    
    ggplot(country_data, aes(x = n, y = country)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Reports by Country (Top 10)",
           x = "Number of Reports",
           y = "") +
      theme_minimal()
  })
  
  # Store drug analysis plots
  all_plots$drug_indication <- reactive({
    indication_data <- filtered_data() %>%
      count(drug_indication, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(drug_indication = reorder(drug_indication, n))
    
    ggplot(indication_data, aes(x = n, y = drug_indication)) +
      geom_col(fill = '#4E73DF') +
      labs(title = "Drug Indication Distribution (Top 10)",
           x = "Number of Reports",
           y = "") +
      theme_minimal()
  })
  
  all_plots$drug_reaction_heatmap <- reactive({
    # Get top 10 drugs and top 10 reactions
    top_drugs <- filtered_data() %>%
      count(drug_name, sort = TRUE) %>%
      top_n(10, n) %>%
      pull(drug_name)
    
    top_reactions <- filtered_data() %>%
      count(reaction, sort = TRUE) %>%
      top_n(10, n) %>%
      pull(reaction)
    
    # Create heatmap data
    heatmap_data <- filtered_data() %>%
      filter(drug_name %in% top_drugs, reaction %in% top_reactions) %>%
      count(drug_name, reaction) %>%
      mutate(drug_name = factor(drug_name, levels = rev(top_drugs)),
             reaction = factor(reaction, levels = top_reactions))
    
    ggplot(heatmap_data, aes(x = reaction, y = drug_name, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#4E73DF") +
      labs(title = "Drug-Reaction Heatmap",
           x = "",
           y = "",
           fill = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Add report generator server
  customReportServer("report_generator", filtered_data, all_plots)
}
# Run the app
shinyApp(ui = ui, server = server)
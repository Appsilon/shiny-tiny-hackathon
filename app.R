library(shiny)
library(bslib)
library(fontawesome)
library(DT)
library(plotly)
library(readr)
library(dplyr)

df <- read_csv(file = "data.csv")

ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      .app-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 20px;
        background-color: white;
        border-bottom: 1px solid #ddd;
      }
      
      .header-title {
        color: #0275d8;  /* Bootstrap blue color */
        font-size: 24px;
        font-weight: bold;
        margin: 0;
      }
      
      .logo-container img {
        height: 60px;
      }
      
      /* Custom nav styling */
      .custom-navbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        background-color: #0275d8; /* Changed to blue */
        padding: 8px 16px;
        border-bottom: 1px solid #0262b7;
      }
      
      .navbar-left, .navbar-right {
        display: flex;
        align-items: center;
      }
      
      .nav-link {
        color: white; /* Changed text color to white for better contrast */
        padding: 8px 16px;
        text-decoration: none;
      }
      
      .nav-link:hover {
        text-decoration: underline;
        color: #f0f0f0; /* Slightly darker white on hover */
      }
      
      .search-container {
        display: flex;
        align-items: center;
        border: none; /* Removed border */
        border-radius: 0.25rem;
        padding: 0.25rem 0.5rem;
        margin-left: 1rem;
        background-color: #0275d8; /* Changed to blue */
      }
      
      .search-icon {
        color: white; /* Changed to white */
        margin-right: 0.5rem;
      }
      
      .search-input {
        border: none;
        outline: none;
        background: transparent;
        width: 150px;
        color: white; /* Text color to white */
      }
      
      .search-input::placeholder {
        color: white; /* Placeholder text color to white */
        opacity: 0.8;
      }
      
      /* Custom action button styling - ENLARGED */
      .custom-action-btn {
        background-color: #0275d8;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 12px 20px;  /* Increased padding */
        margin-right: 10px;
        margin-bottom: 10px;
        cursor: pointer;
        font-size: 16px;  /* Increased font size */
        font-weight: 500;  /* Slightly bolder */
        min-width: 140px;  /* Minimum width */
      }
      
      .custom-action-btn:hover {
        background-color: #0262b7;
      }
      
      /* Container for second row content */
      .content-row {
        display: flex;
        margin-top: 20px;
        gap: 20px;
      }
      
      .value-boxes-container {
        flex: 3;
      }
      
      .controls-container {
        flex: 1;
        padding: 15px;
        background-color: white;
        border-radius: 5px;
      }
      
      /* Custom value box styling */
      .value-box-custom .value-box-value {
        font-size: 1.8rem !important;  /* Reduced from default */
      }
      
      .value-box-custom .value-box-title {
        font-size: 0.9rem !important;  /* Reduced from default */
      }
      
      .value-box-custom {
        border: none !important;  /* Remove border */
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);  /* Optional: light shadow instead of border */
      }
      
      /* Data visualization row */
      .data-viz-row {
        display: flex;
        margin-top: 20px;
        gap: 20px;
      }
      
      .datatable-container {
        flex: 1;
        padding: 15px;
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      .plot-container {
        flex: 1;
        padding: 15px;
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      /* Card header styling */
      .card-header {
        color: #808080;  /* Grey color */
        font-weight: bold;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # Custom header with title and logo
  div(class = "app-header",
      h1(class = "header-title", "FDA Adverse Event Reporting System (FAERS) Public Dashboard"),
      div(class = "logo-container", img(src = "www\\logo.png"))
  ),
  
  # Custom navigation bar
  div(class = "custom-navbar",
      # Left side of navbar
      div(class = "navbar-left",
          tags$a(href = "#", id = "home", class = "nav-link", "Home"),
          div(class = "search-container",
              tags$i(class = "search-icon", fa("search")),
              tags$input(type = "text", class = "search-input", placeholder = "Search")
          )
      ),
      
      # Right side of navbar
      div(class = "navbar-right",
          actionLink(inputId = "disclaimer", label = "Disclaimer", class = "nav-link"),
          actionLink(inputId = "report_problem", label = "Report a Problem", class = "nav-link"),
          actionLink(inputId = "faq", label = "FAQ", class = "nav-link"),
          actionLink(inputId = "feedback", label = "Site Feedback", class = "nav-link")
      )
  ),
  
  # Content row with value boxes and controls
  div(class = "content-row",
      # Left side - Value boxes in a single row
      div(class = "value-boxes-container",
          layout_column_wrap(
            width = 1/3,  # This divides the container into 3 equal columns
            height = "auto",
            value_box(
              title = "Total Reports",
              value = textOutput("total_reports"),
              showcase = bsicons::bs_icon("graph-up"),
              theme_color = "white",
              fill = TRUE,
              style = "color: #0275d8;",  # Blue color for text and icon
              class = "value-box-custom"  # Add custom class
            ),
            value_box(
              title = "Serious Reports (excluding deaths)",
              value = textOutput("serious_reports"),
              showcase = bsicons::bs_icon("exclamation-triangle-fill"),
              theme_color = "white",
              fill = TRUE,
              style = "color: #fd7e14;",  # Warning/orange color for text and icon
              class = "value-box-custom"  # Add custom class
            ),
            value_box(
              title = "Death Reports",
              value = textOutput("death_reports"),
              showcase = bsicons::bs_icon("x-circle-fill"),
              theme_color = "white",
              fill = TRUE,
              style = "color: #dc3545;",  # Danger/red color for text and icon
              class = "value-box-custom"  # Add custom class
            )
          )
      ),
      
      # Right side - Controls
      div(class = "controls-container",
          # Select input
          selectInput(
            inputId = "filter_select",
            label = NULL,
            choices = c("Reports by Report Type")
          ),
          
          # Action buttons
          div(
            style = "margin-top: 20px;",
            actionButton(
              inputId = "all_years_btn",
              label = "All years",
              class = "custom-action-btn"
            ),
            actionButton(
              inputId = "ten_years_btn",
              label = "Last 10 years",
              class = "custom-action-btn"
            )
          )
      )
  ),
  
  # Data visualization row - DataTable and Plotly outputs
  div(class = "data-viz-row",
      # Left side - DataTable
      div(class = "datatable-container",
          h4("Adverse Events Data", class = "card-header"),
          DTOutput("events_table")
      ),
      
      # Right side - Plotly visualization
      div(class = "plot-container",
          h4("Event Trends Over Time", class = "card-header"),
          plotlyOutput("events_plot", height = "400px")
      )
  ),
  
  # Footer row with left and right aligned content
  div(style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 20px; margin-top: 20px;",
      tags$b("Data as of December 31, 2023"),
      actionLink("vdp", "Vulnerability Disclosure Policy", style = "color: #0275d8;")
  )
)

server <- function(input, output, session) {
  currentData <- reactiveVal(df)
  
  output$total_reports <- renderText({
    currentData() %>% nrow()
  })
  
  output$serious_reports <- renderText({
    currentData() %>%
      filter(Seriousness == "Serious" & DeathCategory == "No Death") %>% 
      nrow()
  })
  
  output$death_reports <- renderText({
    currentData() %>%
      filter(DeathCategory == "Death") %>% 
      nrow()
  })
  
  observeEvent(input$all_years_btn, {
    currentData(df)
  })
  
  observeEvent(input$ten_years_btn, {
    l10yr <- max(df$Year) - 9
    filt_data <- df %>% filter(Year >= l10yr)
    currentData(filt_data)
  })
  
  output$events_table <- renderDT({
    df_summary <- currentData() %>%
      group_by(Year) %>%
      summarise(
        `Total Reports` = sum(NumberOfReports),
        Expedited       = sum(if_else(ReportType == "Expedited", NumberOfReports, 0)),
        `Non-Expedited` = sum(if_else(ReportType == "Non-Expedited", NumberOfReports, 0)),
        Direct          = sum(if_else(ReportType == "Direct", NumberOfReports, 0)),
        BSR             = sum(if_else(ReportType == "BSR", NumberOfReports, 0))
      ) %>%
      ungroup() %>% 
      mutate(Year = as.character(Year))
    
    grand_total <- df_summary %>%
      summarise(
        Year            = "Total Reports",
        `Total Reports` = sum(`Total Reports`),
        Expedited       = sum(Expedited),
        `Non-Expedited` = sum(`Non-Expedited`),
        Direct          = sum(Direct),
        BSR             = sum(BSR)
      )
    
    final_table <- bind_rows(grand_total, df_summary) %>%
      mutate(Year = as.character(Year)) %>%
      mutate(sort_order = if_else(Year == "Total Reports", 9999, as.numeric(Year))) %>%
      arrange(desc(sort_order)) %>%
      select(-sort_order)
    
    datatable(
      final_table, 
      rownames = FALSE, 
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        initComplete = JS("
          function(settings, json) {
            var api = this.api();
            
            // Year column search input
            var yearColumn = api.column(0);
            var yearInput = $('<input type=\"text\" placeholder=\"Search Year\" style=\"width:100%;\"/>')
              .appendTo($(yearColumn.header()).empty())
              .on('keyup change', function() {
                if (yearColumn.search() !== this.value) {
                  yearColumn.search(this.value, false, false).draw();
                }
              });
            
            // Add column filter dropdown
            var filterContainer = $('<div style=\"text-align:right; margin-bottom:8px;\"></div>')
              .insertBefore($(api.table().container()).find('table'));
            
            var filterLabel = $('<span>Report Type: </span>').appendTo(filterContainer);
            
            var filterSelect = $('<select></select>')
              .appendTo(filterContainer)
              .on('change', function() {
                var val = $(this).val();
                
                // Show all columns
                if (val === 'All') {
                  api.columns().visible(true);
                } else {
                  // First hide all columns except Year
                  api.columns([1,2,3,4,5]).visible(false);
                  
                  // Then show only the selected column
                  if (val === 'Total Reports') api.column(1).visible(true);
                  if (val === 'Expedited') api.column(2).visible(true);
                  if (val === 'Non-Expedited') api.column(3).visible(true);
                  if (val === 'Direct') api.column(4).visible(true);
                  if (val === 'BSR') api.column(5).visible(true);
                }
              });
            
            // Add options to the dropdown
            $('<option value=\"All\" selected>All</option>').appendTo(filterSelect);
            $('<option value=\"Total Reports\">Total Reports</option>').appendTo(filterSelect);
            $('<option value=\"Expedited\">Expedited</option>').appendTo(filterSelect);
            $('<option value=\"Non-Expedited\">Non-Expedited</option>').appendTo(filterSelect);
            $('<option value=\"Direct\">Direct</option>').appendTo(filterSelect);
            $('<option value=\"BSR\">BSR</option>').appendTo(filterSelect);
            
            // Reduce font size of table cells
            $(api.table().container()).find('table').css('font-size', '0.85em');
          }
        ")
      )
    ) %>% 
      formatCurrency(
        columns = c("Total Reports", "Expedited", "Non-Expedited", "Direct", "BSR"),
        currency = "",
        digits = 0,
        mark = ","
      )
  })
  
  output$events_plot <- renderPlotly({
    df_summary <- currentData() %>%
      group_by(Year, ReportType) %>%
      summarise(ReportCount = sum(NumberOfReports, na.rm = TRUE)) %>%
      ungroup()
    
    df_summary$ReportType <- factor(df_summary$ReportType, 
                                    levels = c("BSR", "Direct", "Expedited", "Non-Expedited"))
    
    df_totals <- df_summary %>%
      group_by(Year) %>%
      summarise(Total = sum(ReportCount))
    
    custom_colors <- c("lightblue", "darkblue", "darkred", "darkgreen")
    
    fig <- plot_ly(df_summary, 
                   x = ~Year, 
                   y = ~ReportCount, 
                   color = ~ReportType, 
                   colors = custom_colors,
                   type = 'bar',
                   text = ~ReportCount,
                   textposition = 'auto') %>%
      layout(title = "Reports received by Report Type",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Report Count"),
             barmode = 'stack',
             legend = list(orientation = "h",
                           x = 0.5, 
                           y = -0.2, 
                           xanchor = "center"))
    
    annotations <- list()
    for(i in seq_along(df_totals$Year)) {
      annotations[[i]] <- list(
        x = df_totals$Year[i],
        y = df_totals$Total[i],
        text = df_totals$Total[i],
        xanchor = 'center',
        yanchor = 'bottom',
        showarrow = FALSE,
        textangle = 90
      )
    }
    
    fig <- fig %>% layout(annotations = annotations)
    fig
  })
  
  observeEvent(input$disclaimer, {
    showModal(
      modalDialog(
        title = "Disclaimer",
        HTML("<p><strong>Disclaimer:</strong><br>The data presented on this dashboard is sourced from the FDA Adverse Event Reporting System (FAERS) and reflects reports submitted voluntarily by healthcare professionals, consumers, and manufacturers. While FAERS is a valuable tool for monitoring drug and therapeutic product safety, it has limitations. Reported events do not necessarily mean the product caused the adverse event, and the information may be incomplete, inaccurate, or unverified. The number of reports should not be interpreted as an incidence rate or an indicator of the safety profile of a product. The presence of a report in the database does not establish causality. Users are encouraged to consider additional clinical, scientific, and epidemiological information when evaluating drug safety. This dashboard is intended for informational and educational purposes only and should not be used as a substitute for professional medical advice, diagnosis, or treatment. The U.S. Food and Drug Administration (FDA) does not endorse any specific drug or treatment based on FAERS data.<br><strong>Data as of December 31, 2023.</strong></p>"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$faq, {
    showModal(
      modalDialog(
        title = "FAQ",
        HTML(
          '<h2>FAERS Dashboard FAQ</h2>

        <div class="faq">
            <div class="question">1. What is the FAERS Public Dashboard?</div>
            <div class="answer">The FAERS Public Dashboard is an online tool provided by the FDA to access reports of adverse events related to drug and medical product use.</div>
        </div>
        
        <div class="faq">
            <div class="question">2. How can I filter reports by type or year?</div>
            <div class="answer">You can use the dropdown menus and buttons on the dashboard to filter reports by report type and select data for all years or the last 10 years.</div>
        </div>
        
        <div class="faq">
            <div class="question">3. What do the report categories (Expedited, Non-Expedited, Direct, BSR) mean?</div>
            <div class="answer">These categories represent different types of adverse event reports based on severity, submission source, and reporting requirements.</div>
        </div>'
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$report_problem, {
    showModal(
      modalDialog(
        title = "Report a Problem",
        textAreaInput("problem_text", "Describe the issue:", "", rows = 4, width = "100%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_problem", "Submit")
        )
      )
    )
  })
  
  observeEvent(input$submit_problem,{
    
    updateTextAreaInput(inputId = "problem_text",value = "")
    
  })
  
  observeEvent(input$feedback, {
    showModal(
      modalDialog(
        title = "Feedback",
        textAreaInput("feedback_text", "Provide Feedback:", "", rows = 4, width = "100%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_feedback", "Submit")
        )
      )
    )
  })
  
  observeEvent(input$submit_feedback,{
    
    updateTextAreaInput(inputId = "feedback_text",value = "")
    
  })
  
  output$vdp = downloadHandler(
    filename = function() {
      "Vulnerability_Disclosure_Policy.pdf"  
    },
    content = function(file) {
      pdf_path <- "dummy_Vulnerability_Disclosure_Policy.pdf"
      
      if (file.exists(pdf_path)) {
        file.copy(pdf_path, file)
      } else {
        stop("File not found: Check the file path.")
      }
    },
    contentType = "application/pdf"
  )

  
}

shinyApp(ui, server)

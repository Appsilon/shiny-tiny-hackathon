# app.R
library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(rmarkdown)

# Create a color-blind friendly palette
# These colors are distinguishable for most types of color blindness
cb_palette <- list(
  primary = "#0072B2",       # Blue
  secondary = "#009E73",     # Green
  warning = "#E69F00",       # Orange
  danger = "#CC79A7",        # Pink
  highlight = "#D55E00",     # Red-orange
  info = "#56B4E9",          # Light blue
  success = "#009E73",       # Green
  neutral = "#999999",       # Gray
  text_dark = "#2A3132",     # Dark gray for text
  text_light = "#F8F9FA",    # Light gray/white for text on dark bg
  bg_light = "#FFFFFF",      # White background
  bg_dark = "#2A3132",       # Dark background
  grid = "#CCCCCC"           # Grid lines
)

# Create a custom theme with color-blind friendly colors
cb_theme <- bs_theme(
  version = 5,
  preset = "default",
  
  # Core colors
  primary = cb_palette$primary,
  secondary = cb_palette$secondary,
  success = cb_palette$success,
  warning = cb_palette$warning,
  danger = cb_palette$danger,
  info = cb_palette$info,
  
  # Text and background
  fg = cb_palette$text_dark,
  bg = cb_palette$bg_light,
  
  # Font
  base_font = "Roboto, 'Helvetica Neue', Helvetica, Arial, sans-serif",
  heading_font = "Roboto, 'Helvetica Neue', Helvetica, Arial, sans-serif",
  
  # Custom CSS
  "card-cap-bg" = "#F5F5F5",
  "navbar-bg" = cb_palette$primary
)

# Apply custom CSS for a consistent look
cb_theme <- cb_theme %>%
  bs_add_rules(
    c(
      # Consistent text colors
      ".text-primary { color: #0072B2 !important; }",
      ".text-secondary { color: #009E73 !important; }",
      ".text-warning { color: #E69F00 !important; }",
      ".text-danger { color: #CC79A7 !important; }",
      ".text-info { color: #56B4E9 !important; }",
      
      # Improved contrast for cards
      ".card { border-color: #dddddd; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }",
      ".card-header { background-color: #f5f5f5; border-bottom: 1px solid #dddddd; font-weight: 500; }",
      
      # Value box styling
      ".value-box-primary .value-box-showcase { background-color: #0072B2 !important; }",
      ".value-box-warning .value-box-showcase { background-color: #E69F00 !important; }",
      ".value-box-danger .value-box-showcase { background-color: #CC79A7 !important; }",
      
      # Table styling
      ".table th { border-bottom: 2px solid #0072B2; }",
      ".table-striped tbody tr:nth-of-type(odd) { background-color: rgba(0, 114, 178, 0.05); }",
      
      # Improve form control contrast
      ".form-control:focus { border-color: #0072B2; box-shadow: 0 0 0 0.25rem rgba(0, 114, 178, 0.25); }",
      
      # Button styling
      ".btn-primary { background-color: #0072B2; border-color: #0072B2; }",
      ".btn-primary:hover, .btn-primary:focus { background-color: #005b8f; border-color: #005b8f; }",
      
      # Pagination controls
      ".page-item.active .page-link { background-color: #0072B2; border-color: #0072B2; }"
    )
  )

# Read the real data
files <- list.files("data", full.names = TRUE)

file_names <- tools::file_path_sans_ext(basename(files))

data <- map(files, function(x) {
  # Read each Excel file and clean it
  read_excel(x) %>%
    mutate(across(everything(), ~ ifelse(. == "-", NA, .)),
           across(everything(), as.numeric)) %>%
    filter(Year != "Total Reports")
  
}) %>%
  set_names(file_names)


# Extract the years for analysis
years <- data$report_type$Year

# Define UI --------------------------------------
ui <- page_navbar(
  title = "FDA Incident Reporting Dashboard",
  theme = cb_theme,
  fillable = TRUE,
  
  ## Dashboard panel -------------------------------
  nav_panel(title = "Dashboard", card(
    full_screen = TRUE,
    height = 1000,
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "dataset",
          "Select Dataset:",
          choices = c(
            "Report Type" = "report_type",
            "Reporter" = "reporter",
            "Reporter Region" = "reporter_region",
            "Report Seriousness" = "report_seriousness",
            "Reporter Age" = "report_age",
            "Reporter Sex" = "report_sex"
          )
        ),
        sliderInput(
          "year_range",
          "Select Year Range:",
          min = min(years),
          max = max(years),
          value = c(min(years), max(years)),
          sep = ""  # Remove comma separator
        ),
        selectInput(
          "chart_type",
          "Chart Type:",
          choices = c("Stacked Bar Chart", "Line Chart")
        ),
        actionButton(
          "generate_report",
          "Generate PDF Report",
          icon = icon("file-pdf"),
          class = "btn btn-primary",
          style = "margin-top: 20px; margin-bottom: 20px;"
        )
      ),
      fluidRow(
        height = "100px",
        layout_column_wrap(
          width = 1 / 3,
          value_box(
            title = "Total Reports",
            value = textOutput("total_reports"),
            showcase = bsicons::bs_icon("clipboard-data"),
            theme = value_box_theme(bg = cb_palette$primary, fg = "white")
          ),
          value_box(
            title = "Serious Reports",
            value = textOutput("serious_reports"),
            showcase = bsicons::bs_icon("exclamation-triangle"),
            theme = value_box_theme(bg = cb_palette$warning, fg = "white")
          ),
          value_box(
            title = "Death Reports",
            value = textOutput("death_reports"),
            showcase = bsicons::bs_icon("x-circle"),
            theme = value_box_theme(bg = cb_palette$danger, fg = "white")
          )
        ),
        fluidRow(card(plotlyOutput("report_chart"))),
        fluidRow(card(DTOutput("report_table")))
        
      )
    )
  )),
  
  ##  About Panel --------------------------------
  nav_panel(title = "About", card(
    card_header("About This Dashboard"),
    p(
      "This dashboard presents FDA incident reporting data across multiple dimensions:"
    ),
    tags$ul(
      tags$li("Sex Distribution: Reports categorized by gender"),
      tags$li("Age Distribution: Reports across different age groups"),
      tags$li(
        "Report Seriousness: Serious vs non-serious reports, including deaths"
      ),
      tags$li("Reporter Region: Domestic vs foreign reporting sources"),
      tags$li("Reporter Type: Types of individuals reporting incidents"),
      tags$li("Report Type: Categories of reports submitted to the FDA")
    ),
    p(
      "Use the navigation tabs and controls to explore different aspects of the data."
    ),
    p("Data is sourced directly from FDA reporting systems.")
  ))
)

# Define Server --------------------------------
server <- function(input, output, session) {
  ## Reactive datasets --------------------------
  selected_data <- reactive({
    req(input$dataset)
    data[[input$dataset]]
  })
  
  # Filter years based on selection
  filtered_data <- reactive({
    data <- selected_data()
    req(data)
    
    data %>%
      filter(Year %in% input$year_range[1]:input$year_range[2]) %>%
      select(-`Total Reports`)
    
  })
  
  ## Summary statistics ------------------
  output$total_reports <- renderText({
    # Get the most recent year's total
    total <- data[["report_seriousness"]] %>% filter(Year == max(Year)) %>% pull(`Total Reports`) %>% as.numeric()
    format(total, big.mark = ",")
  })
  
  output$serious_reports <- renderText({
    # Get the most recent year's total
    total <- data[["report_seriousness"]] %>% filter(Year == max(Year)) %>% pull(Serious) %>% as.numeric()
    format(total, big.mark = ",")
  })
  
  output$death_reports <- renderText({
    # Get the most recent year's total
    total <- data[["report_seriousness"]] %>% filter(Year == max(Year)) %>% pull(Death) %>% as.numeric()
    format(total, big.mark = ",")
  })
  
  # Report Chart ----------------------
  output$report_chart <- renderPlotly({
    data <- filtered_data()
    req(data)
    
    # Reshape data for plotting
    plot_data <- tidyr::pivot_longer(
      data,
      cols = -c(1:2),
      names_to = "Category",
      values_to = "Count"
    )
    
    # Define a color-blind friendly palette for the charts
    # ColorBrewer palette that is color-blind friendly
    cb_colors <- c("#0072B2", "#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#999999")
    
    if (input$chart_type == "Stacked Bar Chart") {
      # Create the stacked bar chart with color-blind friendly colors
      p <- ggplot(plot_data, aes(x = Year, y = Count, fill = Category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = cb_colors) +
        scale_y_continuous(labels = comma) +
        labs(
          #title = paste(input$dataset, "by Year"),
          x = "Year",
          y = "Count",
          fill = "Category"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          # Improved text legibility
          text = element_text(color = cb_palette$text_dark),
          # Improved grid lines
          panel.grid.major = element_line(color = cb_palette$grid, linewidth = 0.2),
          panel.grid.minor = element_line(color = cb_palette$grid, linewidth = 0.1)
        )
      
      ggplotly(p) %>%
        layout(legend = list(orientation = "v"))
      
    } else {
      # Create the line chart with color-blind friendly colors
      plot_ly(
        plot_data,
        x = ~ Year,
        y = ~ Count,
        color = ~ Category,
        colors = cb_colors,
        type = "scatter",
        mode = "lines+markers"
      ) %>%
        layout(
         # title = paste(input$dataset, "by Year"),
          xaxis = list(
            title = "Year",
            showgrid = TRUE,
            gridcolor = cb_palette$grid,
            zerolinecolor = cb_palette$grid,
            showline = TRUE,
            linecolor = cb_palette$text_dark,
            tickvals = years,
            tickangle = 45,
            tickfont = list(size = 10)
          ),
          yaxis = list(
            title = "Number of Reports",
            showgrid = TRUE,
            gridcolor = cb_palette$grid,
            zerolinecolor = cb_palette$grid,
            showline = TRUE,
            linecolor = cb_palette$text_dark
          ),
          font = list(
            family = "Roboto, Arial, sans-serif",
            size = 12,
            color = cb_palette$text_dark
          ),
          paper_bgcolor = cb_palette$bg_light,
          plot_bgcolor = cb_palette$bg_light,
          margin = list(l = 60, r = 30, t = 50, b = 80),
          hovermode = "x unified"
        )
    }
  })
  
  
  
  # Report Table ---------------------
  output$report_table <- renderDT({
    data <- filtered_data()
    req(data)
    
    datatable(
      data,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "calc(100vh - 400px)",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      extensions = 'Buttons',
      class = 'compact'
    ) %>%
      formatStyle(
        columns = names(data),
        backgroundColor = '#f8f9fa',
        borderWidth = '1px',
        borderCollapse = 'collapse',
        fontSize = '80%'
      ) %>%
      formatRound(columns = setdiff(names(data), "Year"), digits = 0)
  })
  
  ## Report Generation ----------------------
  observeEvent(input$generate_report, {
    # Show progress notification
    showNotification(
      "Generating PDF report...",
      type = "message",
      duration = NULL,
      id = "pdf_notification"
    )
    
    # Create temporary directory for report files
    report_dir <- tempdir()
    rmd_file <- file.path(report_dir, "report.Rmd")
    
    # Get data from current view for the report
    current_data <- filtered_data()
    
    # Create plot for the report using ggplot2 with color-blind friendly colors
    cb_colors <- c("#0072B2", "#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#999999")
    
    # Reshape data for plotting
    plot_data <- tidyr::pivot_longer(
      current_data,
      cols = -c(1:2),
      names_to = "Category",
      values_to = "Count"
    )
    
    # Create time trend plot
    time_trend_plot <- ggplot(plot_data, aes(x = Year, y = Count, color = Category)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = cb_colors) +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste(input$dataset, "Over Time"),
        x = "Year",
        y = "Number of Reports",
        color = "Category"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(color = cb_palette$text_dark),
        panel.grid.major = element_line(color = cb_palette$grid, linewidth = 0.2),
        panel.grid.minor = element_line(color = cb_palette$grid, linewidth = 0.1)
      )
    
    # Create bar chart
    bar_chart <- ggplot(plot_data, aes(x = Year, y = Count, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = cb_colors) +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste(input$dataset, "by Year"),
        x = "Year",
        y = "Number of Reports",
        fill = "Category"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(color = cb_palette$text_dark),
        panel.grid.major = element_line(color = cb_palette$grid, linewidth = 0.2),
        panel.grid.minor = element_line(color = cb_palette$grid, linewidth = 0.1)
      )
    
    # Save plots as image files
    ggsave(
      file.path(report_dir, "time_trend.png"),
      time_trend_plot,
      width = 8,
      height = 5
    )
    ggsave(
      file.path(report_dir, "bar_chart.png"),
      bar_chart,
      width = 8,
      height = 5
    )
    
    # Create the Rmd file content with dynamic data
    rmd_content <- paste0('---
title: "FDA FAERS Dashboard Report"
date: "', format(Sys.time(), '%B %d, %Y'), '"
output:
  pdf_document:
    toc: true
    toc_depth: 2
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(kableExtra)
```

# Executive Summary

This report provides an overview of adverse event reports from the FDA Adverse Event Reporting System (FAERS). The data presented focuses on ', input$dataset, ' data for years ', input$year_range[1], ' to ', input$year_range[2], '.

# Report Trends

The following chart shows the trend of FAERS reports over time:

```{r time-trend, fig.width=8, fig.height=4.5}
knitr::include_graphics("time_trend.png")
```

# Category Breakdown

Reports broken down by category:

```{r bar-chart, fig.width=8, fig.height=4.5}
knitr::include_graphics("bar_chart.png")
```

# Detailed Data

The table below provides detailed information about the report categories:

```{r data-table}
# Create data table from the filtered data
data_table <- ', capture.output(dput(current_data)), '

kable(data_table,
      caption = "', paste(input$dataset, "Data"), '",
      booktabs = TRUE,
      longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header"))
```

# Notes and Limitations

*FAERS data does not confirm a causal relationship between the drug and the reported adverse event. The number of reports in FAERS cannot be used to determine the incidence of an adverse event. FAERS data may contain duplicate and incomplete reports.*

---

Report generated on ', format(Sys.time(), "%Y-%m-%d at %H:%M:%S"), '
')
    
    # Write the Rmd content to a file
    writeLines(rmd_content, rmd_file)
    
    # Render the Rmd to PDF
    output_file <- file.path(report_dir, "FDA_FAERS_Report.pdf")
    
    # Process and render the report
    tryCatch({
      render(rmd_file, output_file = output_file)
      
      # Remove notification and show success
      removeNotification(id = "pdf_notification")
      showNotification(
        ui = div(
          icon("check"),
          "PDF report generated successfully",
          tags$br(),
          downloadLink("download_report", "Download Report", style = "color: white; text-decoration: underline;")
        ),
        type = "message",
        duration = 10
      )
      
      # Store the file path for download
      output$download_report <- downloadHandler(
        filename = function() {
          paste("FDA_FAERS_Report_",
                format(Sys.time(), "%Y%m%d_%H%M%S"),
                ".pdf",
                sep = "")
        },
        content = function(file) {
          file.copy(output_file, file)
        }
      )
    }, error = function(e) {
      # Handle errors
      removeNotification(id = "pdf_notification")
      showNotification(
        ui = div(
          icon("exclamation-triangle"),
          "Error generating PDF report:",
          e$message
        ),
        type = "error",
        duration = 10
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
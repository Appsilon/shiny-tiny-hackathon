# Header with white top bar and blue navigation bar
headerUI <- function() {
  tagList(
    # Top white bar with title and logo
    tags$div(
      class = "header-top",
      # Usar APP_TITLE del config.R en lugar de texto fijo
      tags$h1(APP_TITLE, class = "header-title"),
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/4/43/Logo_of_the_United_States_Food_and_Drug_Administration.svg", 
               alt = "FDA Logo",
               class = "header-logo")
    ),
    
    # Blue navigation bar
    tags$div(
      class = "header-nav",
      
      # Left side navigation items
      tags$div(
        class = "nav-left",
        tags$a(href = "#", class = "nav-link", "Home"),
        tags$a(href = "#", class = "nav-link", icon("search"), "Search")
      ),
      
      # Right side navigation items
      tags$div(
        class = "nav-right",
        tags$a(href = "#", class = "nav-link", "Disclaimer"),
        tags$a(href = "https://www.accessdata.fda.gov/scripts/medwatch/index.cfm?action=reporting.home", class = "nav-link", "Report a Problem"),
        tags$a(href = "https://fis.fda.gov/extensions/FPD-FAQ/FPD-FAQ.html", class = "nav-link", "FAQ"),
        tags$a(href = "mailto:DrugInfo@FDA.HHS.GOV?subject=FAERS%20Public%20Dashboard%20Feedback", class = "nav-link", "Site Feedback")
      )
    )
  )
}

# Statistics summary row with all elements in one line
summaryStatsUI <- function(datos) {
  # Crear las opciones para el dropdown con nombres para mostrar
  opciones_mostradas <- setNames(
    REPORTER_OPTIONS,
    sapply(REPORTER_OPTIONS, obtener_nombre_mostrar)
  )
  
  tags$div(
    class = "stats-container",
    
    # Total Reports
    tags$div(
      class = "stat-box",
      tags$div(
        class = "stat-label-report",
        "Total Reports"
      ),
      tags$div(
        class = "stat-value-report",
        icon("chart-line", class = "icon-primary"),
        format(datos$total, big.mark = ",")
      )
    ),
    
    # Serious Reports
    tags$div(
      class = "stat-box",
      tags$div(
        class = "stat-label-warning",
        "Serious Reports (excluding death)"
      ),
      tags$div(
        class = "stat-value-warning",
        icon("exclamation-triangle", class = "icon-warning"),
        format(datos$serious, big.mark = ",")
      )
    ),
    
    # Death Reports
    tags$div(
      class = "stat-box",
      tags$div(
        class = "stat-label-death",
        "Death Reports"
      ),
      tags$div(
        class = "stat-value-death",
        icon("skull-crossbones", class = "icon-danger"),
        format(datos$death, big.mark = ",")
      )
    ),
    
    # Dropdown filter
    tags$div(
      class = "filter-dropdown",
      selectInput(
        "reporterType", 
        NULL, 
        choices = opciones_mostradas,
        selected = "report_type",
        width = "100%"
      )
    )
  )
}

# Year filter buttons (All Years / Last 10 Years)
yearButtonsUI <- function() {
  tags$div(
    class = "year-filters",
    actionButton(
      "allYears", 
      "All Years", 
      class = "btn btn-year"
    ),
    actionButton(
      "last10Years", 
      "Last 10 Years", 
      class = "btn btn-year"
    )
  )
}

# Section header
sectionHeaderUI <- function(title) {
  tags$h3(title, class = "section-header")
}

# Main content (table and chart)
mainContentUI <- function() {
  tagList(
    # Section title
    tags$div(
      class = "data-table-container",
      sectionHeaderUI(textOutput("reportTitle")),
      
      # Table and chart in two columns
      tags$div(
        class = "content-flex-container",
        
        # Data table column
        tags$div(
          class = "table-container",
          tags$div(class = "data-table-wrapper", DTOutput("reportTable"))
        ),
        
        # Chart column
        tags$div(
          class = "chart-container",
          plotlyOutput("barChart", height = "500px")
        )
      )
    ),
    
    # Footer using config variables
    tags$div(
      class = "footer-container",
      # Data as of date section with policy link
      tags$div(
        class = "data-date-section",
        tags$em(paste("Data as of", format(Sys.Date(), "%B %d, %Y"))),
        tags$a(href = "#", class = "policy-link", "Vulnerability Disclosure Policy")
      ),
      
      # Main footer description using config variables
      tags$div(
        class = "footer-description",
        tags$p(HTML(FOOTER_INTRO_TEXT)),
        tags$ul(
          tags$li(HTML(FOOTER_DIRECT_REPORTS_TEXT)),
          tags$li(
            HTML(FOOTER_MANDATORY_REPORTS_TEXT),
            tags$ol(
              type = "i",
              tags$li(HTML(FOOTER_EXPEDITED_REPORTS_TEXT)),
              tags$li(HTML(FOOTER_NONEXPEDITED_REPORTS_TEXT))
            )
          ),
          tags$li(HTML(FOOTER_BSR_REPORTS_TEXT))
        )
      ),
      
      # Additional info from config
      tags$div(
        class = "footer-additional-info",
        tags$p(HTML(FOOTER_ADDITIONAL_TEXT))
      )
    )
  )
}

# Function to obtain display name based on file name
obtener_nombre_mostrar <- function(tipo_archivo) {
  # Convert underscores to spaces and capitalize each word
  nombre_base <- gsub("_", " ", tipo_archivo)
  nombre_base <- gsub("\\b(\\w)", "\\U\\1", nombre_base, perl = TRUE)
  
  # Add "Reports by" prefix
  nombre_mostrar <- paste("Reports by", nombre_base)
  
  return(nombre_mostrar)
}




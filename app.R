library(shiny)
library(shinydashboard)  # for valueBox, box components
library(dplyr)
library(tidyr)           # for pivot_wider()
library(ggplot2)
library(DT)

# Define the desired order for table selector and for ordering values
table_order <- c("Report_Type", "Reporter", "Reporter_Region", "Seriousness", "Age_Group", "Sex")

variable_order <- list(
  "Report_Type" = c("Expedited", "Non-Expedited", "Direct", "BSR"),
  "Reporter" = c("Consumer", "Healthcare Professional", "Not Specified", "Other"),
  "Reporter_Region" = c("Domestic", "Foreign", "Not Specified"),
  "Seriousness" = c("Serious", "Death", "Non-Serious"),
  "Age_Group" = c("0-1 Month", "2 Months-2 Years", "3-11 Years", "12-17 Years", "18-64 Years", "65-85 Years", "More than 85 Years", "Not Specified"),
  "Sex" = c("Female", "Male", "Not Specified")
)

ui <- fluidPage(
  # -- Include our custom CSS --
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # -- A top "title bar" in blue, approximating the screenshot's banner --
  div(id = "titleBar",
      "FDA Adverse Event Reporting System (FAERS) Public Dashboard"
  ),

  # -- Spacing under the title bar --
  br(),

  # ---- First Fluid Row: 3 value boxes, blank, table selector ----
  fluidRow(
    column(width = 2, valueBoxOutput("boxTotalReports")),
    column(width = 2, valueBoxOutput("boxSeriousExclDeath")),
    column(width = 2, valueBoxOutput("boxDeathReports")),
    column(width = 3, ""),  # blank space
    column(width = 3,
           selectInput("tableSelect", "Select Table:",
                       choices = character(0))  # updated in server
    )
  ),

  # ---- Second Fluid Row: Year & Variable selectors, plus two action buttons ----
  fluidRow(
    column(width = 3,
           selectInput("yearSelect", "Select Year(s):",
                       choices = NULL, multiple = TRUE, selected = NULL)
    ),
    column(width = 3,
           selectInput("variableSelect", "Select Variable(s):",
                       choices = NULL, multiple = TRUE, selected = NULL)
    ),
    column(width = 2, ""),  # empty column
    column(width = 2,
           actionButton("allYears", "All Years")
    ),
    column(width = 2,
           actionButton("last10Years", "Last 10 Years")
    )
  ),

  # ---- Third Fluid Row: Pivot table (in a scrollable card) and Plot ----
  fluidRow(
    column(width = 6,
           box(
             title = "Pivot Table", width = NULL, status = "primary", solidHeader = TRUE,
             div(style = "overflow-y: auto; max-height: 500px;", dataTableOutput("pivotTable"))
           )
    ),
    column(width = 6,
           box(
             title = "Reports by Report Type", width = NULL, status = "primary", solidHeader = TRUE,
             plotOutput("plotByYearType", height = "450px")
           )
    )
  )
)

server <- function(input, output, session) {
  # ----------------------------------------------------------------------------
  # 1. LOAD THE DATA FROM "data2.csv"
  # ----------------------------------------------------------------------------
  dat <- read.csv("data2.csv", stringsAsFactors = FALSE)

  # ----------------------------------------------------------------------------
  # 2. TOP VALUE BOXES: Filter the full dataset by Year and Variable
  # ----------------------------------------------------------------------------
  filtered_boxes <- reactive({
    temp <- dat
    if (!is.null(input$yearSelect) && length(input$yearSelect) > 0) {
      temp <- temp %>% filter(Year %in% input$yearSelect)
    }
    if (!is.null(input$tableSelect) && !is.null(input$variableSelect) &&
        length(input$variableSelect) > 0) {
      temp <- temp %>% filter(.data[[input$tableSelect]] %in% input$variableSelect)
    }
    temp
  })

  output$boxTotalReports <- renderValueBox({
    total <- nrow(filtered_boxes())
    valueBox(
      formatC(total, format = "d", big.mark = ","),
      span("Total Reports", style = "font-size:6px;"),
      icon = icon("database"),
      color = "blue"
    )
  })

  output$boxSeriousExclDeath <- renderValueBox({
    serious_only <- nrow(filtered_boxes() %>% filter(Seriousness == "Serious"))
    valueBox(
      formatC(serious_only, format = "d", big.mark = ","),
      span("Serious Reports (excl. death)", style = "font-size:6px;"),
      icon = icon("heartbeat"),   # alternative icon
      color = "green"            # using green for "serious" to match screenshot style
    )
  })

  output$boxDeathReports <- renderValueBox({
    deaths <- nrow(filtered_boxes() %>% filter(Seriousness == "Death"))
    valueBox(
      formatC(deaths, format = "d", big.mark = ","),
      span("Death Reports", style = "font-size:6px;"),
      icon = icon("heart-broken"),
      color = "red"
    )
  })

  # ----------------------------------------------------------------------------
  # 3. UPDATE the Table Selector in the Specified Order
  # ----------------------------------------------------------------------------
  observe({
    available_tables <- table_order[table_order %in% names(dat)]
    updateSelectInput(session, "tableSelect", choices = available_tables, selected = available_tables[1])
  })

  # ----------------------------------------------------------------------------
  # 4. UPDATE the Year and Variable Selectors When the Table Selection Changes
  # ----------------------------------------------------------------------------
  observeEvent(input$tableSelect, {
    req(input$tableSelect)
    years <- sort(unique(dat$Year), decreasing = TRUE)
    updateSelectInput(session, "yearSelect", choices = years, selected = NULL)

    vars <- unique(dat[[input$tableSelect]])
    if (input$tableSelect %in% names(variable_order)) {
      desired_order <- variable_order[[input$tableSelect]]
      vars <- intersect(desired_order, vars)
    }
    updateSelectInput(session, "variableSelect", choices = vars, selected = NULL)
  })

  # ----------------------------------------------------------------------------
  # 5. Year Buttons: "All Years" -> no selection; "Last 10 Years" -> top 10
  # ----------------------------------------------------------------------------
  observeEvent(input$allYears, {
    req(input$tableSelect)
    all_years <- sort(unique(dat$Year), decreasing = TRUE)
    updateSelectInput(session, "yearSelect", choices = all_years, selected = NULL)
  })

  observeEvent(input$last10Years, {
    req(input$tableSelect)
    all_years <- sort(unique(dat$Year), decreasing = TRUE)
    if (length(all_years) > 0) {
      last10 <- head(all_years, 10)
      updateSelectInput(session, "yearSelect", choices = all_years, selected = last10)
    }
  })

  # ----------------------------------------------------------------------------
  # 6. FILTER for pivot table & plot
  # ----------------------------------------------------------------------------
  filtered_data <- reactive({
    req(input$tableSelect)
    temp <- dat
    if (!is.null(input$yearSelect) && length(input$yearSelect) > 0) {
      temp <- temp %>% filter(Year %in% input$yearSelect)
    }
    if (!is.null(input$variableSelect) && length(input$variableSelect) > 0) {
      temp <- temp %>% filter(.data[[input$tableSelect]] %in% input$variableSelect)
    }
    temp
  })

  # ----------------------------------------------------------------------------
  # 7. PIVOT TABLE: If exactly 1 year is selected -> group by Year+Quarter
  #                 else -> group by Year
  # ----------------------------------------------------------------------------
  final_table <- reactive({
    req(input$tableSelect)
    # If exactly 1 year selected, group by Year+Quarter
    if (!is.null(input$yearSelect) && length(input$yearSelect) == 1) {
      dt <- filtered_data() %>%
        group_by(Year, Quarter, var = .data[[input$tableSelect]]) %>%
        summarize(Value = n(), .groups = "drop") %>%
        pivot_wider(names_from = var, values_from = Value, values_fill = 0)

      dt <- dt %>%
        mutate(Year = as.character(Year),
               Quarter = as.character(Quarter)) %>%
        arrange(Quarter)

      dt <- dt %>% mutate(`Total Reports` = rowSums(across(-c(Year, Quarter))))

      # Reorder columns if desired
      if (input$tableSelect %in% names(variable_order)) {
        desired_order <- variable_order[[input$tableSelect]]
        current_vars <- setdiff(names(dt), c("Year", "Quarter", "Total Reports"))
        ordered_vars <- intersect(desired_order, current_vars)
        remaining_vars <- setdiff(current_vars, ordered_vars)
        dt <- dt %>% select(Year, Quarter, `Total Reports`, all_of(ordered_vars), all_of(remaining_vars))
      } else {
        dt <- dt %>% select(Year, Quarter, `Total Reports`, everything())
      }

      totals <- dt %>%
        select(-c(Year, Quarter)) %>%
        summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
        mutate(Year = "Total Reports", Quarter = "") %>%
        select(Year, Quarter, everything())

      dt <- bind_rows(totals, dt)
    } else {
      # Otherwise, group by Year only
      dt <- filtered_data() %>%
        group_by(Year, var = .data[[input$tableSelect]]) %>%
        summarize(Value = n(), .groups = "drop") %>%
        pivot_wider(names_from = var, values_from = Value, values_fill = 0)

      dt <- dt %>%
        mutate(Year = as.character(Year)) %>%
        arrange(desc(as.integer(Year)))

      dt <- dt %>% mutate(`Total Reports` = rowSums(across(-Year)))

      # Reorder columns if desired
      if (input$tableSelect %in% names(variable_order)) {
        desired_order <- variable_order[[input$tableSelect]]
        current_vars <- setdiff(names(dt), c("Year", "Total Reports"))
        ordered_vars <- intersect(desired_order, current_vars)
        remaining_vars <- setdiff(current_vars, ordered_vars)
        dt <- dt %>% select(Year, `Total Reports`, all_of(ordered_vars), all_of(remaining_vars))
      } else {
        dt <- dt %>% select(Year, `Total Reports`, everything())
      }

      totals <- dt %>%
        select(-Year) %>%
        summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
        mutate(Year = "Total Reports") %>%
        select(Year, everything())

      dt <- bind_rows(totals, dt)
    }
    dt
  })

  # ----------------------------------------------------------------------------
  # 8. RENDER THE PIVOT TABLE
  # ----------------------------------------------------------------------------
  output$pivotTable <- renderDataTable({
    dt <- final_table()
    numeric_cols <- names(dt)[sapply(dt, is.numeric)]
    datatable(
      dt,
      options = list(paging = FALSE, searching = FALSE, autoWidth = TRUE),
      rownames = FALSE,
      escape = FALSE
    ) %>%
      formatStyle(
        columns = names(dt),
        valueColumns = "Year",
        target = "row",
        fontWeight = styleEqual("Total Reports", "bold")
      ) %>%
      formatCurrency(
        columns = numeric_cols,
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      )
  })

  # ----------------------------------------------------------------------------
  # 9. RENDER THE PLOT: If exactly 1 year selected -> Quarter on x-axis
  #                     otherwise -> Year on x-axis
  # ----------------------------------------------------------------------------
  output$plotByYearType <- renderPlot({
    if (!is.null(input$yearSelect) && length(input$yearSelect) == 1) {
      plot_data <- filtered_data() %>%
        group_by(Year, Quarter, var = .data[[input$tableSelect]]) %>%
        summarize(Value = n(), .groups = "drop")

      ggplot(plot_data, aes(x = factor(Quarter, levels = c("Q1","Q2","Q3","Q4")),
                            y = Value, fill = var)) +
        geom_bar(stat = "identity") +
        labs(x = "Quarter", y = "Number of Reports", fill = input$tableSelect,
             title = paste("Quarterly Data for Year", input$yearSelect)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      plot_data <- filtered_data() %>%
        group_by(Year, var = .data[[input$tableSelect]]) %>%
        summarize(Value = n(), .groups = "drop")

      ggplot(plot_data, aes(x = factor(Year), y = Value, fill = var)) +
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "Number of Reports", fill = input$tableSelect) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui, server)

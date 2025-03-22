box::use(
  dplyr[arrange, desc, n, summarise],
  shiny.fluent[
    Dropdown.shinyInput,
    FontIcon,
    Separator,
    Slider.shinyInput,
    Stack,
    Text,
  ],
  shiny[div, moduleServer, NS, reactive, req, tagList, uiOutput],
  tidyr[pivot_wider],
)

box::use(
  app / logic / data[mock_data, years],
  app / view / home / chart_module,
  app / view / home / table_module,
)


#' UI for table module
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "ms-Grid",
      # TOP ROW: KPI (left) + Filters (right)
      div(
        class = "ms-Grid-row",
        style = "display: flex; margin-top: 10px;",
        # Left side: KPI row
        div(
          class = "ms-Grid-col ms-sm6",
          div(
            style = "display: flex; gap: 20px; height: 100%;",
            # KPI 1
            Stack(
              style = list(
                border = "1px solid #ccc",
                padding = "10px",
                width = "150px"
              ),
              Text(variant = "large", "Total Reports"),
              uiOutput(ns("kpi_total"))
            ),
            # KPI 2
            Stack(
              style = list(
                border = "1px solid #ccc",
                padding = "10px",
                width = "150px"
              ),
              Text(variant = "large", "Serious"),
              uiOutput(ns("kpi_serious"))
            ),
            # KPI 3
            Stack(
              style = list(
                border = "1px solid #ccc",
                padding = "10px",
                width = "150px"
              ),
              Text(variant = "large", "Death"),
              uiOutput(ns("kpi_death"))
            )
          )
        ),
        # Right side: Filters
        div(
          class = "ms-Grid-col ms-sm6",
          # Dimension dropdown
          Dropdown.shinyInput(
            inputId = ns("dimension_dropdown"),
            label = "Dimension",
            value = "report_type",
            options = lapply(
              c(
                "report_type",
                "reporter",
                "region",
                "seriousness",
                "age_group",
                "sex"
              ),
              function(x) list(key = x, text = x)
            ),
            style = list(width = 180)
          ),
          # Slider for years
          Slider.shinyInput(
            inputId = ns("year_range"),
            label = "Starting Year",
            min = min(years),
            max = max(years),
            value = min(years),
            step = 1
          )
        )
      ),

      Separator(),

      # SECOND ROW: table (left) + chart (right)
      div(
        class = "ms-Grid-row",
        style = "display: flex;",
        # Table in a card
        div(
          class = "ms-Grid-col ms-sm6",
          table_module$ui(ns("table")),
          style = "max-height: 600px; overflow-y: auto;"
        ),
        # Chart in a card
        div(
          class = "ms-Grid-col ms-sm6",
          chart_module$ui(ns("chart")),
          style = "height: 500px;"
        ),
      )
    )
  )
}

#' Server logic for table module
#' @export
server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    # Reactive dimension
    dimension_r <- reactive({
      input$dimension_dropdown
    })

    # Reactive year range
    year_range_r <- reactive({
      input$year_range
    })

    # Filter data
    filtered_data <- reactive({
      req(year_range_r())
      rng <- year_range_r()
      mock_data[mock_data$year >= rng, ]
    })

    aggregated_data <- reactive({
      filtered_data() |>
        summarise(count = n(), .by = c(year, dimension_r())) |>
        pivot_wider(names_from = dimension_r(), values_from = count) |>
        arrange(desc(year))
    })

    # Update KPI text
    output$kpi_total <- shiny::renderUI({
      value <- nrow(filtered_data())
      div(
        style = "display:flex;",
        FontIcon(
          iconName = "Chart",
          style = list(`font-size` = "30px", color = "black")
        ),
        Text(value, variant = "xxLarge", style = list(color = "black"))
      )
    })
    output$kpi_serious <- shiny::renderUI({
      div(
        style = "display:flex;",
        FontIcon(
          iconName = "WarningSolid",
          style = list(`font-size` = "30px", color = "yellow")
        ),
        Text(
          sum(filtered_data()$seriousness == "serious"),
          variant = "xxLarge",
          style = list(color = "yellow")
        )
      )
    })
    output$kpi_death <- shiny::renderUI({
      div(
        style = "display:flex;",
        FontIcon(
          iconName = "StatusErrorFull",
          style = list(`font-size` = "30px", color = "red")
        ),
        Text(
          sum(filtered_data()$seriousness == "death"),
          variant = "xxLarge",
          style = list(color = "red")
        )
      )
    })

    # Table module
    table_module$server(
      id = "table",
      aggregated_data = aggregated_data
    )

    # Chart module
    chart_module$server(
      id = "chart",
      aggregated_data = aggregated_data
    )
  })
}

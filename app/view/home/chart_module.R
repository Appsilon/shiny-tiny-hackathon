box::use(
  dplyr[all_of],
  ggplot2[aes, geom_bar, ggplot, labs, theme_minimal],
  shiny[moduleServer, NS, plotOutput, renderPlot],
  tidyr[pivot_longer],
)

#' UI for the chart module
#' @export
ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("bar_chart"), height = "600px")
}

#' Server for the chart module
#' @export
server <- function(id, aggregated_data) {
  moduleServer(id, function(input, output, session) {
    output$bar_chart <- renderPlot({
      cols_to_pivot <- names(aggregated_data())[-1]
      agg <- aggregated_data() |>
        pivot_longer(all_of(cols_to_pivot))

      ggplot(agg, aes(x = as.factor(year), y = value, fill = name)) +
        geom_bar(stat = "identity") +
        labs(x = "Years", y = "Values", title = "Stacked Bar Plot in ggplot2") +
        theme_minimal()
    })
  })
}

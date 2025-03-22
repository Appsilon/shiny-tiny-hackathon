box::use(
  shiny.fluent[DetailsList],
  shiny[moduleServer, NS, renderUI, uiOutput],
)

#' UI for the table module
#' @export
ui <- function(id) {
  ns <- NS(id)
  # Return a placeholder UI; the actual DetailsList is rendered in renderUI
  uiOutput(ns("details_list"))
}

#' Server for the table module
#' @param filtered_data reactive data frame
#' @param dimension reactive string for dimension name
#' @export
server <- function(id, aggregated_data) {
  moduleServer(id, function(input, output, session) {
    output$details_list <- renderUI({
      df <- aggregated_data()
      items_list <- apply(df, 1, as.list)
      columns_list <- lapply(names(df), function(col_name) {
        list(
          key = col_name,
          name = col_name,
          fieldName = col_name,
          minWidth = 80,
          maxWidth = 150,
          isResizable = TRUE
        )
      })
      DetailsList(
        items = items_list,
        columns = columns_list,
        compact = TRUE,
        style = list(
          height = "100%"
        )
      )
    })
  })
}

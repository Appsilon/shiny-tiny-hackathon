box::use(
  shiny,
  bslib,
  reactable,
)

box::use(
  app / logic / table,
)



#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    reactable$reactableOutput(ns("table"))
  )
}


#' @export
server <- function(id, df) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table <- reactable$renderReactable({
      shiny$req(df())
      data <- df()
      shiny$validate(
        shiny$need(nrow(df() > 0), "No records found for selected filters.")
      )
      coldefs <- table$fda_coldefs(data)
      table$fda_table(data, coldefs)
    })

  })
}

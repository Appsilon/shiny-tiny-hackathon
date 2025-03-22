box::use(
  shiny,
  bslib,
  reactable,
  highcharter,
)

box::use(
  app / logic / hc,
)



#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    highcharter$highchartOutput(ns("stackedChart"))
  )
}


#' @export
server <- function(id, df) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$stackedChart <- highcharter$renderHighchart({
      shiny$req(df())
      print(nrow(df()))
      shiny$validate(
        shiny$need(nrow(df() > 0), "No records found for selected filters.")
      )
      hc$create_stock_chart(df())
    })

  })
}

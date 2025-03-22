box::use(
  shiny,
)

box::use(
  app / view / homepage,
)
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$useBusyIndicators(spinners = FALSE),
    shiny$busyIndicatorOptions(pulse_height="0.4rem",
                               pulse_background = "linear-gradient(135deg,
                                                   #459ede, #23587c,
                                                        #a0d1f7);"),
    homepage$ui(ns("homepage"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {

    homepage$server("homepage")

  })
}

box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[moduleServer, NS],
)

box::use(
  app / view / home / home,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Use CommandBar to mimic the top horizontal bar
  fluentPage(
    Pivot(
      PivotItem(
        headerText = "Home",
        home$ui(id = ns("home"))
      ),
      # SEARCH tab (placeholder)
      PivotItem(
        headerText = "Search",
        # You can call another module if you have one, e.g. search$ui(...)
        "Search page content goes here..."
      ),
      # DISCLAIMER tab (placeholder)
      PivotItem(
        headerText = "Disclaimer",
        "Disclaimer page content goes here..."
      ),
      # REPORT PROBLEM tab (placeholder)
      PivotItem(
        headerText = "Report a Problem",
        "Report a Problem page content goes here..."
      ),
      # PRINT tab (placeholder)
      PivotItem(
        headerText = "Print",
        "Print page content goes here..."
      ),
      # FEEDBACK tab (placeholder)
      PivotItem(
        headerText = "Site Feedback",
        "Site Feedback page content goes here..."
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    home$server("home")
  })
}

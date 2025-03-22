# -------------------
#  MODULE UI
# -------------------
fdaUI <- function(id) {
  ns <- NS(id)

  tagList(

    bslib::card(
      div(
        id = ns("settings"),
        class = "settings"

      )
    )
  )
}

# -------------------
#  MODULE SERVER
# -------------------
fdaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

 })
}

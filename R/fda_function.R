show_modal <- function() {

  showModal(modalDialog(
    title = "Disclaimer",
    size = "xl",
    tags$img(src = "img/disclaimer.png",
             class = "disclaimer",
             alt = "disclaimer",
             width = "90%"
    ),
    easyClose = FALSE,
    footer = modalButton("Accept")
  ))
}

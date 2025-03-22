
show_disclaimer <- function() {
    showModal(modalDialog(
        title = NULL,
        footer = NULL,
        easyClose = FALSE,
        htmlTemplate("www/html/disclaimer.html"),
        div(
            class = "text-center mt-3",
            actionButton(
                inputId = "accept_disclaimer", 
                label = "Accept", 
                class = "btn-primary"
            ),
            actionButton(
                inputId = "do_not_accept", 
                label = "Do Not Accept", 
                class = "btn-outline-secondary ml-2"
            )
        ),
        size = "l"
    ))
}
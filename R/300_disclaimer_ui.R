
disclaimer_ui <- function(id) {

    ns <- NS(id)

    tagList(
        htmlTemplate("www/html/disclaimer.html"),
        div(
            class = "d-flex justify-content-center",
            checkboxInput(
                inputId = ns("checkbox"),
                label = "I have read and understand the disclaimer.",
                value = FALSE,
                width = "100%"
            )
        ),
        div(
            class = "text-center mt-3",
            shinyjs::disabled(actionButton(
                inputId = ns("accept_disclaimer"), 
                label = "Accept", 
                class = "btn-primary"
            )),
            actionButton(
                inputId = ns("do_not_accept"), 
                label = "Do Not Accept", 
                class = "btn-outline-secondary ml-2"
            )
        )
    )

}
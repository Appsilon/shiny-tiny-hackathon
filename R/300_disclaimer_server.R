
disclaimer_server <- function(
    id
) { moduleServer(id, function(input, output, session) {

    # ------ * Enable/disable accept disclaimer button ------------------------
    observeEvent(input$checkbox, {
        shinyjs::toggleState("accept_disclaimer", condition = input$checkbox)
    })

    # ------ * Accept disclaimer -----------------------------------------------
    observeEvent(input$accept_disclaimer, {
        session$userData$disclaimer_accepted <- TRUE
        removeModal()
    })

    # ------ * Do not accept disclaimer ----------------------------------------
    observeEvent(input$do_not_accept, {
        # Redirect to FDA website
        redirect_url <- paste0(
            "https://www.fda.gov/drugs/fdas-adverse-event-reporting-system-faers/", 
            "fda-adverse-event-reporting-system-faers-public-dashboard"
        )
        session$sendCustomMessage("redirect", redirect_url)
    })

})}

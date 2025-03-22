
function(input, output, session) {

    # ------ INITIALIZE --------------------------------------------------------
    # Initialize session variables
    session$userData$disclaimer_accepted <- FALSE

    # ------ MODULES -----------------------------------------------------------

    # ------ * Disclaimer ------------------------------------------------------
    if (Sys.getenv("HIDE_DISCLAIMER") != "true") {
        showModal(modalDialog(
            title = NULL,
            footer = NULL,
            easyClose = FALSE,
            size = "l",
            disclaimer_ui("disclaimer")
        ))
        disclaimer_server("disclaimer")
    }

    # ------ * Home ------------------------------------------------------------
    home_server("home")

}

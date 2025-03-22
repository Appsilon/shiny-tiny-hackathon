
bslib::page_fluid(
    title = "Shiny App",
    theme = bslib::bs_theme(
        version = 5,
        primary = "#023364"
    ),
    shinyjs::useShinyjs(),
    includeCSS("www/css/main.min.css"),
    includeScript("www/js/shiny_custom_message.js"),
    fluidRow(
        column(
            width = 12,
            class = "header",
            tags$img(
                src = "img/logo.png",
                alt = "Logo Data Champ’",
                width = "50px"
            ),
            h3("FDA Adverse Events Reporting System (FAERS) Public Dashboard")
        )
    ),
    home_ui("home")
)

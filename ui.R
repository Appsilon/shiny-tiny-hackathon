
bslib::page_fixed(
    title = "Shiny App",
    theme = bslib::bs_theme(
        version = 5
    ),
    shinyjs::useShinyjs(),
    includeCSS("www/css/main.min.css"),
    "Coucou"
)

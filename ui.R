
bslib::page_fluid(
    title = "Shiny App",
    theme = bslib::bs_theme(
        version = 5
    ),
    shinyjs::useShinyjs(),
    includeCSS("www/css/main.min.css"),
    includeScript("www/js/shiny_custom_message.js"),
    h1("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
    bslib::navset_bar(
        id = "menu",
        bslib::nav_panel(
            title = "Home",
            id = "home",
            value = "home",
            home_ui("home")
        ),
        bslib::nav_panel(
            title = bsicons::bs_icon("search", title = "Search"),
            id = "search",
            value = "search",
            search_ui("search")
        )
    )
)

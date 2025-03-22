library(shiny)

ui <- fluidPage(
  theme = "css/main.css",
  tags$head(
    div(
      class = "titlepanel",
      id = "titlepanel",
      "FDA Adverse Event Reporting System (FAERS) Public Dashboard",
      tags$img(src = "img/fda.webp",
               class = "logo",
               alt = "us food and drug administration logo",
      )
    )
  ),
  div(
    class = "header-container",
    # Internal navigation: use actionButton + observeEvent in server
    actionButton("home", "Home", class = "btn btn-outline-light"),
    actionButton("search", "Search", class = "btn btn-outline-light"),
    actionButton("disclaimer", "Disclaimer", class = "btn btn-outline-light"),

    # External links: styled as buttons, open in new tabs/windows
    tags$a(
      "Report a Problem",
      class = "btn btn-outline-light",
      href = "https://www.accessdata.fda.gov/scripts/medwatch/index.cfm?action=reporting.home",
      target = "_blank"
    ),
    tags$a(
      "FAQ",
      class = "btn btn-outline-light",
      href = "https://fis.fda.gov/extensions/FPD-FAQ/FPD-FAQ.html",
      target = "_blank"
    ),
    tags$a(
      "Site Feedback",
      class = "btn btn-outline-light",
      href = "mailto:someone@example.com?subject=Site%20Feedback"
    )
  ),
  # Main content: tabsetPanel for Home and Search
  tabsetPanel(
    id = "main_tabs",
    type = "hidden",

    tabPanel("Home",
             value = "home",
             hidden = TRUE,
             fdaUI("fda")
    ),
    tabPanel("Search", value = "search",
             h3("Search Page"),
             textInput("searchText", "Enter search query:"),
             actionButton("doSearch", "Search")
    )
  ),
  tags$img(src = "img/footer.png",
           class = "logo",
           alt = "us food and drug administration logo",
           width = "90%"
  )
)

server <- function(input, output, session) {

  fdaServer("fda")

  sass::sass(
    input = sass::sass_file("www/scss/main.scss"),
    output = "www/css/main.css"
  )

  # button clicks switch tabs accordingly
  observeEvent(input$home, {
    updateTabsetPanel(session, "main_tabs", selected = "home")
  })
  observeEvent(input$search, {
    updateTabsetPanel(session, "main_tabs", selected = "search")
  })

  # disclaimer
  observe({
    show_modal()
  })

  observeEvent(input$disclaimer, {
    show_modal()
  })
}

shinyApp(ui, server)

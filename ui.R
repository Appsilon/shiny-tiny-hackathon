require(shiny)
require(reactable)

ui <- navbarPage(
  
  tags$head(
    tags$style(HTML("
      .topbar {
        background-color: #005ea2;
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 24px;
        color: white;
      }

      .topbar-title {
        font-weight: bold;
        font-size: 16px;
      }

      .topbar-links {
        display: flex;
        gap: 16px;
      }

      .topbar-links a {
        color: white;
        text-decoration: none;
        font-size: 14px;
        font-weight: 400;
      }

      .topbar-links a:hover {
        text-decoration: underline;
      }

      .nav-tabs {
        margin-left: auto;
        margin-right: 16px;
      }
    "))
  ),
  
  title = div(
    
    class = "navbar-brand-container",
    div("FDA Adverse Events Reporting System (FAERS) Public Dashboard", class = "navbar-brand-text"),
    div(class = "navbar-links",
        actionLink("show_disclaimer", "Disclaimer", style = "color: white; margin-left: 15px;"),
        tags$a("Report a problem", href = "https://www.accessdata.fda.gov/scripts/medwatch/index.cfm?action=reporting.home", target = "_blank"),
        tags$a("FAQ", href = "https://fis.fda.gov/extensions/FPD-FAQ/FPD-FAQ.html", target = "_blank"),
        tags$a(
          "Site feedback",
          href = "mailto:DrugInfo@fda.hhs.gov?subject=FAERS%20Public%20Dashboard%20Feedback",
          style = "color: white; margin-left: 15px;"
        )
    )
    
  ),
  
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tabPanel("Home",
           fluidPage(
             tags$head(
               tags$style(HTML("
          .navbar {
            background-color: #005ea2;
          }

          .navbar-default .navbar-nav > li > a {
            color: white;
            font-weight: 500;
          }

          .navbar-default .navbar-brand {
            color: white;
            font-weight: bold;
          }

          .navbar .navbar-nav {
            margin-left: auto;
          }
          
          .navbar .navbar-header {
            flex-grow: 1;
            display: flex;
            align-items: center;
          }

          .metric-tile {
            padding: 15px;
            border-radius: 8px;
            color: white;
            font-weight: bold;
            font-size: 18px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }

          .metric-title {
            font-size: 16px;
          }

          .metric-value {
            font-size: 28px;
            margin-top: 5px;
          }

          .total-tile {
            background-color: #005ea2;
          }

          .serious-tile {
            background-color: #f6a400;
          }

          .death-tile {
            background-color: #d7191c;
          }
          
          .navbar-brand-container {
            display: flex;
            justify-content: space-between;
            align-items: center;
            width: 100%;
          }
          
          .navbar-brand-text {
            color: white;
            font-weight: bold;
            font-size: 16px;
          }
          
          .navbar-links a {
            color: white;
            margin-left: 15px;
            font-weight: 400;
            font-size: 14px;
            text-decoration: none;
          }
          
          .navbar-links a:hover {
            text-decoration: underline;
          }

        "))
             ),
             
             fluidRow(
               column(4, div(class = "metric-tile total-tile",
                             div("Total reports", class = "metric-title"),
                             div(textOutput("total_reports"), class = "metric-value")
               )),
               column(4, div(class = "metric-tile serious-tile",
                             div("Serious reports", class = "metric-title"),
                             div(textOutput("serious_reports"), class = "metric-value")
               )),
               column(4, div(class = "metric-tile death-tile",
                             div("Death reports", class = "metric-title"),
                             div(textOutput("death_reports"), class = "metric-value")
               ))
             ),
             
             fluidRow(
               column(6,
                      h4("Reports received by report type"),
                      radioButtons(
                        inputId = "year_range",
                        label = NULL,
                        choices = c("All years", "Last 10 years"),
                        selected = "All years",
                        inline = TRUE
                      )
               ),
               column(6,
                      div(style = "display: flex; justify-content: flex-end; align-items: right; height: 100%;",
                          selectInput(
                            inputId = "group_type",
                            label = "Select report grouping:",
                            choices = c(
                              "Report type" = "type",
                              "Reporter" = "reporter",
                              "Reporter region" = "region",
                              "Report seriousness" = "seriousness",
                              "Age group" = "age",
                              "Sex" = "sex"
                            ),
                            selected = "type",
                            width = "50%"
                          )
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      div(style = "overflow-x: auto;", reactableOutput("report_table"))
               ),
               column(6, plotOutput("report_plot", height = "525px"))
             )
           )
  ),
  
  tabPanel("Search", h4("Search functionality coming soon..."))
)
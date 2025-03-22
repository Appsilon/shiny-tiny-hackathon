# ui.R
#Install below packages before running the application
#install.packages(c("shiny", "shinydashboard", "shinyWidgets", "highcharter", "reactable", "DT", "plotly", "htmltools", "scales", "readxl", "dplyr","renv"))

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(highcharter)
library(reactable)
library(DT)
library(plotly)
library(htmltools)
library(scales)

# Define UI
ui <- fluidPage(
  # Custom CSS
  tags$head(
    # Include external CSS file from www folder
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Font Awesome for icons (if not already included)
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ), 
  # Header
  div(
    class = "navbar-custom",
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        h2("FDA Adverse Events Reporting System (FAERS) Public Dashboard", 
           style = "color: white; margin: 0;")
      ),
      div(
        icon("stethoscope", class = "fa-lg", lib = "font-awesome")
      )
    ),
    
    # Navigation bar
    div(
      style = "margin-top: 10px;",
      actionLink("home_link", "Home", class = "nav-link", style = "color: white; margin-right: 15px;"),
      actionLink("search_link", "Search", class = "nav-link", style = "color: white; margin-right: 15px;"),
      div(
        class = "right-nav",
        style = "float: right;",
        actionLink("disclaimer_link", "Disclaimer", class = "nav-link", style = "color: white; margin-right: 15px;"),
        actionLink("report_problem_link", "Report a Problem", class = "nav-link", style = "color: white; margin-right: 15px;"),
        actionLink("faq_link", "FAQ", class = "nav-link", style = "color: white; margin-right: 15px;"),
        actionLink("site_feedback_link", "Site Feedback", class = "nav-link", style = "color: white;")
      )
    )
  ),
  
  # Main content
  div(
    # Summary statistics
    fluidRow(
      column(4,
             div(
               class = "info-box",
               div(
                 style = "display: flex; align-items: center;",
                 div(style = "font-size: 24px; margin-right: 10px;", icon("chart-line")),
                 div(
                   p("Total Reports", style = "margin: 0; color: #666;"),
                   p(textOutput("total_reports_text"), style = "font-size: 24px; font-weight: bold; margin: 0;")
                 )
               )
             )
      ),
      column(4,
             div(
               class = "info-box info-box-warning",
               div(
                 style = "display: flex; align-items: center;",
                 div(style = "font-size: 24px; margin-right: 10px; color: #f0ad4e;", icon("exclamation-triangle")),
                 div(
                   p("Serious Reports (excluding death)", style = "margin: 0; color: #666;"),
                   p(textOutput("serious_reports_text"), style = "font-size: 24px; font-weight: bold; margin: 0;")
                 )
               )
             )
      ),
      column(4,
             div(
               class = "info-box info-box-danger",
               div(
                 style = "display: flex; align-items: center;",
                 div(style = "font-size: 24px; margin-right: 10px; color: #d9534f;", icon("skull-crossbones")),
                 div(
                   p("Death Reports", style = "margin: 0; color: #666;"),
                   p(textOutput("death_reports_text"), style = "font-size: 24px; font-weight: bold; margin: 0;")
                 )
               )
             )
      )
    ),
    
    # # Time period selector
    # fluidRow(
    #   column(12,
    #          div(
    #            class = "filter-container",
    #            selectInput("report_type_filter", "Reports by Report Type", 
    #                        choices = c("All"), 
    #                        selected = "All", width = "200px"),
    #            div(
    #              class = "period-buttons",
    #              actionButton("all_years", "All Years", 
    #                           class = "period-btn"),
    #              actionButton("last_10_years", "Last 10 Years", 
    #                           class = "period-btn")
    #            )
    #          )
    #   )
    # ),
    
    # Reports by Report Type
    fluidRow(
      column(6,
             h4("Reports received by Report Type"),
             div(
               class = "search-container",
               textInput("year_filter", "Year", width = "100px")
             ),
             div(
               class = "search-container",
               textInput("report_type_search", "Report Type", width = "100px")
             ),
             DTOutput("reports_table")
      ),
      column(6,
             h4("Reports received by Report Type"),
             plotlyOutput("reports_plot", height = "400px")
      )
    ),
    
    # Footer
    div(
      class = "footer",
      p("Data as of December 31, 2024"),
      p("This page displays the number of adverse event reports received by FDA for drugs and therapeutic biologic products by the following Report Types."),
      tags$ul(
        tags$li("Direct Reports are voluntarily submitted directly to FDA through the MedWatch program by consumers and healthcare professionals."),
        tags$li("Mandatory Reports are submitted by manufacturers and are categorized as:")
      ),
      div(
        style = "text-align: right;",
        a("Vulnerability Disclosure Policy", href = "#")
      )
    ),
    
    # Hidden sections for FAQ and Disclaimer
    # FAQ and Disclaimer sections are hidden until accessed
    div(id = "faq", uiOutput("faq_content"), style = "display: none;"),
    div(id = "disclaimer", uiOutput("disclaimer_content"), style = "display: none;")
  )
)
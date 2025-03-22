require(shiny)
require(tidyverse)
require(ggplot2)
require(reactable)
require(reactable.extras)
require(scales)

report_data <- read_csv("data/mock_report_data.csv")

server <- function(input, output, session) {
  
  load_summary_data <- reactive({
    
    type <- input$group_type
    
    filename <- switch(type,
                       "type" = "mock_total_summary.csv",
                       "sex" = "sex_summary.csv",
                       "age" = "age_summary.csv",
                       "seriousness" = "seriousness_summary.csv",
                       "region" = "region_summary.csv",
                       "reporter" = "reporter_summary.csv")
    
    read_csv(glue::glue("data/{filename}"))
    
  })
  
  filtered_summary_data <- reactive({
    
    df <- load_summary_data()
    
    if (input$year_range == "Last 10 years") {
      df <- df %>% filter(Year >= max(Year) - 9)
    }
    
    df
    
  })
  
  load_plot_data <- reactive({
    
    type <- input$group_type
    
    filename <- switch(type,
                       "type" = "mock_report_data.csv",
                       "sex" = "sex_data.csv",
                       "age" = "age_data.csv",
                       "seriousness" = "seriousness_data.csv",
                       "region" = "region_data.csv",
                       "reporter" = "reporter_data.csv")
    
    read_csv(glue::glue("data/{filename}"))
    
  })
  
  get_pretty_number <- function(x) {
    
    format(x, big.mark = ",", scientific = FALSE)
    
  }
  
  output$total_reports <- renderText({
    
    df <- filtered_summary_data()
    get_pretty_number(sum(df$TotalReports))
    
  })
  
  output$serious_reports <- renderText({
    
    df <- filtered_summary_data()
    
    if ("SeriousReports" %in% names(df)) {
      get_pretty_number(sum(df$SeriousReports))
    } else {
      "0"
    }
    
  })
  
  output$death_reports <- renderText({
    
    df <- filtered_summary_data()
    
    if ("DeathReports" %in% names(df)) {
      get_pretty_number(sum(df$DeathReports))
    } else {
      "0"
    }
    
  })
  
  output$report_plot <- renderPlot({
    
    summary_df <- filtered_summary_data()
    plot_df <- load_plot_data()
    
    grouping_column <- switch(input$group_type,
                              "type" = "ReportType",
                              "sex" = "Sex",
                              "age" = "AgeGroup",
                              "seriousness" = "Seriousness",
                              "region" = "Region",
                              "reporter" = "Reporter")
    
    ggplot(
      plot_df %>% filter(Year %in% summary_df$Year),
      aes(x = factor(Year), y = Count, fill = .data[[grouping_column]])
    ) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$report_table <- renderReactable({
    
    df <- filtered_summary_data()
    
    column_defs <- list(
      Year = colDef(name = "Year")
    )
    
    for (col in setdiff(names(df), "Year")) {
      column_defs[[col]] <- colDef(
        name = gsub("([a-z])([A-Z])", "\\1 \\2", col),
        format = colFormat(separators = TRUE)
      )
    }
    
    reactable(
      df,
      defaultSorted = list(Year = "desc"),
      filterable = FALSE,
      pagination = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "#f7f7f8", fontWeight = "bold")
      ),
      columns = column_defs
    )
    
  })
  
  observeEvent(input$show_disclaimer, {
    
    showModal(modalDialog(
      title = "Disclaimer",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      tagList(
        p("Each year, the FDA receives over one million adverse event and medication error reports..."),
        p("Although these reports are a valuable source of information, this surveillance system has limitations..."),
        tags$ul(
          tags$li("Consumers should not stop or change medication..."),
          tags$li("The FAERS web search feature is limited..."),
          tags$li("Data submitted to the FAERS system..."),
          tags$li("FAERS data alone cannot be used..."),
          tags$li("Confirming whether a drug product actually caused..."),
          tags$li("FAERS data do not represent all known safety information..."),
          tags$li("Variations in trade, product, and company names...")
        ),
        p("Importantly, safety reports submitted to FDA do not necessarily reflect a conclusion by FDA..."),
        tags$a(
          href = "https://www.fda.gov/regulatoryinformation/foi/howtomakeafoiarequest/default.htm",
          target = "_blank",
          "Freedom of Information Act (FOIA) request site"
        )
      )
    ))
  })
  
}

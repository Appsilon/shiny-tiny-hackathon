# server.R - Server logic
server <- function(input, output, session) {

  # Filtered data based on user inputs
  filtered_data <- reactive({
    data <- report_data

    # Apply year filter
    data <- data %>% filter(year >= input$yearFilter[1] & year <= input$yearFilter[2])

    # Apply report type filter if any selected
    if (!is.null(input$typeFilter)) {
      data <- data %>% filter(report_type %in% input$typeFilter)
    }

    return(data)
  })

  # For the table, we need to pivot the data to have report types as columns
  table_data <- reactive({
    data <- filtered_data()

    # Optionally aggregate by decade if too many years selected
    if (input$yearFilter[2] - input$yearFilter[1] > 20) {
      data <- data %>%
        mutate(decade = floor(year / 10) * 10) %>%
        group_by(decade, report_type) %>%
        summarize(report_count = sum(report_count), .groups = "drop") %>%
        rename(year = decade) %>%
        mutate(year = paste0(year, "s"))
    }

    # Pivot the data to have report types as columns
    pivoted_data <- data %>%
      pivot_wider(
        names_from = report_type,
        values_from = report_count,
        values_fill = list(report_count = 0)
      ) %>%
      mutate(Total = rowSums(select(., -year)))

    # Reorder columns to put Total first
    pivoted_data <- pivoted_data %>%
      select(year, Total, everything())

    # Convert column names to title case
    names(pivoted_data) <- str_to_title(names(pivoted_data))

    return(pivoted_data)
  })

  # Total reports count
  output$totalReports <- renderText({
    total <- sum(filtered_data()$report_count)
    format(total, big.mark = ",")
  })

  # Reset button handler
  observeEvent(input$resetFilters, {
    updateSliderInput(session, "yearFilter",
                      value = c(min(report_data$year), max(report_data$year)))
    updateCheckboxGroupInput(session, "typeFilter",
                             selected = levels(report_data$report_type))
  })

  # Render the data table with columns for report types
  output$reportTable <- renderDT({
    datatable(table_data(),
              options = list(
                pageLength = 10,
                lengthMenu = c(5, 10, 15, 20),
                searching = FALSE,
                order = list(list(0, 'desc')),
                dom = 'tlip'
              ),
              rownames = FALSE) %>%
      formatRound(columns = which(sapply(table_data(), is.numeric)),
                  digits = 0, mark = ",")
  })

  # Render the bar plot
  output$barPlot <- renderPlot({
    req(nrow(filtered_data()) > 0)

    # If too many years selected, aggregate by decades for better visualization
    plot_data <- filtered_data()
    if (input$yearFilter[2] - input$yearFilter[1] > 20) {
      plot_data <- plot_data %>%
        mutate(decade = floor(year / 10) * 10) %>%
        group_by(decade, report_type) %>%
        summarize(report_count = sum(report_count), .groups = "drop") %>%
        rename(year = decade) %>%
        mutate(year = factor(paste0(year, "s")))
    } else {
      plot_data <- plot_data %>%
        mutate(year = factor(year))
    }

    ggplot(plot_data, aes(x = year, y = report_count, fill = report_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Reports received by Report Type",
           x = NULL,
           y = "Report Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1", name = NULL) +
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
}

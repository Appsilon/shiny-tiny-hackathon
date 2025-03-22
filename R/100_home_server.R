
home_server <- function(
    id
) { moduleServer(id, function(input, output, session) {

    # ------ REACTIVE ----------------------------------------------------------

    # ------ * data_wide -------------------------------------------------------
    # Transform data in wide format for the table
    data_wide <- reactive({
        data_wide <- data_list[[input$report_category]] |>
            data.table::dcast(
                formula = as.formula(paste0("year ~ ", input$report_category)),
                value.var = "count"
            )
        data_wide[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = -"year"]
        total_row <- data.table(year = NA_real_)
        for (col in setdiff(names(data_wide), "year")) {
            total_row[, (col) := sum(data_wide[[col]], na.rm = TRUE)]
        }
        data_wide <- rbind(data_wide, total_row)
        setcolorder(data_wide, "year")
        setorder(data_wide, -year)
        data_wide
    })

    # ------ OUTPUT ------------------------------------------------------------

    # ------ * Table -----------------------------------------------------------
    output$table <- DT::renderDT({
        data_wide()
    })

    # ------ * Plot ------------------------------------------------------------
    output$plot <- ggiraph::renderGirafe({
        data <- data_list[[input$report_category]]
        p <- ggplot(data, aes(x = year, y = count, fill = .data[[input$report_category]])) +
            geom_bar(stat = "identity") +
            theme_minimal()
        ggiraph::girafe(ggobj = p, width_svg = 6, height_svg = 4)
    })

    # ------ * Data description -------------------------------------------------
    output$data_description <- renderUI({
        htmltools::htmlTemplate(
            file = paste0("www/html/description_", input$report_category, ".html")
        )
    })

})}

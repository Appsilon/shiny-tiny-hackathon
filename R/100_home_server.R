
home_server <- function(
    id
) { moduleServer(id, function(input, output, session) {

    # ------ REACTIVE ----------------------------------------------------------

    # ------ * Filter years ----------------------------------------------------
    filter_years <- reactiveVal("all_years")
    observeEvent(input$all_years, filter_years("all_years"))
    observeEvent(input$last_10_years, filter_years("last_10_years"))

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
    output$table <- renderUI({
        data <- data_wide()

        # Compute the grid template columns based on the number of columns
        grid_template_columns <- paste0(
            "repeat(", ncol(data), ", 1fr)",
            collapse = " "
        )

        data[, year := as.character(year)]
        data[is.na(year), year := "Total"]

        div(
            class = "grid-table",
            div(
                class = "grid-table-header",
                style = paste0("grid-template-columns: ", grid_template_columns),
                lapply(names(data), div, class = "grid-table-header-cell")
            ),
            div(
                class = "grid-table-body",
                lapply(
                    seq_len(nrow(data)),
                    function(i) {
                        # Add a special class for the total row for additional styling options
                        row_class <- if (data$year[i] == "Total") {
                            "grid-table-body-row total-row"
                        } else {
                            "grid-table-body-row"
                        }
                        
                        div(
                            class = row_class,
                            style = paste0("grid-template-columns: ", grid_template_columns),
                            lapply(data[i, ], function(cell_value) {
                                # Format numeric values with commas
                                if(is.numeric(cell_value)) {
                                    cell_value <- format(cell_value, big.mark = ",", trim = TRUE)
                                }
                                div(class = "grid-table-body-cell", cell_value)
                            })
                        )
                    }
                )
            )
        )
    })

    # ------ * Plot ------------------------------------------------------------
    output$plot <- ggiraph::renderGirafe({
        data <- data_list[[input$report_category]]
        
        # Get all possible levels for the selected category
        all_levels <- unique(unlist(lapply(data_list, function(df) {
            if (input$report_category %in% names(df)) {
                return(unique(df[[input$report_category]]))
            }
            return(NULL)
        })))
        
        # Create factor with ALL possible levels, not just those in current filtered data
        data[[input$report_category]] <- factor(
            data[[input$report_category]],
            levels = all_levels
        )
        
        if (filter_years() == "last_10_years") {
            data <- data[year >= max(year, na.rm = TRUE) - 10]
        }
        p <- ggplot(data, aes(
            x = year, 
            y = count, 
            fill = .data[[input$report_category]],
            tooltip = paste0(
                "<strong>", .data[[input$report_category]], ", ", year, "</strong><br />",
                "Count: ", format(count, big.mark = ",")
            )
        )) +
            ggiraph::geom_col_interactive(position = "stack") +
            scale_fill_manual(
                values = custom_palette,
                # Important: Drop=FALSE ensures all levels stay in the legend
                drop = FALSE
            ) +
            scale_y_continuous(
                labels = scales::label_number(big.mark = ","),
                expand = expansion(mult = c(0, 0.05))
            ) +
            labs(title = "", x = "", y = "", fill = "") +
            theme_minimal() +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                legend.position = "bottom"
            )

        ggiraph::girafe(
            ggobj = p,
            width_svg = 8,
            height_svg = 4,
            options = list(
                ggiraph::opts_hover(css = "fill-opacity: 0.8;"),
                ggiraph::opts_tooltip(
                    css = paste(
                        "background-color: white",
                        "color: #333",
                        "padding: 8px",
                        "border-radius: 4px",
                        "box-shadow: 0 0 10px 0 rgba(0, 0, 0, 0.1)",
                        "font-size: 12px",
                        sep = ";"
                    ),
                    placement = "container"
                )
            )
        )
    })

    # ------ * Data description -------------------------------------------------
    output$data_description <- renderUI({
        htmltools::htmlTemplate(
            file = paste0("www/html/description_", input$report_category, ".html")
        )
    })

})}

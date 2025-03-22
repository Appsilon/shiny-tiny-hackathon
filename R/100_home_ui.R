
home_ui <- function(id) {

    ns <- NS(id)

    tagList(
        fluidRow(
            column(
                width = 6,
                bslib::layout_column_wrap(
                    bslib::value_box(
                        value = "30,179,725",
                        title = "Total Reports",
                        showcase = bsicons::bs_icon("bar-chart-line")
                    ),
                    bslib::value_box(
                        value = "16,664,479",
                        title = "Serious Reports (excluding death)",
                        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
                    ),
                    bslib::value_box(
                        value = "2,722,806",
                        title = "Death Reports",
                        showcase = bsicons::bs_icon("x-circle-fill")
                    )
                )
            ),
            column(
                width = 6,
                selectInput(
                    inputId = ns("report_category"),
                    label = "Reports by",
                    choices = setNames(
                        ref_list$report_categories$name,
                        ref_list$report_categories$label
                    )
                )
            )
        ),
        fluidRow(
            column(
                width = 6,
                DT::DTOutput(ns("table"))
            ),
            column(
                width = 6,
                ggiraph::girafeOutput(ns("plot"))
            )
        ),
        fluidRow(
            column(
                width = 12,
                h3("Data as of December 31, 2024"),
                uiOutput(ns("data_description"))
            )
        )
    )


}

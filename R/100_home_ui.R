
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
            )
        ),
        fluidRow(
            column(
                width = 6,
                DT::DTOutput(ns("table"))
            ),
            column(
                width = 6,
                ggiraph::ggiraphOutput(ns("plot"))
            )
        )
    )


}

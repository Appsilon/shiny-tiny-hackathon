
home_ui <- function(id) {

    ns <- NS(id)

    tags$div(
        class = "main-content",
        fluidRow(
            class = "align-items-center py-4 mb-4",
            style = "background-color: #F6F6F6;",
            column(
                width = 6,
                div(
                    class = "select-input",
                    selectInput(
                        inputId = ns("report_category"),
                        label = "Reports by:",
                        choices = setNames(
                            ref_list$report_categories$name,
                            ref_list$report_categories$label
                        )
                    )
                )
            ),
            column(
                width = 6,
                div(
                    class = "d-flex flex-wrap justify-content-between",
                    div(
                        class = "value-box",
                        div(
                            class = "value-box-icon",
                            bsicons::bs_icon("bar-chart-line")
                        ),
                        div(
                            class = "value-box-content",
                            div(
                                class = "value-box-content-value",
                                id = ns("total_reports"),
                                "30,179,725"
                            ),
                            div(
                                class = "value-box-content-label",
                                "Total Reports"
                            )
                        )
                    ),
                    div(
                        class = "value-box",
                        div(
                            class = "value-box-icon",
                            bsicons::bs_icon("exclamation-triangle-fill")
                        ),
                        div(
                            class = "value-box-content",
                            div(
                                class = "value-box-content-value",
                                id = ns("serious_reports"),
                                "16,664,479"
                            ),
                            div(
                                class = "value-box-content-label",
                                "Serious Reports (excluding death)"
                            )
                        )
                    ),
                    div(
                        class = "value-box",
                        div(
                            class = "value-box-icon",
                            bsicons::bs_icon("x-circle-fill")
                        ),
                        div(
                            class = "value-box-content",
                            div(
                                class = "value-box-value",
                                id = ns("death_reports"),
                                "2,722,806"
                            ),
                            div(
                                class = "value-box-label",
                                "Death Reports"
                            )
                        )
                    )
                )
            )
        ),
        fluidRow(
            style = "height: 45vh;",
            column(
                width = 6,
                uiOutput(ns("table"))
            ),
            column(
                width = 6,
                ggiraph::girafeOutput(ns("plot"), height = "45vh", width = "100%")
            )
        ),
        fluidRow(
            class = "footer",
            column(
                width = 12,
                div(
                    class = "footer-content",
                    div(
                        class = "footer-content-date",
                        "Data as of December 31, 2024"
                    ),
                    div(
                        class = "footer-content-description",
                        uiOutput(ns("data_description"))
                    )
                )
            )
        )
    )


}

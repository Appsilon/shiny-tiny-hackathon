box::use(shiny,
         bslib,
         bsicons,
         sass,
         dplyr,
         shinyWidgets,
)

box::use(app / logic / data_f ,
         app / view / table,
         app / view / hc,
)


theme <- bslib$bs_theme() |>
  bslib$bs_add_rules(sass$sass_file("app/styles/main.scss"))


#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$page_navbar(
    theme = theme,
    selected = "Home",
    id = ns("tabs"),
    header = shiny$tags$head(shiny$tags$title("FDA Adverse Event Reporting System")),
    title = shiny$img(src = "static/logo.png", alt = "FDA", class = "logofda"),
    position = "fixed-top",
    bslib$nav_panel(
      title = "Home",
      # Report Type
      shiny$selectInput(
        ns("report_type"),
        "",
        selected = NULL,
        choices = NULL
      ),
      # Cards
      bslib$layout_column_wrap(
        fill = FALSE,
        width = 1 / 3,

        bslib$value_box(
          title = "Total Reports",
          value = shiny$textOutput(ns("card_total")),
          showcase = bsicons$bs_icon("file-earmark-text")
        ),
        bslib$value_box(
          title = "Serious Reports",
          value = shiny$textOutput(ns("card_serious")),
          showcase = bsicons$bs_icon("exclamation-triangle")
        ),
        bslib$value_box(
          title = "Death Reports",
          value = shiny$textOutput(ns("card_death")),
          showcase = bsicons$bs_icon("clipboard-x")
        )
      ),

      # Filters and table
      bslib$layout_column_wrap(
        width = 1 / 2,
        height = 300,
        bslib$card(
          full_screen = TRUE,
          bslib$layout_column_wrap(
            width = 1 / 2,
            shinyWidgets$pickerInput(
              ns("years"),
              "Years",
              selected = NULL,
              choices = NULL,
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            shinyWidgets$pickerInput(
              ns("variables"),
              "Variables",
              selected = NULL,
              choices = NULL,
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
          ),
          shiny$br(),
          table$ui(ns("table"))
        ),
        # Chart
        bslib$card(full_screen = TRUE,
                   hc$ui(ns("hc")))
      )


    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      shiny$actionButton(ns("disclaimer"), "Disclaimer", class = "btn btn-primary btn-header")
    ),
    bslib$nav_item(
      shiny$actionButton(
        "reportproblem",
        "Report a Problem",
        class = "bbtn btn-primary btn-header",
        onclick = "window.location.href='https://www.accessdata.fda.gov/scripts/medwatch/index.cfm?action=reporting.home';"
      )
    ),
    bslib$nav_item(
      shiny$actionButton("faq", "FAQ", class = "bbtn btn-primary btn-header",
                         onclick = "window.location.href='https://fis.fda.gov/extensions/FPD-FAQ/FPD-FAQ.html';")
    )

  )

}


#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Taxonomy data for filters and data manipulation
    taxonomy <- shiny$reactive({
      data_f$taxonomy()
    })

    # Updating Report Type input
    shiny$observe({
      desc <- unique(taxonomy()$description)
      shiny$updateSelectizeInput(session,
                                 "report_type",
                                 choices =  desc,
                                 selected = desc[1])
    })

    #####
    # Data loading (reactive used for lazy loading)
    report_data <- shiny$reactive({
      data_f$report_data()
    })

    reporter_data <- shiny$reactive({
      data_f$reporter_data()
    })

    sex_data <- shiny$reactive({
      data_f$sex_data()
    })
    #####

    # Getting data name
    choosen_data_name <- shiny$reactive({
      shiny$req(taxonomy())
      shiny$req(input$report_type)

      chosen_data <- taxonomy() |>
        dplyr$filter(description == input$report_type) |>
        dplyr$pull(data_name) |>
        unique()
      chosen_data
    })


    selected_data <- shiny$reactive({
      shiny$req(choosen_data_name())
      chosen_data <- choosen_data_name()
      if (chosen_data == "report_data") {
        report_data()
      } else if (chosen_data == "sex_data") {
        sex_data()
      } else if (chosen_data == "reporter_data") {
        reporter_data()
      } else {
        NULL
      }
    })

    # Update Variables and Years by selected Report Type
    shiny$observe({
      shiny$req(input$report_type)

      chosen_data <- selected_data()
      series_cols <-
        setdiff(names(chosen_data), c("Year", "Total_Reports"))

      shinyWidgets$updatePickerInput(session,
                                     "years",
                                     choices =  chosen_data$Year,
                                     selected = chosen_data$Year)

      shinyWidgets$updatePickerInput(session,
                                     "variables",
                                     choices =  series_cols,
                                     selected = series_cols)

    })


    filtered_data <- shiny$reactive({
      shiny$req(selected_data())
      shiny$req(input$years)
      shiny$req(input$variables)

      if (!is.null(input$years)) {
        df <-  selected_data() |> dplyr$filter(Year %in% input$years)

      }
      if (!is.null(input$variables)) {
        df <-  df |> dplyr$select(Year, Total_Reports, input$variables)
      }
      df
    })


    ####
    # Cards
    output$card_total <- shiny$renderText({
      shiny$req(filtered_data())
      format(
        sum(filtered_data()$Total_Reports, na.rm = TRUE),
        big.mark = ",",
        scientific = FALSE
      )
    })

    output$card_serious <- shiny$renderText({
      shiny$req(filtered_data())
      format(round(2 / 3 * sum(
        filtered_data()$Total_Reports, na.rm = TRUE
      )),
      big.mark = ",",
      scientific = FALSE)
    })

    output$card_death <- shiny$renderText({
      shiny$req(filtered_data())
      format(round(1 / 1111 * sum(
        filtered_data()$Total_Reports, na.rm = TRUE
      )),
      big.mark = ",",
      scientific = FALSE)
    })
    ####


    # Modules for table and chart
    # filtered reactive data used for dynamic update
    table$server("table", filtered_data)
    hc$server("hc", filtered_data)


  })
}

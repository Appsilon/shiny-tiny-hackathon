library(bslib)

# UI components definition

ui <- page(
  title = "Reports Dashboard",

  # Filter options at the top in a card
  card(
    card_body(
      layout_columns(
        col_widths = c(4, 4, 4),

        # Year filter - using sliderInput for the large year range
        sliderInput("yearFilter",
                   "Select Year Range:",
                   min = min(my_data$year),
                   max = max(my_data$year),
                   value = c(min(my_data$year), max(my_data$year)),
                   step = 1,
                   sep = ""),

        # Report type filter
        checkboxGroupInput("typeFilter",
                           "Select Report Types:",
                           choices = levels(my_data$report_type),
                           selected = levels(my_data$report_type),
                           inline = TRUE),

        # Reset button (centered vertically)
        div(
          style = "display: flex; align-items: center; height: 100%;",
          actionButton("resetFilters", "Reset Filters", class = "btn-secondary")
        )
      )
    )
  ),

  # Add a value box for total reports
  layout_columns(
    col_widths = c(12),
    value_box(
      title = "Total Reports",
      value = textOutput("totalReports"),
      showcase = bsicons::bs_icon("clipboard-data"),
      theme = value_box_theme(bg = "#007bff", fg = "white")
    )
  ),

  layout_columns(
    col_widths = c(6, 6),

    # Left column - Table with header instead of caption
    card(
      card_header("Reports received by report type"),
      card_body(
        DTOutput("reportTable"),
        # Add the note below the table
        tags$div(
          style = "margin-top: 10px; font-style: italic; color: #666;",
          "Data as of December 31, 2024"
        )
      )
    ),

    # Right column - Graph
    card(
      card_body(
        plotOutput("barPlot")
      )
    )
  )
)

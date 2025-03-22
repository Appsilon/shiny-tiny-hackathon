#' Get the columns to use in the plot based on the selected chart
#'
#' This function returns the specific columns to be used in the plot depending on the selected chart type.
#'
#' @param selected_chart A string representing the selected chart type. Possible values are:
#'   - "by_reporter_type"
#'   - "by_reporter"
#'   - "by_reporter_region"
#'   - "by_report_seriousness"
#'   - "by_age_group"
#'   - "by_sex"
#'
#' @returns A vector of column names to be used in the plot corresponding to the selected chart.
#' @export
get_plot_columns <- function(selected_chart) {
  switch(selected_chart,
         "by_reporter_type" = c("expedited", "non_expedited", "direct", "bsr"),
         "by_reporter" = c("consumer", "health_professional", "not_specified_reporter", "other_reporter"),
         "by_reporter_region" = c("domestic", "foreign", "not_specified_region"),
         "by_report_seriousness" = c("serious", "death", "non_serious"),
         "by_age_group" = c("months_0_1", "months_2_36", "years_3_11", "years_12_17", "years_18_64"),
         "by_sex" = c("female", "male", "not_specified_sex")
         )
}


#' Add plotly stacked bar chart
#'
#' This function creates a stacked bar chart using Plotly based on the selected columns.
#' The plot shows the distribution of the selected categories over the years.
#'
#' @param columns_to_use A vector of column names to be used in the plot. This should be a subset of the columns 
#'   available in the dataset, typically returned by the `get_plot_columns` function.
#'
#' @returns A Plotly stacked bar chart.
#' @export
stacked_bars <- function(columns_to_use, years_to_display) {
  
  # Default column colors
  column_colors <- c(
    'total' = '#1f77b4',               # Blue
    'expedited' = '#ff7f0e',           # Orange
    'non_expedited' = '#2ca02c',       # Green
    'direct' = '#d62728',              # Red
    'bsr' = '#9467bd',                 # Purple
    'consumer' = '#8c564b',            # Brown
    'health_professional' = '#e377c2', # Pink
    'not_specified_reporter' = '#7f7f7f', # Gray
    'other_reporter' = '#bcbd22',      # Olive
    'domestic' = '#17becf',            # Cyan
    'foreign' = '#ff9896',             # Light Red
    'not_specified_region' = '#9edae5', # Light Cyan
    'serious' = '#393b79',             # Dark Blue
    'death' = '#f03b20',               # Dark Red
    'non_serious' = '#7f7f7f',         # Gray
    'months_0_1' = '#aec7e8',          # Light Blue
    'months_2_36' = '#ffbb78',         # Light Orange
    'years_3_11' = '#98df8a',          # Light Green
    'years_12_17' = '#ff9896',         # Light Red
    'years_18_64' = '#c5b0d5',         # Light Purple
    'female' = '#ff1493',              # Deep Pink
    'male' = '#0000ff',                # Blue
    'not_specified_sex' = '#d3d3d3'    # Light Gray
  )
  
  # Filter the data to include only the specified columns (and 'year')
  data_long <- mock_data %>%
    select(c("year", columns_to_use)) %>%
    filter(
      if (years_to_display == "All years") {
        year %in% 1968:2024
      } else {
        between(year, 2015, 2024)
      }
    ) %>% 
    pivot_longer(cols = -year, names_to = "category", values_to = "count")  # Convert to long format
  
  # Add default colors to the category if they are not in the dataset
  selected_colors <- column_colors[names(column_colors) %in% columns_to_use]  # Keep only colors for selected columns
  
  # Create the Plotly plot
  plot <- data_long %>%
    plot_ly(
      x = ~year,
      y = ~count,
      color = ~category,
      colors = selected_colors,
      type = 'bar',
      # text = ~category,
      hoverinfo = 'category'
    ) %>%
    layout(
      barmode = 'stack',  # Stacked bars
      title = 'Stacked Bar Plot by Category and Year',
      xaxis = list(title = 'Year'),
      yaxis = list(title = 'Total Amount')
    )
  
  return(plot)
}

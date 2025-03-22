box::use(
  highcharter,
  dplyr,
)

#' @export
create_stock_chart <- function(data) {
  # Check if the data contains a "Year" column
  if (!"Year" %in% names(data)) stop("Data must contain a 'Year' column.")
  
  # Arrange the data in ascending order by Year
  data_subset <- data |> dplyr$arrange(Year)
  
  # Convert the 'Year' column to a Date and then to a timestamp (Highstock format)
  data_subset$Date <- highcharter$datetime_to_timestamp(
    as.Date(paste0(data_subset$Year, "-01-01"))
  )
  
  # Define series columns by excluding "Year", "Total_Reports", and "Date"
  series_cols <- setdiff(names(data_subset), c("Year", "Total_Reports", "Date"))
  
  # Create a Highstock chart with range selector, formatted y-axis and stack labels,
  # and a tooltip that shows each series value and the total with thousand separators.
  hc <- highcharter$highchart(type = "stock") |>
    highcharter$hc_rangeSelector(selected = 6) |>
    highcharter$hc_xAxis(type = "datetime") |>
    highcharter$hc_yAxis(
      opposite = FALSE,  # Place the y-axis on the left
      title = list(text = "Report Count"),
      labels = list(
        formatter = highcharter$JS("function() { 
          return Highcharts.numberFormat(this.value, 0, '.', ','); 
        }")
      ),
      stackLabels = list(
        enabled = TRUE,
        formatter = highcharter$JS("function() { 
          return Highcharts.numberFormat(this.total, 0, '.', ','); 
        }")
      )
    ) |>
    highcharter$hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      formatter = highcharter$JS("function () {
          var s = '<b>' + Highcharts.dateFormat('%Y', this.x) + '</b>';
          var total = 0;
          this.points.forEach(function(point) {
              s += '<br/><span style=\"color:' + point.series.color + '\">' 
                + point.series.name + '</span>: <b>' 
                + Highcharts.numberFormat(point.y, 0, '.', ',') + '</b>';
              total += point.y;
          });
          s += '<br/><span style=\"font-weight:bold;\">Total: ' 
            + Highcharts.numberFormat(total, 0, '.', ',') + '</span>';
          return s;
      }")
    ) |>
    highcharter$hc_legend(enabled = TRUE) |>  # Enable legend display
    highcharter$hc_plotOptions(column = list(stacking = "normal"))
  
  # Loop through each series column, format the series name and data, and add it to the chart.
  for (col in series_cols) {
    series_name <- gsub("_", " ", col)  # Replace "_" with space in series names
    series_data <- lapply(seq_along(data_subset$Date), function(i) {
      list(data_subset$Date[i], as.numeric(data_subset[[col]][i]))
    })
    hc <- hc |> 
      highcharter$hc_add_series(
        name = series_name,
        data = series_data,
        type = "column",
        stacking = "normal"
      ) |> 
      highcharter$hc_exporting(enabled = TRUE)
  }
  
  return(hc)
}

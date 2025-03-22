box::use(
  reactable,
  htmlwidgets,
)


# Dynamic coldefs for reactable
#' @export
fda_coldefs <- function(data){
  col_defs <- lapply(names(data), function(col) {
    displayName <- gsub("_", " ", col)
    
    if (col == "Year") {
      reactable$colDef(
        name = displayName,
        cell = htmlwidgets$JS("function(cellInfo) { 
                     return cellInfo.value == null ? '-' : cellInfo.value; 
                   }"),
        footer = "Total",
        defaultSortOrder = "desc"
      )
    } else if (is.numeric(data[[col]])) {
      reactable$colDef(
        name = displayName,
        cell = htmlwidgets$JS("function(cellInfo) {
                     if (cellInfo.value == null) { 
                       return '-'; 
                     } else if (typeof cellInfo.value === 'number') { 
                       return cellInfo.value.toLocaleString(); 
                     } else { 
                       return cellInfo.value; 
                     }
                   }"),
        footer = function(values) {
          formattedTotal <- format(sum(values, na.rm = TRUE), big.mark = ",", scientific = FALSE)
          formattedTotal
        }
      )
    } else {
      reactable$colDef(
        name = displayName,
        cell = htmlwidgets$JS("function(cellInfo) { 
                     return cellInfo.value == null ? '-' : cellInfo.value; 
                   }")
      )
    }
  })
  
  names(col_defs) <- names(data)
  return(col_defs)
}


#' @export
fda_table <- function(data, col_defs){
  reactable$reactable(
    data,
    columns = col_defs,
    defaultSorted = "Year",
    sortable = TRUE,
    defaultPageSize = 14
  )
}
# data_functions.R - Funciones para el manejo de datos

# Función para obtener la ruta del archivo y el nombre a mostrar
obtener_info_archivo <- function(tipo_archivo) {
  archivo <- paste0("data/", tipo_archivo, ".parquet")
  
  # Convertir primera letra a mayúscula
  nombre_base <- gsub("_", " ", tipo_archivo)
  nombre_base <- gsub("\\b(\\w)", "\\U\\1", nombre_base, perl = TRUE)
  
  # Añadir prefijo "Report by"
  nombre_mostrar <- paste("Report by", nombre_base)
  
  return(list(
    archivo = archivo,
    nombre_mostrar = nombre_mostrar
  ))
}

# Función para leer datos según tipo de archivo (manteniendo formato long)
leer_datos_filtrados <- function(tipo_archivo) {
  # Obtener información del archivo
  info <- obtener_info_archivo(tipo_archivo)
  archivo <- info$archivo
  nombre_mostrar <- info$nombre_mostrar
  
  # Intentar leer el archivo
  tryCatch({
    datos <- read_parquet(archivo)
    
    # Ajustar los datos según el tipo de archivo
    if(tipo_archivo == "report_type") {
      if("Year" %in% names(datos) && "Total.Reports" %in% names(datos)) {
        datos <- datos %>% 
          select(Year,Total.Reports, Expedited, Non.Expedited, Direct, BSR) %>%
          rename(
            `Total Reports` = Total.Reports,
            `Non-Expedited` = Non.Expedited
          ) %>%
          filter(!is.na(Year))
      }
    } else if(tipo_archivo == "reporter") {
      if("Year" %in% names(datos) && "Reporter" %in% names(datos) && "Report.Count" %in% names(datos)) {
        datos <- datos %>%
          select(Year, Reporter, Report.Count) %>%
          rename(ReportCount = Report.Count)
      }
    } else if(tipo_archivo == "reporter_region") {
      if("Year" %in% names(datos) && "Reporter.Region" %in% names(datos) && "Report.Count" %in% names(datos)) {
        datos <- datos %>%
          select(Year, Reporter.Region, Report.Count) %>%
          rename(
            ReporterRegion = Reporter.Region,
            ReportCount = Report.Count
          )
      }
    } else if(tipo_archivo == "report_serioursness") {
      if("Year" %in% names(datos) && "Seriousness" %in% names(datos) && "Report.Count" %in% names(datos)) {
        datos <- datos %>%
          select(Year, Seriousness, Report.Count) %>%
          rename(
            ReportCount = Report.Count
          )
      }
    } else if(tipo_archivo == "age_group") {
      if("Year" %in% names(datos) && "Age.Group" %in% names(datos) && "Report.Count" %in% names(datos)) {
        datos <- datos %>%
          select(Year, Age.Group, Report.Count) %>%
          rename(
            `Age Group` = Age.Group,
            ReportCount = Report.Count
          )
      }
    } else if(tipo_archivo == "sex") {
      if("Year" %in% names(datos) && "Sex" %in% names(datos) && "Report.Count" %in% names(datos)) {
        datos <- datos %>%
          select(Year, Sex, Report.Count)%>%
          rename(
            ReportCount = Report.Count
          )
      }
    }
    
    # Añadir el nombre para mostrar como atributo
    attr(datos, "nombre_mostrar") <- nombre_mostrar
    
    # Añadir información de categoría como atributo para conversión posterior
    if(tipo_archivo == "report_type") {
      attr(datos, "categoria") <- NULL
      attr(datos, "valor") <- "Total Reports"
    } else if(tipo_archivo == "reporter") {
      attr(datos, "categoria") <- "Reporter"
      attr(datos, "valor") <- "ReportCount"
    } else if(tipo_archivo == "reporter_region") {
      attr(datos, "categoria") <- "ReporterRegion"
      attr(datos, "valor") <- "ReportCount"
    } else if(tipo_archivo == "report_serioursness") {
      attr(datos, "categoria") <- "Seriousness"
      attr(datos, "valor") <- "ReportCount"
    } else if(tipo_archivo == "age_group") {
      attr(datos, "categoria") <- "Age Group"
      attr(datos, "valor") <- "ReportCount"
    } else if(tipo_archivo == "sex") {
      attr(datos, "categoria") <- "Sex"
      attr(datos, "valor") <- "ReportCount"
    }
    
    return(datos)
  }, error = function(e) {
    message("Error al leer el archivo ", archivo, ": ", e$message)
    return(NULL)
  })
}

# Inicializar datos de resumen
inicializar_datos_resumen <- function() {
  # Intentar leer datos de reportes totales
  datos_totales <- tryCatch({
    df <- read_parquet("data/report_type.parquet")
    # Extraer el valor total de reportes
    if("Total.Reports" %in% names(df)) {
      total_value <- df$Total.Reports[1]
      if(is.na(total_value)) {
        total_value <- sum(df$Total.Reports, na.rm = TRUE)
      }
      total_value
    } else {
      0
    }
  }, error = function(e) {
    message("Error al leer los datos totales: ", e$message)
    return(0)
  })
  
  # Leer datos de seriedad para obtener reportes serios y muertes
  seriousness_data <- tryCatch({
    read_parquet("data/report_serioursness.parquet")
  }, error = function(e) {
    message("Error al leer datos de seriedad: ", e$message)
    return(NULL)
  })
  
  # Calcular reportes serios y muertes
  serious_reports <- 0
  death_reports <- 0
  
  if(!is.null(seriousness_data) && 
     "Seriousness" %in% names(seriousness_data) && 
     "Report.Count" %in% names(seriousness_data)) {
    
    # Sumar todos los reportes serios
    serious_data <- seriousness_data %>% 
      filter(Seriousness == "Serious")
    
    if(nrow(serious_data) > 0) {
      serious_reports <- sum(serious_data$Report.Count, na.rm = TRUE)
    }
    
    # Sumar todos los reportes de muerte
    death_data <- seriousness_data %>% 
      filter(Seriousness == "Death")
    
    if(nrow(death_data) > 0) {
      death_reports <- sum(death_data$Report.Count, na.rm = TRUE)
    }
  }
  
  # Fecha de actualización
  fecha_actualizacion <- format(Sys.Date(), "%B %d, %Y")
  
  return(list(
    total = datos_totales,
    serious = serious_reports,
    death = death_reports,
    fecha = fecha_actualizacion
  ))
}

# Obtener datos filtrados por tipo de archivo y período
obtener_datos_filtrados <- function(tipo_archivo, periodo) {
  # Obtener datos según tipo de archivo
  datos <- leer_datos_filtrados(tipo_archivo)
  
  # Aplicar filtro de período si es necesario
  if(!is.null(datos) && periodo == "last10" && "Year" %in% names(datos)) {
    # Convertir Year a numérico si es character
    if(is.character(datos$Year)) {
      datos$Year <- as.numeric(datos$Year)
    }
    
    año_actual <- max(datos$Year, na.rm = TRUE)
    datos <- datos %>% filter(Year > año_actual - 10)
  }
  
  return(datos)
}

# Obtener datos para el gráfico (mantiene formato long)
obtener_datos_grafico <- function(tipo_archivo, periodo) {
  datos <- obtener_datos_filtrados(tipo_archivo, periodo)
  return(datos)
}

# Convertir datos a formato wide para tablas
convertir_a_wide <- function(datos) {
  # Verificar si ya está en formato wide o no necesita conversión
  if(is.null(datos) || ncol(datos) <= 2 || is.null(attr(datos, "categoria"))) {
    return(datos)
  }
  
  # Obtener nombre de columna de categoría y de valor
  col_categoria <- attr(datos, "categoria")
  col_valor <- attr(datos, "valor")
  
  # Convertir a wide
  datos_wide <- datos %>%
    pivot_wider(
      id_cols = Year,
      names_from = all_of(col_categoria),
      values_from = all_of(col_valor),
      values_fill = 0
    )
  
  # Mantener los atributos
  attr(datos_wide, "nombre_mostrar") <- attr(datos, "nombre_mostrar")
  
  return(datos_wide)
}

# Función para obtener el nombre para mostrar
obtener_nombre_mostrar <- function(tipo_archivo) {
  info <- obtener_info_archivo(tipo_archivo)
  return(info$nombre_mostrar)
}
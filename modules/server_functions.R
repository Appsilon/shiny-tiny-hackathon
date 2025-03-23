crear_tabla_datos <- function(datos_tabla, tipo_reporte = "report_type") {
  library(DT)
  library(dplyr)
  library(tidyr)
  
  if(is.null(datos_tabla)) {
    return(datatable(data.frame(Mensaje = "No se pudieron cargar los datos")))
  }
  
  # Para report_type (caso especial que ya tiene formato específico)
  if(tipo_reporte == "report_type") {
    # Asegurarse de que los datos estén en el formato correcto
    if(!"Year" %in% names(datos_tabla)) {
      return(datatable(data.frame(Error = "Estructura de datos incorrecta, falta columna Year")))
    }
    
    # Crear la tabla con DT
    tabla <- datatable(
      datos_tabla,
      colnames = c("Year", "Total Reports", "Expedited", "Non-Expedited", "Direct", "BSR"),
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = 'rt',
        ordering = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-center"),
          list(targets = 0, className = "dt-left")
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Year',
        target = 'row',
        fontWeight = styleEqual("Total Reports", 'bold')
      )
    
    # Formatear columnas numéricas con separadores de miles (excluyendo la columna Year)
    cols_numericas <- which(sapply(datos_tabla, is.numeric) & names(datos_tabla) != "Year")
    if(length(cols_numericas) > 0) {
      tabla <- tabla %>%
        formatCurrency(
          columns = cols_numericas,
          currency = "", 
          interval = 3, 
          mark = ",", 
          digits = 0
        )
    }
    
    return(tabla)
  } else {
    # Para otros tipos de reportes usando los atributos asignados en leer_datos_filtrados
    
    # Obtener categoría y valor de los atributos
    categoria_col <- attr(datos_tabla, "categoria")
    valor_col <- attr(datos_tabla, "valor")
    
    # Si no hay atributos definidos, intentar detectar automáticamente
    if(is.null(categoria_col) || is.null(valor_col)) {
      # Detectar basado en nombres estándar
      valor_candidatos <- c("ReportCount", "Report.Count")
      for(val in valor_candidatos) {
        if(val %in% names(datos_tabla)) {
          valor_col <- val
          break
        }
      }
      
      # Encontrar la columna de categoría (que no sea Year ni valor)
      if(!is.null(valor_col)) {
        categoria_col <- setdiff(names(datos_tabla), c("Year", valor_col))[1]
      }
    }
    
    # Verificar que tengamos lo necesario para hacer pivot
    if(!is.null(categoria_col) && !is.null(valor_col) && "Year" %in% names(datos_tabla)) {
      # Transformar a formato wide
      datos_wide <- datos_tabla %>%
        pivot_wider(
          id_cols = Year,
          names_from = !!categoria_col,
          values_from = !!valor_col,
          values_fill = 0
        )
    } else {
      # Si no podemos hacer pivot, usar datos originales
      datos_wide <- datos_tabla
    }
    
    # Convertir la columna Year a character para evitar problemas al combinar con "Total Reports"
    datos_wide <- datos_wide %>%
      mutate(Year = as.character(Year))
    
    # Identificar columnas numéricas para agregar fila de totales (excluyendo Year)
    col_numericas <- names(datos_wide)[sapply(datos_wide, is.numeric) & names(datos_wide) != "Year"]
    
    # Calcular totales para cada columna numérica
    if(length(col_numericas) > 0) {
      # Crear una fila con los totales
      fila_total <- data.frame(Year = "Total Reports")
      
      # Calcular el total de cada columna
      for(col in col_numericas) {
        fila_total[[col]] <- sum(datos_wide[[col]], na.rm = TRUE)
      }
      
      # Agregar fila de totales al inicio
      datos_wide <- bind_rows(fila_total, datos_wide)
    }
    
    # Ordenar el resto de filas por año de manera descendente, manteniendo "Total Reports" al inicio
    datos_wide <- datos_wide %>%
      mutate(
        # Convertir Year a numérico para ordenar (solo para los que no son "Total Reports")
        year_num = ifelse(Year == "Total Reports", -Inf, as.numeric(Year)),
        # Crear variable para ordenar
        orden = ifelse(Year == "Total Reports", 0, 1)
      ) %>%
      arrange(orden, desc(year_num)) %>%
      select(-orden, -year_num)
    
    # Crear tabla con los datos transformados
    return(datatable(
      datos_wide,
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = 'rt',
        ordering = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-center"),
          list(targets = 0, className = "dt-left")
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Year',
        target = 'row',
        fontWeight = styleEqual("Total Reports", 'bold')
      ) %>%
      formatCurrency(
        # Seleccionar solo columnas numéricas que no sean la columna Year
        columns = which(sapply(datos_wide, is.numeric) & names(datos_wide) != "Year"), 
        currency = "", 
        interval = 3, 
        mark = ",", 
        digits = 0
      ))
  }
}

# Crear gráfico de barras - Versión mejorada con leyenda a la derecha y categorías para report_type
crear_grafico_barras <- function(chart_data, tipo_reportero) {
  # Importaciones necesarias
  library(dplyr)
  library(plotly)
  library(tidyr)
  
  if(is.null(chart_data)) {
    return(plot_ly() %>% 
             layout(title = "No se pudieron cargar los datos para el gráfico"))
  }
  
  # Preparar datos según su estructura
  if(tipo_reportero == "report_type" || tipo_reportero == "All Reporters") {
    # Para datos de reportes totales - CAMBIO IMPORTANTE AQUÍ
    if("Year" %in% names(chart_data)) {
      # Eliminar la primera fila del dataframe si hay valores NA
      chart_data <- chart_data %>% filter(Year != "Total Reports",!is.na(Year))
      
      # Identificar las columnas de categorías (todas excepto Year)
      categorias <- setdiff(names(chart_data), "Year")
      
      # Eliminar explícitamente Total Reports y Total.Reports de las categorías
      categorias <- setdiff(categorias, c("Total Reports", "Total.Reports"))
      
      # Si no quedan categorías, mostrar error
      if(length(categorias) == 0) {
        return(plot_ly() %>% 
                 layout(title = "No hay categorías disponibles para mostrar"))
      }
      
      # Colores para diferentes categorías
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      # Iniciar con la primera categoría
      primera_categoria <- categorias[1]
      p <- plot_ly(chart_data, x = ~Year, y = as.formula(paste0("~`", primera_categoria, "`")), 
                   type = 'bar', name = primera_categoria,
                   marker = list(color = colores[1]))
      
      # Agregar el resto de las categorías con diferentes colores
      if(length(categorias) > 1) {
        for(i in 2:min(length(categorias), length(colores))) {
          categoria <- categorias[i]
          p <- p %>% add_trace(
            y = as.formula(paste0("~`", categoria, "`")), 
            name = categoria,
            marker = list(color = colores[i])
          )
        }
      }
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else if(tipo_reportero == "reporter" || tipo_reportero %in% c("Consumer", "Healthcare Professional")) {
    # Para datos de reportero específico
    if("Year" %in% names(chart_data) && "Reporter" %in% names(chart_data) && "ReportCount" %in% names(chart_data)) {
      # Preparar datos para gráfico: agrupar por año y reportero
      chart_data_prep <- chart_data %>%
        group_by(Year, Reporter) %>%
        summarise(Report.Count = sum(ReportCount, na.rm = TRUE), .groups = 'drop')
      
      # Crear un gráfico con barras apiladas por tipo de reportero
      reporteros <- unique(chart_data_prep$Reporter)
      
      # Iniciar con el primer reportero
      p <- plot_ly(chart_data_prep %>% filter(Reporter == reporteros[1]), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = reporteros[1],
                   marker = list(color = '#4682B4'))
      
      # Agregar el resto de los reporteros con diferentes colores
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      if(length(reporteros) > 1) {
        for(i in 2:min(length(reporteros), length(colores))) {
          p <- p %>% add_trace(
            data = chart_data_prep %>% filter(Reporter == reporteros[i]),
            y = ~Report.Count, name = reporteros[i],
            marker = list(color = colores[i])
          )
        }
      }
    } else if("Year" %in% names(chart_data) && tipo_reportero %in% names(chart_data)) {
      # Formato alternativo (wide)
      p <- plot_ly(chart_data, x = ~Year, y = as.formula(paste0("~`", tipo_reportero, "`")), 
                   type = 'bar', name = tipo_reportero,
                   marker = list(color = '#4682B4'))
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else if(tipo_reportero == "reporter_region" || tipo_reportero == "Reporter Region") {
    # Para datos por región
    if("Year" %in% names(chart_data) && "ReporterRegion" %in% names(chart_data) && "ReportCount" %in% names(chart_data)) {
      # Preparar datos para gráfico: agrupar por año y región
      chart_data_prep <- chart_data %>%
        group_by(Year, ReporterRegion) %>%
        summarise(Report.Count = sum(ReportCount, na.rm = TRUE), .groups = 'drop')
      
      # Crear un gráfico con barras apiladas por región
      regiones <- unique(chart_data_prep$ReporterRegion)
      
      # Iniciar con la primera región
      p <- plot_ly(chart_data_prep %>% filter(ReporterRegion == regiones[1]), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = regiones[1],
                   marker = list(color = '#4682B4'))
      
      # Agregar el resto de las regiones con diferentes colores
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      if(length(regiones) > 1) {
        for(i in 2:min(length(regiones), length(colores))) {
          p <- p %>% add_trace(
            data = chart_data_prep %>% filter(ReporterRegion == regiones[i]),
            y = ~Report.Count, name = regiones[i],
            marker = list(color = colores[i])
          )
        }
      }
    } else if("Year" %in% names(chart_data) && "Reporter.Region" %in% names(chart_data) && "Report.Count" %in% names(chart_data)) {
      # Formato alternativo
      # Crear un gráfico con barras apiladas por región
      regiones <- unique(chart_data$Reporter.Region)
      
      # Iniciar con la primera región
      p <- plot_ly(chart_data %>% filter(Reporter.Region == regiones[1]), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = regiones[1],
                   marker = list(color = '#4682B4'))
      
      # Agregar el resto de las regiones con diferentes colores
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      if(length(regiones) > 1) {
        for(i in 2:min(length(regiones), length(colores))) {
          p <- p %>% add_trace(
            data = chart_data %>% filter(Reporter.Region == regiones[i]),
            y = ~Report.Count, name = regiones[i],
            marker = list(color = colores[i])
          )
        }
      }
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else if(tipo_reportero == "report_serioursness" || tipo_reportero == "Seriousness") {
    # Para datos de seriedad
    if("Year" %in% names(chart_data) && "Seriousness" %in% names(chart_data) && "ReportCount" %in% names(chart_data)) {
      # Preparar datos para gráfico
      chart_data_prep <- chart_data %>%
        group_by(Year, Seriousness) %>%
        summarise(Report.Count = sum(ReportCount, na.rm = TRUE), .groups = 'drop')
      
      # Colores para tipos de seriedad
      colores_seriedad <- list(
        "Serious" = '#FFB347',  # Naranja para serio
        "Death" = '#FF6B6B',    # Rojo para muerte
        "Non-Serious" = '#90EE90' # Verde para no serio
      )
      
      # Crear un gráfico con barras apiladas por tipo de seriedad
      tipos_seriedad <- unique(chart_data_prep$Seriousness)
      
      # Iniciar con el primer tipo
      primer_tipo <- tipos_seriedad[1]
      color_primer_tipo <- colores_seriedad[[primer_tipo]] %||% '#4682B4'
      
      p <- plot_ly(chart_data_prep %>% filter(Seriousness == primer_tipo), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = primer_tipo,
                   marker = list(color = color_primer_tipo))
      
      # Agregar el resto de los tipos
      if(length(tipos_seriedad) > 1) {
        for(i in 2:length(tipos_seriedad)) {
          tipo <- tipos_seriedad[i]
          color_tipo <- colores_seriedad[[tipo]] %||% '#4682B4'
          
          p <- p %>% add_trace(
            data = chart_data_prep %>% filter(Seriousness == tipo),
            y = ~Report.Count, name = tipo,
            marker = list(color = color_tipo)
          )
        }
      }
    } else if("Year" %in% names(chart_data) && "Seriousness" %in% names(chart_data) && "Report.Count" %in% names(chart_data)) {
      # Formato alternativo
      # Colores para tipos de seriedad
      colores_seriedad <- list(
        "Serious" = '#FFB347',  # Naranja para serio
        "Death" = '#FF6B6B',    # Rojo para muerte
        "Non-Serious" = '#90EE90' # Verde para no serio
      )
      
      # Crear un gráfico con barras apiladas por tipo de seriedad
      tipos_seriedad <- unique(chart_data$Seriousness)
      
      # Iniciar con el primer tipo
      primer_tipo <- tipos_seriedad[1]
      color_primer_tipo <- colores_seriedad[[primer_tipo]] %||% '#4682B4'
      
      p <- plot_ly(chart_data %>% filter(Seriousness == primer_tipo), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = primer_tipo,
                   marker = list(color = color_primer_tipo))
      
      # Agregar el resto de los tipos
      if(length(tipos_seriedad) > 1) {
        for(i in 2:length(tipos_seriedad)) {
          tipo <- tipos_seriedad[i]
          color_tipo <- colores_seriedad[[tipo]] %||% '#4682B4'
          
          p <- p %>% add_trace(
            data = chart_data %>% filter(Seriousness == tipo),
            y = ~Report.Count, name = tipo,
            marker = list(color = color_tipo)
          )
        }
      }
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else if(tipo_reportero == "age_group" || tipo_reportero == "Age Group") {
    # Para datos por grupo de edad
    if("Year" %in% names(chart_data) && "Age Group" %in% names(chart_data) && "ReportCount" %in% names(chart_data)) {
      # Preparar datos para gráfico
      chart_data_prep <- chart_data %>%
        group_by(Year, `Age Group`) %>%
        summarise(Report.Count = sum(ReportCount, na.rm = TRUE), .groups = 'drop')
      
      # Crear un gráfico con barras apiladas por grupo de edad
      grupos_edad <- unique(chart_data_prep$`Age Group`)
      
      # Iniciar con el primer grupo
      p <- plot_ly(chart_data_prep %>% filter(`Age Group` == grupos_edad[1]), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = grupos_edad[1],
                   marker = list(color = '#4682B4'))
      
      # Colores para diferentes grupos
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      # Agregar el resto de los grupos con diferentes colores
      if(length(grupos_edad) > 1) {
        for(i in 2:min(length(grupos_edad), length(colores))) {
          p <- p %>% add_trace(
            data = chart_data_prep %>% filter(`Age Group` == grupos_edad[i]),
            y = ~Report.Count, name = grupos_edad[i],
            marker = list(color = colores[i])
          )
        }
      }
    } else if("Year" %in% names(chart_data) && "Age.Group" %in% names(chart_data) && "Report.Count" %in% names(chart_data)) {
      # Formato alternativo
      # Crear un gráfico con barras apiladas por grupo de edad
      grupos_edad <- unique(chart_data$Age.Group)
      
      # Iniciar con el primer grupo
      p <- plot_ly(chart_data %>% filter(Age.Group == grupos_edad[1]), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = grupos_edad[1],
                   marker = list(color = '#4682B4'))
      
      # Colores para diferentes grupos
      colores <- c('#4682B4', '#90EE90', '#D3D3D3', '#20B2AA', '#FFD700', '#FF6347', '#BA55D3')
      
      # Agregar el resto de los grupos con diferentes colores
      if(length(grupos_edad) > 1) {
        for(i in 2:min(length(grupos_edad), length(colores))) {
          p <- p %>% add_trace(
            data = chart_data %>% filter(Age.Group == grupos_edad[i]),
            y = ~Report.Count, name = grupos_edad[i],
            marker = list(color = colores[i])
          )
        }
      }
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else if(tipo_reportero == "sex" || tipo_reportero == "Sex") {
    # Para datos por sexo
    if("Year" %in% names(chart_data) && "Sex" %in% names(chart_data) && "ReportCount" %in% names(chart_data)) {
      # Preparar datos para gráfico
      chart_data_prep <- chart_data %>%
        group_by(Year, Sex) %>%
        summarise(Report.Count = sum(ReportCount, na.rm = TRUE), .groups = 'drop')
      
      # Colores para sexos
      colores_sexo <- list(
        "Female" = '#FF69B4',  # Rosa para femenino
        "Male" = '#4682B4',    # Azul para masculino
        "Not Specified" = '#D3D3D3' # Gris para no especificado
      )
      
      # Crear un gráfico con barras apiladas por sexo
      sexos <- unique(chart_data_prep$Sex)
      
      # Iniciar con el primer sexo
      primer_sexo <- sexos[1]
      color_primer_sexo <- colores_sexo[[primer_sexo]] %||% '#4682B4'
      
      p <- plot_ly(chart_data_prep %>% filter(Sex == primer_sexo), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = primer_sexo,
                   marker = list(color = color_primer_sexo))
      
      # Agregar el resto de los sexos
      if(length(sexos) > 1) {
        for(i in 2:length(sexos)) {
          sexo <- sexos[i]
          color_sexo <- colores_sexo[[sexo]] %||% '#4682B4'
          
          p <- p %>% add_trace(
            data = chart_data_prep %>% filter(Sex == sexo),
            y = ~Report.Count, name = sexo,
            marker = list(color = color_sexo)
          )
        }
      }
    } else if("Year" %in% names(chart_data) && "Sex" %in% names(chart_data) && "Report.Count" %in% names(chart_data)) {
      # Formato alternativo
      # Colores para sexos
      colores_sexo <- list(
        "Female" = '#FF69B4',  # Rosa para femenino
        "Male" = '#4682B4',    # Azul para masculino
        "Not Specified" = '#D3D3D3' # Gris para no especificado
      )
      
      # Crear un gráfico con barras apiladas por sexo
      sexos <- unique(chart_data$Sex)
      
      # Iniciar con el primer sexo
      primer_sexo <- sexos[1]
      color_primer_sexo <- colores_sexo[[primer_sexo]] %||% '#4682B4'
      
      p <- plot_ly(chart_data %>% filter(Sex == primer_sexo), 
                   x = ~Year, y = ~Report.Count, type = 'bar', 
                   name = primer_sexo,
                   marker = list(color = color_primer_sexo))
      
      # Agregar el resto de los sexos
      if(length(sexos) > 1) {
        for(i in 2:length(sexos)) {
          sexo <- sexos[i]
          color_sexo <- colores_sexo[[sexo]] %||% '#4682B4'
          
          p <- p %>% add_trace(
            data = chart_data %>% filter(Sex == sexo),
            y = ~Report.Count, name = sexo,
            marker = list(color = color_sexo)
          )
        }
      }
    } else {
      return(plot_ly() %>% 
               layout(title = "Estructura de datos inesperada para el gráfico"))
    }
  } else {
    # Para cualquier otro tipo no contemplado
    return(plot_ly() %>% 
             layout(title = paste("Tipo de reportero no soportado:", tipo_reportero)))
  }
  
  # Obtener el título del gráfico según el tipo de reportero
  titulo <- case_when(
    tipo_reportero == "report_type" ~ "Reports by Type",
    tipo_reportero == "reporter" ~ "Reports by Reporter",
    tipo_reportero == "reporter_region" ~ "Reports by Reporter Region",
    tipo_reportero == "report_serioursness" ~ "Reports by Seriousness",
    tipo_reportero == "age_group" ~ "Reports by Age Group",
    tipo_reportero == "sex" ~ "Reports by Sex",
    TRUE ~ paste("Reports by", tipo_reportero)
  )
  
  # Configuración común del layout - AQUÍ ESTÁ EL CAMBIO PARA LA LEYENDA A LA DERECHA
  p %>% layout(
    title = titulo,
    xaxis = list(title = "Year"),
    yaxis = list(title = "Report Count"),
    barmode = 'stack',
    # Colocar la leyenda a la derecha del gráfico
    legend = list(
      x = 1.05,  # Posición x (>1 para colocarla fuera del gráfico a la derecha)
      y = 0.5,   # Posición y (centrada verticalmente)
      xanchor = 'left',  # Ancla a la izquierda de la leyenda
      yanchor = 'middle',  # Ancla al medio de la leyenda
      orientation = 'vertical',  # Orientación vertical
      bgcolor = 'rgba(255, 255, 255, 0.7)',  # Fondo semitransparente
      bordercolor = 'rgba(0, 0, 0, 0.1)',    # Borde ligero
      borderwidth = 1
    ),
    # Ajustar márgenes para dejar espacio a la leyenda
    margin = list(l = 50, r = 150, b = 50, t = 50, pad = 4)
  )
}


# Función para preparar los datos para el panel de información
crear_panel_informacion <- function(datos_resumen) {
  # Formatear números con separadores de miles
  total_formateado <- format(datos_resumen$total, big.mark = ",")
  serious_formateado <- format(datos_resumen$serious, big.mark = ",")
  death_formateado <- format(datos_resumen$death, big.mark = ",")
  
  # Crear elemento div para cada panel de información
  total_reports <- div(
    div(style = "font-weight: bold; margin-bottom: 5px;", "Total Reports"),
    div(style = "font-size: 1.5em;", HTML(paste0('<i class="fa fa-chart-line"></i> ', total_formateado)))
  )
  
  serious_reports <- div(
    div(style = "font-weight: bold; margin-bottom: 5px;", "Serious Reports (excluding death)"),
    div(style = "font-size: 1.5em; color: #FF8C00;", HTML(paste0('<i class="fa fa-exclamation-triangle"></i> ', serious_formateado)))
  )
  
  death_reports <- div(
    div(style = "font-weight: bold; margin-bottom: 5px;", "Death Reports"),
    div(style = "font-size: 1.5em; color: #FF0000;", HTML(paste0('<i class="fa fa-skull-crossbones"></i> ', death_formateado)))
  )
  
  return(list(
    total = total_reports,
    serious = serious_reports,
    death = death_reports
  ))
}

# Operador de NULL coalescing para R
"%||%" <- function(x, y) if (is.null(x)) y else x
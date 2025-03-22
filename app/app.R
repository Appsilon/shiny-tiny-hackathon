# app.R - Archivo principal de la aplicación FAERS
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(arrow)
library(tidyr)

# Cargar los módulos
source("modules/data_functions.R")
source("modules/ui_elements.R")
source("modules/server_functions.R")
source("modules/config.R")

# Inicializar datos para el resumen
datos_resumen <- inicializar_datos_resumen()

# UI principal completa
ui <- fluidPage(
  # Incluir estilos personalizados
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Incluir FontAwesome para los iconos si no está ya incluido por shiny
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  # Componentes principales
  headerUI(),
  summaryStatsUI(inicializar_datos_resumen()),
  yearButtonsUI(),
  mainContentUI()
)

# Servidor completo
server <- function(input, output, session) {
  # Valor reactivo para el período de tiempo
  periodo <- reactiveVal("all")
  
  # Observadores para los botones de período
  observeEvent(input$allYears, {
    periodo("all")
    updateTabsetPanel(session, "periodoPanel", selected = "allYearsTab")
  })
  
  observeEvent(input$last10Years, {
    periodo("last10")
    updateTabsetPanel(session, "periodoPanel", selected = "last10YearsTab")
  })
  
  # Datos de resumen para los paneles informativos
  datos_resumen <- reactive({
    inicializar_datos_resumen()
  })
  
  # Renderizar paneles de información
  output$totalReportsValue <- renderUI({
    paneles <- crear_panel_informacion(datos_resumen())
    paneles$total
  })
  
  output$seriousReportsValue <- renderUI({
    paneles <- crear_panel_informacion(datos_resumen())
    paneles$serious
  })
  
  output$deathReportsValue <- renderUI({
    paneles <- crear_panel_informacion(datos_resumen())
    paneles$death
  })
  
  # Obtener datos según filtros
  datos_filtrados <- reactive({
    obtener_datos_filtrados(input$reporterType, periodo())
  })
  
  # Renderizar tabla
  output$reportTable <- renderDT({
    datos <- datos_filtrados()
    crear_tabla_datos(datos, input$reporterType)
  })
  
  # Obtener datos para el gráfico
  datos_grafico <- reactive({
    obtener_datos_grafico(input$reporterType, periodo())
  })
  
  # Renderizar gráfico
  output$barChart <- renderPlotly({
    crear_grafico_barras(datos_grafico(), input$reporterType)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
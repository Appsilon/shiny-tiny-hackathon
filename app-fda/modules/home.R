

box::use(
  bslib[card, layout_column_wrap, card_header, card_body, value_box, card_footer],
  shiny[NS, moduleServer, selectInput, tagList, radioButtons, reactive,
        renderText, textOutput],
  bsicons[bs_icon],
  reactable[reactable, renderReactable, reactableOutput, colDef, colFormat],
  utils[head],
  scales[label_comma],
  echarts4r[e_add_nested, echarts4rOutput, e_x_axis, e_legend, e_color,
            e_labels, e_y_axis, e_charts, e_tooltip, e_bar, renderEcharts4r,
            e_grid],
  tidyr[pivot_longer],
  dplyr[filter, group_by, mutate, arrange, across, where]
)

ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    layout_column_wrap(fill = F,
      width = 1/3,
      value_box(
        title = "Total Reports",
        value = textOutput(ns("txt_total")),
        showcase = bs_icon("graph-up-arrow", size = 35),
        theme = "primary"
        ),
      value_box(
        title = "Serious Reports (excluding death)",
        value = textOutput(ns("txt_serious")),
        showcase = bs_icon("exclamation-circle-fill", size = 35),
        theme = "warning"
      ),
      value_box(
        title = "Death Reports",
        value = textOutput(ns("txt_death")),
        showcase = bs_icon("x-circle-fill", size = 35), 
        theme = "danger" 
      )
    ),
    layout_column_wrap(
      fill = F,
      fillable = F,
      width = 1/3,
      inline = T,
      selectInput(
        inputId = ns("reports"),
        label = "Reports by",
        choices = c(
          "Report Type" = "report_type",
          "Reporter" = "reporter",
          "Age Group" = "age_group",
          "Sex" = "sex",
          "Report Seriousness" = "report_seriousness",
          "Report Region" = "report_region"
          )
      ),
      radioButtons(
        inputId = ns("radio"),
        label = "Options",
        choices = list(
          "All Years" = "all_years",
          "Last 10 Years" = "last_10_years"
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = T,
        card_body(
          reactableOutput(ns("tbl_datos"))
        )
      ),
      card(
        full_screen = T,
        card_body(
          echarts4rOutput(ns("plt_datos"))
        )
      )
      
    )


  )
  
}

server <- function(id, db_report_type, db_reporter, db_reporter_region,
                   db_report_seriousness, db_report_age_group, db_sex){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    datos <- reactive({

      if(input$reports == "report_type"){

        datos <- db_report_type

      } else if(input$reports == "reporter"){
        
        datos <- db_reporter
        
      } else if(input$reports == "age_group"){
        
        datos <- db_report_age_group
        
      } else if(input$reports == "sex"){
        
        datos <- db_sex
        
      } else if(input$reports == "report_seriousness"){
        
        datos <- db_report_seriousness
        
      } else if(input$reports == "report_region"){
        
        datos <- db_reporter_region
        
      }
      
      if(input$radio == "all_years"){
        datos
      } else{
        datos <- datos |> 
          head(10)
      }

      datos
    })
    
    output$txt_total   <- renderText({
      
      total <- sum(datos()$total_reports, na.rm = T)
      label_comma()(total)
      
    })
    output$txt_serious <- renderText({
      
      total <- 16664479
      label_comma()(total)
      
    })
    output$txt_death   <- renderText({
      
      total <- 2722806
      label_comma()(total)
      
    })
    output$tbl_datos   <- renderReactable({

      datos <- datos() |> 
        mutate(across(
          where(is.numeric) & !any_of("year"),
          label_comma()
        ))
      
      reactable(
        data = datos,
        compact = T,
        sortable = F,
        defaultPageSize = 60,
        style = list(fontSize = "0.8rem"),
        columns = list(
          year = colDef(name = "Year", width = 60),
          total_reports = colDef(name = "Total Reports", format = colFormat(separators = T))
        )
        )
      
    })
    
    output$plt_datos   <- renderEcharts4r({
      
      df_datos <- datos() |> 
        pivot_longer(-year, names_to = "names", values_to = "values") |> 
        filter(names != "total_reports") |> 
        mutate(year = as.character(year)) |> 
        arrange(year)
      
      df_datos |>
        group_by(names) |> 
        e_charts(year) |>
        e_bar(values, stack = "group") |> 
        e_tooltip() |> 
        e_grid(
          left = "5%", 
          right = "5%", 
          top = "10%", 
          bottom = "10%"
        )

      
    })
    
  })
}
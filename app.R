library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Simulated FDA Inspections Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fiscal_year", "Fiscal Year:",
                  choices = c("All", sort(unique(simulated_data$FiscalYear))),
                  selected = "All"),
      selectInput("product_type", "Product Type:",
                  choices = c("All", unique(simulated_data$ProductType)),
                  selected = "All"),
      selectInput("classification", "Classification:",
                  choices = c("All", "NAI", "VAI", "OAI"),
                  selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 plotOutput("inspections_plot"),
                 plotOutput("classification_plot")),
        tabPanel("Details",
                 dataTableOutput("inspection_table"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- simulated_data
    if (input$fiscal_year != "All") {
      data <- data %>% filter(FiscalYear == input$fiscal_year)
    }
    if (input$product_type != "All") {
      data <- data %>% filter(ProductType == input$product_type)
    }
    if (input$classification != "All") {
      data <- data %>% filter(Classification == input$classification)
    }
    data
  })
  
  output$inspections_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(FiscalYear) %>%
      summarise(Count = n())
    
    ggplot(data, aes(x = FiscalYear, y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Number of Inspections per Fiscal Year",
           x = "Fiscal Year", y = "Number of Inspections")
  })
  
  output$classification_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(Classification) %>%
      summarise(Count = n())
    
    ggplot(data, aes(x = Classification, y = Count, fill = Classification)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Inspection Classifications",
           x = "Classification", y = "Number of Inspections")
  })
  
  output$inspection_table <- renderDataTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)

require(glue)

## create mock data
source(glue('{getwd()}/data/mock-data.R'))

## load server and ui
source(glue('{getwd()}/ui.R'))
source(glue('{getwd()}/server.R'))

shiny::shinyApp(ui = ui, server = server)

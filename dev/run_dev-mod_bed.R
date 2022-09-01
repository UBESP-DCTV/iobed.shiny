library(shiny)
library(shinyjs)
options(shiny.reactlog = TRUE)

golem::detach_all_attached()
golem::document_and_reload(export_all = TRUE)

ui <- mod_bed_ui("bed_1")

server <- function(input,output,session){
  mod_bed_server("bed_1")
}

shinyApp(ui, server)

library(shiny)

golem::detach_all_attached()
golem::document_and_reload(export_all = TRUE)

ui <- mod_video_test_ui("video_test_1")

server <- function(input,output,session){
  mod_video_test_server("video_test_1")
}

shinyApp(ui, server)

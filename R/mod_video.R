#' video UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_video_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' video Server Functions
#'
#' @noRd 
mod_video_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_video_ui("video_1")
    
## To be copied in the server
# mod_video_server("video_1")

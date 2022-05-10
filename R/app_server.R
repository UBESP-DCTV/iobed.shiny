#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_bed_server("bed_1")
  mod_video_server("video_1")
  mod_watch_server("watch_1")
}

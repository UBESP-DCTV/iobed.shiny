#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    #
    dashboardPage(
      title = " I.O.BED Shiny sensor",

      dashboardHeader(title = "I.O.BED sensors"),

      dashboardSidebar(
        sidebarMenu(
          menuItem("1. Bed", tabName = "bed", icon = icon("bed")),
          menuItem("2. Video", tabName = "video", icon = icon("video")),
          menuItem("3. Watch", tabName = "watch", icon = icon("stopwatch"))
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(
            tabName = "bed",
            h2("Bed tab content"),
            mod_bed_ui("bed_1")
          ),


          tabItem(
            tabName = "video",
            h2("Video tab content"),
            mod_video_ui("video_1")
          ),


          tabItem(
            tabName = "watch",
            h2("Watch tab content"),
            mod_watch_ui("watch_1")
          )
        )
      ),

      # Main dashboard skin colour
      skin = "red"
    )
  )
}







#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'iobed.shiny'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


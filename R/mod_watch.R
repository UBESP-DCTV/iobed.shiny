#' watch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_watch_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      list_to_p(c("Foo", "Bar")),
      box(
        title = "Inputs", status = "warning", solidHeader = TRUE,
        textInput(ns("foo"), "Foo: ")
      ),
      box(
        title = "Output", status = "info", solidHeader = TRUE,
        textOutput(ns("out_txt"))
      )
    )
  )
}

#' watch Server Functions
#'
#' @noRd
mod_watch_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$out_txt <- renderText({
      input[["foo"]]
    })
  })
}

## To be copied in the UI
# mod_watch_ui("watch_1")

## To be copied in the server
# mod_watch_server("watch_1")

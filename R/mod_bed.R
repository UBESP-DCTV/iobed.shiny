#' bed UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import iobed.bed
#' @importFrom shiny NS tagList
mod_bed_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      list_to_p(c(
        "In this interface, you can start and stop record bed signals"
      )),

      box(
        title = "Parameters", status = "warning", solidHeader = TRUE,
        textInput(ns("pid"), "Person's ID: "),
        selectInput(
          ns("bedPort"),
          label = "Bed connection port: ",
          choices = serial::listPorts(),
          selected = serial::listPorts()[[1L]]
        ),
        actionButton(ns("bedTestConnect"), "Test connection"),

        actionButton(ns("bedStart"), "Start"),
        actionButton(ns("bedStop"), "Stop"),
      ),
      box(
        title = "Output", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("con")),
      )
    )
  )
}

#' bed Server Functions
#'
#' @noRd
mod_bed_server <- function(id){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    port <- reactive(
      req(input[["bedPort"]])
    )

    test_out <- eventReactive(input[["bedTestConnect"]], {

      con <- reactive(
        iobed.bed::bed_connection(port())
      )

      Sys.sleep(1)

      iobed.bed::pull_bed_stream(con()) |>
        iobed.bed::tidy_iobed_stream()

    })

    output$con <- renderDataTable(tibble::as_tibble(test_out()))
    # output$output <- renderDataTable({test_out()})

    output$is_connected <- renderText({
      glue::glue()
    })

    output$out_txt <- renderText({
      input[["foo"]]
    })
  })
}

## To be copied in the UI
# mod_bed_ui("bed_1")

## To be copied in the server
# mod_bed_server("bed_1")

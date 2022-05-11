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
        title = "setup", status = "info", solidHeader = TRUE,
        checkboxGroupInput(
          ns("checks"),
          "Check all the preliminar operations done",
          c(
            "Plug the bed" = "plugged",
            "Switch on the bed" = "on",
            "Switch on the balance" = "balance",
            "Connect serial to PC" = "connected"
          )
        )
      ),
      box(
        title = "Parameters", status = "warning", solidHeader = TRUE,
        textInput(ns("pid"), "Person's ID: "),
        selectInput(
          ns("bedPort"),
          label = "Bed connection port: ",
          choices =  tryCatch(
            serial::listPorts(),
            error = function(e) "No port available"
          ),
          selected = tryCatch(
            serial::listPorts(),
            error = function(e) "No port available"
          )[[1L]]
        ),
        actionButton(ns("bedTestConnect"), "Test connection"),

        actionButton(ns("bedStart"), "Start"),
        actionButton(ns("bedStop"), "Stop"),
        actionButton(ns("save"), "Write results"),
      ),
      box(
        title = "Selected parameters", status = "info", width = 12,
        textOutput(ns("out_port")),
        textOutput(ns("out_folder"))
      )
    ),
    fluidRow(
      box(
        title = "Test output", status = "info", solidHeader = TRUE,
        tableOutput(ns("out_tbl"))
      ),
      box(
        title = "Final result", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("res_tbl"))
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

    test_out <- eventReactive(input[["bedTestConnect"]], {

      if (exists("con") && serial::isOpen(con)) stop()

      con <- tryCatch(
        iobed.bed::bed_connection(input[["bedPort"]]),
        error = function(e) FALSE
      )

      if (isFALSE(con)) {
        tibble(error = "Connection not established!")
      } else {
        open(con)
        cat(glue::glue("connection is open: {serial::isOpen(con)}.\n"))

        Sys.sleep(3)
        print(con)

        iobed.bed::pull_bed_stream(con) |>
          iobed.bed::tidy_iobed_stream()
      }

    })




    observeEvent(input[["bedStart"]], {

      if (exists("con") && serial::isOpen(con)) close(con)

      con <<- tryCatch(
        iobed.bed::bed_connection(input[["bedPort"]]),
        error = function(e) FALSE
      )

      open(con)
      cat(glue::glue("connection is open: {serial::isOpen(con)}.\n\n"))
      print(con)

    })

    res <- eventReactive(input[["bedStop"]], {
      result <- iobed.bed::pull_bed_stream(con) |>
        iobed.bed::tidy_iobed_stream()
      cat(glue::glue("connection is open: {serial::isOpen(con)}.\n\n"))
      result
    })

    filepath <- reactive({
      req(input[["pid"]])

      today_now <- Sys.time() |>
        stringr::str_remove_all("\\W")
      patient_id <- input[["pid"]]
      normalizePath(path.expand(file.path(
        ".", "data", paste0(today_now, "-", patient_id, "-bed.rds")
      )), mustWork = FALSE)
    })

    observeEvent(input[["save"]], {
      req(res)
      req(filepath)

      cat(glue::glue("RDS to write on {filepath()}.\n"))
      readr::write_rds(res(), filepath())
      cat(glue::glue("RDS written on {filepath()}.\n"))

    })

    output$out_folder <- renderText(
      glue::glue("Outup folder is: {filepath()}")
    )

    output$out_port <- renderText(
      glue::glue("Output port selected is: {input[['bedPort']]}")
    )

    output$out_tbl <- renderTable(test_out())
    output$res_tbl <- DT::renderDT(res())

  })
}

## To be copied in the UI
# mod_bed_ui("bed_1")

## To be copied in the server
# mod_bed_server("bed_1")

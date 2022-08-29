#' watch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import iobed.watch
#' @importFrom shiny NS tagList
mod_watch_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      list_to_p(c("In this interface, you can load, parse and store watch signals")),
      box(
        title = "setup", status = "info", solidHeader = TRUE,
        checkboxGroupInput(
          ns("checks"),
          "Check all the preliminar operations done",
          c(
            "Switch on the app on both watchs" = "on",
            "Dominant wirst is the blue one" = "dominant",
            "Stop the record on both watches" = "stop",
            "Connect one watch a time with the cable" = "connected",
            "Select \"Use mass storage\"" = "storage"
          )
        )
      ),
      box(
        title = "Parameters", status = "warning", solidHeader = TRUE,
        textInput(ns("pid"), "Person's ID: "),
        selectInput(
          ns("wrist"),
          label = "Current wrist: ",
          choices = c(
            "Dominant - Blue" = "blue",
            "Non-dominant - black" = "black"
          )
        ),
        shiny::fileInput(
          ns("file"),
          "Select a FIT file",
          accept = ".fit"
        ),

        actionButton(ns("pull"), "Pull & Process FIT"),
        actionButton(ns("save"), "Write results"),
      ),
      box(
        title = "Selected parameters", status = "info", width = 12,
        textOutput(ns("input_fit")),
        textOutput(ns("out_folder"))
      )
    ),
    fluidRow(
      box(
        title = "Parsed FIT", status = "info", solidHeader = TRUE,
        width = 12,
        DT::DTOutput(ns("out_fit"))
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


    res <- eventReactive(input[["pull"]], {
      fit <- req(input[["file"]])
      pid <- req(input[["pid"]])
      req(filepath)

      cat(glue::glue("File used is: {fit[['datapath']]}\n\n"))
      cat(glue::glue(
        "Is FIT: {stringr::str_detect(fit[['datapath']], '[Ff][Ii][Tt]$')}\n\n"
      ))
      utils::str(fit[['datapath']], 1)

      first <- fit[["datapath"]][[1L]] |>
        iobed.watch::load_fit(dirname(filepath()), overwrite = TRUE)
      cat(glue::glue("first is : {first}\n\n"))

      first_fixed <- file.path(dirname(first), fit[["name"]])
      file.rename(first, first_fixed)

      second <- iobed.watch::fit2datacsv(first_fixed)
      cat(glue::glue("second is : {second}\n\n"))

      third <- iobed.watch::parse_fit_data(second)
      cat(glue::glue("third is : {utils::str(third, 1)}\n\n"))

      third
    })

    output$out_fit <- DT::renderDT(res())


  observeEvent(input[["save"]], {
    req(res)
    req(filepath)

    cat(glue::glue("RDS to write on {filepath()}.\n"))
    readr::write_rds(res(), filepath())
    cat(glue::glue("RDS written on {filepath()}.\n"))

  })










    filepath <- reactive({
      req(input[["pid"]])
      req(input[["wrist"]])

      today_now <- Sys.time() |>
        stringr::str_remove_all("\\W")

      patient_id <- input[["pid"]]
      patient_wrist <- input[["wrist"]]

      normalizePath(path.expand(file.path(
        ".", "data", paste0(
          today_now, "-", patient_id, "-",
          patient_wrist, "_watch.rds"
        )
      )), mustWork = FALSE)
    })

    observeEvent(input[["save"]], {
      # req(res)
      req(filepath)

      cat(glue::glue("RDS to write on {filepath()}.\n"))
      readr::write_rds(res(), filepath())
      cat(glue::glue("RDS written on {filepath()}.\n"))

    })

    output$out_folder <- renderText(
      glue::glue("Output folder is: {filepath()}")
    )
    output$input_fit <- renderText(
      glue::glue("Input FIT file is: {input[['file']]$datapath}")
    )

  })
}

## To be copied in the UI
# mod_watch_ui("watch_1")

## To be copied in the server
# mod_watch_server("watch_1")

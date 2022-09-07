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
  shinyjs::useShinyjs()

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

        # actionButton(ns("set"), "Accept settings"),
        actionButton(ns("bedTestConnect"), "Test connection"),

        actionButton(ns("bedStart"), "Start"),
        actionButton(ns("bedStop"), "Stop & Save"),
      ),

      box(
        title = "Selected parameters", status = "info", width = 12,
        textOutput(ns("out_port")),
        textOutput(ns("out_folder"))
      )
    ),
    fluidRow(
      box(
        title = "Status", status = "info", solidHeader = TRUE,
        width = 12,
        textOutput(ns("status"))
      ),
      box(
        title = "Test output", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("out_tbl"))
      ),
      box(
        title = "Final result (head)", status = "info", solidHeader = TRUE,
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



# Setup -----------------------------------------------------------

    stopifnot(
      `Status bed exists` = exists("status_bed", envir = .GlobalEnv)
    )
    status_bed <- get("status_bed", envir = .GlobalEnv)



# Reactives -------------------------------------------------------

    current_status <- reactive({
      invalidateLater(1e3)
      get_status(status_bed)
    })



    filepath <- reactive({
      validate(need(input[["pid"]], "PID must be provided."))
      message('re-evaluating filepath after button start pressing')

      today_now <- now()
      pid <- input[["pid"]]
      here::here("data", pid, glue::glue("{now()}-{pid}-bed.rds")) |>
        normalizePath(mustWork = FALSE)
    })

    fire_starting(status_bed)




# Preview ---------------------------------------------------------

    test_out <- reactive({
      validate(need(input[["bedPort"]], "Bed port must be provided."))

      if (would_start_when_running(status_bed)) return(NULL)

      usethis::ui_done(
        "(re-)evaluating test_out after button test pressing"
      )

      showNotification(
        "Test for bed connection is running.
        Resulting table should be appear in a while.
        ",
        type = "message",
        duration = 10
      )

      close_if_open_connection("bed_con")

      bed_con <- tryCatch(
        iobed.bed::bed_connection(input[["bedPort"]]),
        error = function(e) FALSE
      )
      if (isFALSE(check_connection(bed_con))) return(NULL)

      open(bed_con)
      withr::defer(close(bed_con))
      usethis::ui_done("Opening bed connection.")
      usethis::ui_info("Connection is open: {serial::isOpen(bed_con)}.")

      Sys.sleep(3)
      summary(bed_con)

      tryCatch({
          res <- tryTwice_pull_and_tidy(bed_con)
          fire_ready(status_bed)
          res
        },
        warning = function(e) test_failed_or_not_run(session = session),
        error = function(e) test_failed_or_not_run(session = session)
      )

    }) |>
      bindEvent(input[["bedTestConnect"]])


# Recording --------------------------------------------------------

    observe({
      usethis::ui_done('Button start clicked')
      bedPort <- req(input[["bedPort"]])
      pid <- req(input[["pid"]])
      .filepath <- filepath()

      if (is_status(status_bed, "starting")) {
        return(test_failed_or_not_run(session = session))
      }
      if (would_start_when_running(status_bed)) return(NULL)
      if (would_start_not_ready(status_bed)) return(NULL)

      close_if_open_connection("bed_con")

      usethis::ui_todo("{{future}} is running!")
      fopts <- golem::get_golem_options()
      res <- future::future(seed = TRUE, {
        options(fopts)

        bed_con <- tryCatch(
          iobed.bed::bed_connection(bedPort),
          error = function(e) FALSE
        )
        if (isFALSE(check_connection(bed_con, session))) {
          usethis::ui_warn("Error in checking the connection.")
          return(NULL)
        }


        open(bed_con)
        withr::defer(close(bed_con))

        fire_running(status_bed)
        usethis::ui_done("Streaming is ON")

        i <- 1
        while (status_bed |> is_status("running")) {
          fire_running(
            status_bed,
            round(1 - 1/sqrt(i/50), 2 + log10(i)) * 100
          )
          Sys.sleep(1)
          i[[1]] <- i[[1]] + 1
        }
        usethis::ui_done("Recording interrupted!")

        res_tbl <- tryTwice_pull_and_tidy(bed_con)

        usethis::ui_todo("writing rds bed data tbl")
        fs::dir_create(dirname(.filepath), recurse = TRUE)
        readr::write_rds(res_tbl, .filepath)

        usethis::ui_done("Elaborating bed table")
        usethis::ui_done("Writing bed data on {.filepath}.")

        .filepath

      })

      res <- promises::catch(
        res,
        function(e) {
          usethis::ui_warn(e$message)
          fire_strange(status_bed)
          showNotification(e$message, type = "warning")
        }
      )

      res <- promises::finally(
        res,
        function() {
          if (!is_status(status_bed, "strange")) fire_ready(status_bed)
        }
      )

      # Return something other than the promise so shiny remains
      # responsive
      NULL
    }) |>
      bindEvent(input[["bedStart"]])




    observe({
      usethis::ui_done("Button stop clicked")

      if (is_status(status_bed, "starting")) {
        return(test_failed_or_not_run(session = session))
      }
      if (would_stop_stopped(status_bed)) return(NULL)
      if (would_stop_interrupted(status_bed)) return(NULL)

      fire_interrupt(status_bed)
      showNotification("Bed recording cycle stopped")
    }) |>
      bindEvent(input[["bedStop"]])



# Outputs ---------------------------------------------------------

    output$out_folder <- renderText(
      glue::glue("Outup folder is: {filepath()}")
    )

    output$out_port <- renderText(
      glue::glue("Output port selected is: {input[['bedPort']]}")
    )

    output$out_tbl <- DT::renderDT(test_out())

    output$res_tbl <- DT::renderDT({
      if (!fs::file_exists(filepath())) return(NULL)
      readr::read_rds(filepath())
    })

    output$status <- renderText({
      paste0("Current status: ", current_status())
    })

  })
}

## To be copied in the UI
# mod_bed_ui("bed_1")

## To be copied in the server
# mod_bed_server("bed_1")

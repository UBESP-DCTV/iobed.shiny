#' video UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import iobed.video
#' @import shinyjs
#' @importFrom shiny NS tagList
mod_video_ui <- function(id){
  useShinyjs()
  ns <- NS(id)
  tagList(
    fluidRow(
      list_to_p(c("In this interface, you can start/stop video recording")),
      box(
        title = "setup", status = "info", solidHeader = TRUE,
        checkboxGroupInput(
          ns("checks"),
          "Check all the preliminar operations done",
          c(
            "Connect the webcam to an USB port" = "connection",
            "Select the index of the camera (e.g. '0' is default camera, often external one is '1')" = "index",
            "Place the camera in a stable position" = "position"
          )
        )
      ),
      box(
        title = "Parameters", status = "warning", solidHeader = TRUE,
        textInput(ns("pid"), "Person's ID: "),
        numericInput(
          ns("index"),
          "Camera index: ",
          min = 0,
          value = 0,
          step = 1
        ),
        sliderInput(
          ns("max_time"),
          "Max seconds to record: ",
          min = 60,
          max = 3600,
          value = 1800,
          step = 60
        ),
        numericInput(
          ns("fps"),
          "Camera frames/second: ",
          min = 1,
          max = 60,
          value = 30,
          step = 1
        ),
        actionButton(ns("preview"), "Take snapshot"),
        actionButton(ns("clear_preview"), "Clear snapshot"),
        actionButton(ns("start"), "Start"),
        actionButton(ns("stop"), "Stop"),
        checkboxInput(ns("save"), "Save PNGs")
      ),
      box(
        title = "Selected parameters", status = "info", width = 12,
        textOutput(ns("out_index")),
        textOutput(ns("out_file")),
        textOutput(ns("out_res_str")),
        textOutput(ns("recording")),
        textOutput(ns("recording_frame"))
      )
    ),
    fluidRow(
      box(
        title = "Camera Preview", status = "info", solidHeader = TRUE,
        width = 12,
        plotOutput(ns("snapshot"))
      )
    )
  )
}

#' video Server Functions
#'
#' @noRd
mod_video_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    test_out <- eventReactive(input[["preview"]], {
      req(input[["max_time"]])
      req(input[["fps"]])
      req(input[["index"]])

      op <- options(digits.secs = 6)
      withr::defer(options(op))

      my_stream <- Rvision::stream(index = input[["index"]])
      withr::defer(Rvision::release(my_stream))

      Rvision::readNext(my_stream)
    })

    output$snapshot <- renderPlot(plot(test_out()))

    observeEvent(
      input[["clear_preview"]],
      shinyjs::hide(ns("snapshot"))
    )


    recording <- reactiveValues(
      on = FALSE,
      frame = 1
    )
    observeEvent(
      input[["start"]], {
      recording$on <- TRUE
      recording$frame <- 1
    })
    observeEvent(input[["stop"]], {
      recording$on <- FALSE
    })

   observeEvent(input[["start"]], {
      req(input[["max_time"]])
      req(input[["fps"]])
      req(input[["index"]])
      req(recording)
      recording$on <- TRUE

      n_frames <- input[["max_time"]] * input[["fps"]]

      op <- options(digits.secs = 6)
      withr::defer(options(op))

      my_stream <- Rvision::stream(index = input[["index"]])
      withr::defer(Rvision::release(my_stream))

      my_queue <<- my_stream |>
        Rvision::queue(size = n_frames, overflow = "replace")
    })

   observeEvent(input[["stop"]], {
      req(input[["pid"]])
      req(filepath)
      req(input[["max_time"]])
      req(input[["fps"]])
      req(recording)

      recording$on <- FALSE
      n_frames <- input[["max_time"]] * input[["fps"]]

      frames <- vector("list", n_frames)
      names <- vector("character", n_frames)

      for (frame in seq_len(length(my_queue))) {
        frames[[frame]] <- Rvision::readNext(my_queue)

        names[[frame]] <- Sys.time() |>
          stringr::str_replace_all(c(
            `-|:` = "",
            ` `="t",
            `\\.`="m"
          ))

        if (input[["save"]]) {
          dir_name <- dirname(filepath())
          base_name <- basename(filepath()) |>
            stringr::str_replace_all("\\.rds", paste0("_", frame, ".png"))
          dir_path <- file.path(dir_name, input[["pid"]])
          dir.create(dir_path, showWarnings = FALSE)
          framepath <- file.path(dir_path, base_name) |>
            fs::path_expand()
          suppressMessages(
            Rvision::write.Image(frames[[frame]], framepath)
          )
        }

        recording$frame <- frame
      }

      output$snapshot <- renderPlot(plot(frames[[1]]))
      res <- purrr::set_names(frames, names)

      readr::write_rds(res, filepath())
      cat(glue::glue("RDS written on {filepath()}.\n"))

      output$out_res_str <- renderText(
        glue::glue("recorded {frame} frames on disk")
      )

    })



    output$recording <- renderText(
      glue::glue("Recording is ON: {recording$on}")
    )
    output$recording_frame <- renderText(
      glue::glue("Recording frame number: {recording$frame}")
    )




    filepath <- reactive({
      req(input[["pid"]])

      today_now <- Sys.time() |>
        stringr::str_remove_all("\\W")

      patient_id <- input[["pid"]]

      normalizePath(path.expand(file.path(
        ".", "data", paste0(
          today_now, "-", patient_id, "-video.rds"
        )
      )), mustWork = FALSE)
    })

    # observeEvent(input[["save"]], {
    #   req(res)
    #   req(filepath)
    #
    #   readr::write_rds(res(), filepath())
    #   cat(glue::glue("RDS written on {filepath()}.\n"))
    #
    # })

    output$out_file <- renderText(
      glue::glue("Output file is: {filepath()}")
    )

    output$out_index <- renderText(
      glue::glue("Camera index is: {input[['index']]}")
    )

  })
}

## To be copied in the UI
# mod_video_ui("video_1")

## To be copied in the server
# mod_video_server("video_1")

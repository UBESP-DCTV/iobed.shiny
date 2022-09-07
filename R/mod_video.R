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
  shinyjs::useShinyjs()

  ns <- NS(id)
  tagList(
    fluidRow(
      list_to_p(
        "In this interface, you can start/stop video recording"
      ),

      box(
        title = "setup", status = "info", solidHeader = TRUE,
        checkboxGroupInput(
          ns("checks"),
          "Check all the preliminar operations done",
          c(
            "Connect the webcam to an USB port" = "connection",
            "Select camera index (internal: 0, external: 1+)" = "index",
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
        # actionButton(ns("set"), "Accept settings"),
        actionButton(ns("preview"), "Take snapshot"),
        actionButton(ns("clear_preview"), "Clear snapshot"),
        actionButton(ns("start"), "Start recording"),
        actionButton(ns("stop"), "Stop recording")
      ),

      box(
        title = "Selected parameters", status = "info", width = 12,
        textOutput(ns("camera_idx")),
        textOutput(ns("out_file")),
        textOutput(ns("out_res_str")),
        textOutput(ns("recording")),
      )
    ),


    fluidRow(
      box(
        title = "Status", status = "info", solidHeader = TRUE,
        width = 12,
        textOutput(ns("status"))
      ),
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
#' @importFrom shinyjs hide disable
#' @noRd
mod_video_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns




# Setup -----------------------------------------------------------


    stopifnot(
      `Status video exists` = exists("status_video", envir = .GlobalEnv)
    )
    status_video <- get("status_video", envir = .GlobalEnv)


    my_writer <- NULL
    my_stream <- NULL
    my_buffer <- NULL




# Reactives -------------------------------------------------------

    current_status <- reactive({
      invalidateLater(1e3)
      get_status(status_video)
    })


    out_folder <- reactive({
      validate(need(input[["pid"]], "pid must be provided"))
      here::here("data", input[['pid']]) |>
        normalizePath(mustWork = FALSE)
    })

    fire_starting(status_video)
    #
    # observe({
    #   message("Button Set clicked")
    #   validate(need(input[["pid"]], "pid must be provided"))
    #   validate(need(input[["index"]], "index must be provided"))
    #
    #   disable(ns("pid"))
    #   disable(ns("index"))
    #
    # }) |>
    #   bindEvent(input[["set"]])




# Preview ---------------------------------------------------------

    test_out <- reactive({
      validate(need(input[["index"]], "index must be provided"))
      my_stream <- tryCatch(
        Rvision::stream(input[["index"]]),
        error = function(e) FALSE
      )
      if (!Rvision::isStream(my_stream)) {
        showNotification(HTML(paste(collapse = "</br>",
          "Error in creating the Stream.",
          "Possibly wrong port selected for the camera; try a different one.",
          "If the issue persist, please, contact Corrado.Lanera@ubep.unipd.it"
        )))
        usethis::ui_warn("Wrong port selected")
        return(NULL)
      }
      withr::defer(Rvision::release(my_stream))
      frame <- Rvision::readNext(my_stream)
      fire_ready(status_video)
      frame
    }) |>
      bindEvent(input[["preview"]])


    observe({
      usethis::ui_warn("!!!  THIS DOESN'T WORK; DON'T KNOW WHY !!!")
      hide(ns("snapshot"))
    }) |>
      bindEvent(input[["clear_preview"]])




# Recordings ------------------------------------------------------

    observe({
      message("Button start clicked")
      req(input[['index']])

      message("Setting digits.secs option")
      op <- options(digits.secs = 6)
      withr::defer(options(op))

      if (is_status(status_video, "starting")) {
        return(test_failed_or_not_run(session = session))
      }
      if (would_start_when_running(status_video)) return(NULL)
      if (would_start_not_ready(status_video)) return(NULL)

      fs::dir_create(out_folder())
      message("Output directory created/checked.")

      showNotification(
        "Recording started!",
        type = "message",
        duration = 10
      )
      message("Recording started.")

      pid <- input[["pid"]]
      index <- input[["index"]]
      out_dir <- out_folder()

      res <- future::future({

        my_stream <- Rvision::stream(index)
        message("Streaming is ON")


        my_buffer <- Rvision::queue(
          x = my_stream, size = 30 * 60, overflow = "grow"
        )
        message("Buffer is active")


        frame <- Rvision::readNext(my_buffer)
        my_writer <- Rvision::videoWriter(
          outputFile = get_video_path(out_dir, pid),
          fourcc = "mpeg",
          fps = 30, height = nrow(frame), width = ncol(frame)
        )
        message("Writer is ready")


        fire_running(status_video)

        i <- 1

        while (TRUE) {
          if (Rvision::empty(my_buffer)) {
            usethis::ui_warn(
              "Empty buffer, cycle skipped waiting 1 s.\n"
            )
            Sys.sleep(1)
            next
          }

          Rvision::readNext(my_buffer, target = frame)
          if (!Rvision::isImage(frame)) {
            usethis::ui_warn("frame is not an image, cycle skipped\n")
            next
          }

          Rvision::writeFrame(my_writer, frame)
          suppressMessages(
            Rvision::write.Image(frame, get_frame_path(out_dir, i, pid))
          )

          if (is_status(status_video, "interrupt")) break

          fire_running(
            status_video,
            round(1 - 1/sqrt(i/50), 2 + log10(i)) * 100
          )
          i[[1]] <- i[[1]] + 1
        }
        Rvision::release(my_writer)
        Rvision::release(my_buffer)
        Rvision::release(my_stream)

        usethis::ui_done("Recording interrupted!\n")
        frame
      })

      res <- promises::catch(
        res,
        function(e) {
          message(e$message)
          fire_strange(status_video)
          usethis::ui_done("Releasing writer/buffer/stream")
          Rvision::release(my_writer)
          Rvision::release(my_buffer)
          Rvision::release(my_stream)

          showNotification(e$message, type = "warning")
        }
      )

      res <- promises::finally(
        res,
        function() {
          if (!is_status(status_video, "strange")) {
            fire_ready(status_video)
          }
        }
      )

      # Return something other than the promise so shiny remains
      # responsive
      NULL
    }) |>
      bindEvent(input$start)




    observe({
      message("Button stop clicked")

      if (is_status(status_video, "starting")) {
        return(test_failed_or_not_run(session = session))
      }
      if (would_stop_stopped(status_video)) return(NULL)
      if (would_stop_interrupted(status_video)) return(NULL)

      fire_interrupt(status_video)
      showNotification("Video recording cycle stopped")
      message("Streaming is OFF")
      message(
        "objects in globalenv (OK if empty!): ", ls(envir = .GlobalEnv)
      )
    }) |>
      bindEvent(input$stop)




# Outputs ---------------------------------------------------------

    output$camera_idx <- renderText({
      validate(need(input[['index']], "index must be provided"))

      glue::glue("Camera index is: {input[['index']]}")

    })

    output$snapshot <- renderPlot({ plot(test_out()) })

    output$out_file <- renderText({
      glue::glue(
        "Output video will be in: {out_folder()}
        Named: YYYYMMDDhhmmss-{input[['pid']]}.mp4"
      )
    })

    output$status <- renderText({
      paste0("Current status: ", current_status())
    })

  })
}

## To be copied in the UI
# mod_video_ui("video_1")

## To be copied in the server
# mod_video_server("video_1")

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
#' @importFrom shinyFiles shinyDirChoose shinyDirButton getVolumes
mod_video_ui <- function(id){
  useShinyjs()
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
        # shinyDirButton(
        #   ns('dirFolder'),
        #   'Select a folder',
        #   'Please select a folder',
        #   FALSE
        # ),
        actionButton(ns("preview"), "Take snapshot"),
        actionButton(ns("clear_preview"), "Clear snapshot"),
        actionButton(ns("start"), "Start recording"),
        actionButton(ns("stop"), "Stop recording"),


        actionButton(ns("pippo"), "Clicca qui")
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




    # Camera settings -------------------------------------------------

    output$camera_idx <- renderText({
      validate(need(input[['index']], "index must be provided"))

      glue::glue("Camera index is: {input[['index']]}")

    })




    # Directory selector ----------------------------------------------

    # volumes <- getVolumes()
    # observe({
    #   shinyDirChoose(input, 'dirFolder', roots = "C:")
    #   cat(glue::glue("Folder selected: {input[['dirFolder']]}"))
    # })


    output$out_file <- renderText({
      validate(
        # need(input[["folder"]], "folder must be provided"),
        need(input[["pid"]], "pid must be provided"),
      )

      glue::glue(
        # "Output video file will be in folder: {input[['folder']]}
        "Output video file will be in folder: {here::here()}
        Named: YYYYMMDDhhmmss-{input[['pid']]}.mp4"
      )

    })




    # Testing snapshot ------------------------------------------------


    test_out <- eventReactive(input[["preview"]], {

      validate(need(input[["index"]], "index must be provided"))

      op <- options(digits.secs = 6)
      withr::defer(options(op))

      my_stream <- Rvision::stream(input[["index"]])
      withr::defer(Rvision::release(my_stream))

      Rvision::readNext(my_stream)
    })


    output$snapshot <- renderPlot({ plot(test_out()) })

    observeEvent(
      input[["clear_preview"]],
      shinyjs::hide(ns("snapshot"))
    )



    # Recordings ------------------------------------------------------

    ciao <- eventReactive(input[["pippo"]], {
      cat("!!!!!!!!!!!!!!!!!!!!!!!\\n")
    }, ignoreNULL = TRUE)



    recording <- reactiveValues(
      on = FALSE,
      frame = 0L
    )


    output$recording <- renderText({
      glue::glue("
        Recording is: {c('OFF', 'ON')[recording$on + 1L]}
        Recording frame: {recording$frame}
      ")
    })


    video_objs <- eventReactive(input[["start"]], {
      # validate(
      #   need(input[["index"]], "index must be provided"),
      #   # need({ folder <- input[["folder"]] }, "folder must be provided"),
      #   need(input[["pid"]], "pid must be provided"),
      #   # need(recording, "recording not in progress")
      # )
      # index <- input[["index"]]
      # pid <- input[["pid"]]
      #
      # op <- options(digits.secs = 6)
      # withr::defer(options(op))

      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

      # my_stream <- Rvision::stream(input)
      # my_buffer <- Rvision::queue(
      #   x = my_stream, size = 30 * 60, overflow = "grow"
      # )
      # frame <- Rvision::readNext(my_buffer)
      # my_writer <- Rvision::videoWriter(
      #   # outputFile = get_video_path(folder, pid),
      #   outputFile = get_video_path(here::here(), pid),
      #   fourcc = "mpeg",
      #   fps = 30, height = nrow(frame), width = ncol(frame)
      # )
      #
      # list(
      #   stream = my_stream,
      #   buffer = my_buffer,
      #   writer = my_writer
      # )
    })


    observeEvent(recording$on, {
      validate(
        # need({ folder <- input[["folder"]] }, "folder must be provided"),
        need(input[["pid"]], "pid must be provided"),
        # need(video_objs, "recording non started yet")
      )
      pid <- input[["pid"]]

      frame <- Rvision::readNext(video_objs()[["buffer"]])

      while (recording$on) {

        if (Rvision::empty(video_objs()[["buffer"]])) {
          usethis::ui_warn(
            "Empty buffer, cycle skipped waiting 1 s."
          )
          Sys.sleep(1)
          next
        }

        Rvision::readNext(video_objs()[["buffer"]], target = frame)
        # frame_path <- get_frame_path(folder, recording$frame, pid)
        frame_path <- get_frame_path(folder, recording$frame, pid)
        if (!Rvision::isImage(frame)) {
          usethis::ui_warn("frame is not an image, cycle skipped")
          next
        }
        recording$frame <- recording$frame + 1L

        Rvision::writeFrame(video_objs()[["writer"]], frame)
        Rvision::write.Image(frame, frame_path)
      }
    })


    observeEvent(input[["stop"]], {
      # validate(need(video_objs, "recording not started yet"))

      if (recording$on) {
        recording$on <- FALSE
        Rvision::release(video_objs()[["writer"]])
        Rvision::release(video_objs()[["buffer"]])
        Rvision::release(video_objs()[["stream"]])

        output$snapshot <- renderPlot(plot(video_objs[["frame"]]))
      }
    })


  })
}

## To be copied in the UI
# mod_video_ui("video_1")

## To be copied in the server
# mod_video_server("video_1")

options(shiny.reactlog = TRUE)

#' video_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_video_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Buttons",
        actionButton(ns("preview"), "Take snapshot"),
        actionButton(ns("start"), "Start recording"),
        actionButton(ns("stop"), "Stop recording")
      ),
      box(
        title = "Set the settings",
        numericInput(
          ns("index"),
          "Camera index: ",
          min = 0,
          value = 0,
          step = 1
        )
      ),
      box(
        title = "Checkt the settings",
        textOutput(ns("camera_idx"))
      )
    ),
    fluidRow(
      box(
        title = "Preview",
        width = 12,
        plotOutput(ns("snapshot"))
      )
    )

  )
}

#' video_test Server Functions
#'
#' @noRd
mod_video_test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Preview ---------------------------------------------------------

    # This works as a reactive because output$snapshot depends on it
    # when user invalidates preview, invalidates test_out, that
    # invalidates output$snapshot. As output it will be computed, so
    # it needs test_out, which will be called and updated.
    test_out <- reactive({
      withr::local_options(list(digits.secs = 6))
      validate(need(input$index, "index must be provided"))

      my_stream <- Rvision::stream(input$index)
      withr::defer(Rvision::release(my_stream))

      Rvision::readNext(my_stream)

    }) |>
      bindEvent(input$preview)


# Recording -------------------------------------------------------

    observe({
      req(input$index)

      tmp_video_file <- tempfile("test-video", fileext = ".mp4")
      usethis::ui_info(
        "Video file at: {usethis::ui_value(tmp_video_file)}."
      )

      my_stream <- Rvision::stream(input$index)
      withr::defer(Rvision::release(my_stream))

      my_buffer <- Rvision::queue(
        x = my_stream, size = 30 * 60, overflow = "grow"
      )
      withr::defer(Rvision::release(my_buffer))

      frame <- Rvision::readNext(my_buffer)
      my_writer <- Rvision::videoWriter(
        outputFile = tmp_video_file,
        fourcc = "mpeg",
        fps = 30, height = nrow(frame), width = ncol(frame)
      )
      withr::defer(Rvision::release(my_writer))


      ## HERE I WOULD START THE (POSSIBLY INFINITE) RECORDING CYCLE
      usethis::ui_todo("Recording is ON")
      while (TRUE) { ## PUT HERE A REACTIVE?! READ A TEXTFILE ON DISK?!
        Rvision::readNext(my_buffer, target = frame)
        Rvision::writeFrame(my_writer, frame)

        if (TRUE) { ## ...OR PUT HERE A SUITABLE CONDITION?!
          break
        }
      }
    }) |>
      bindEvent(input$start)




    observe({
      ## HERE I WOULD STOP THE RECORDING CYCLE
      ## CHANGE A REACTIVE?! MODIFY A TEXTFILE ON DISK?!
      usethis::ui_done("Recording is OFF")
    }) |>
      bindEvent(input$stop)



# outputs ---------------------------------------------------------

    output$camera_idx <- renderText({
      validate(need(input$index, "index must be provided"))
      paste0("Camera index is: ", input$index)
    })

    output$snapshot <- renderPlot({ plot(test_out()) })

  })
}

## To be copied in the UI
# mod_video_test_ui("video_test_1")

## To be copied in the server
# mod_video_test_server("video_test_1")

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()



on_start <- function() {
  workers <- golem::get_golem_options("workers")
  workers <- ifelse(is.null(workers), 4L, workers)

  future::plan(
    future::multisession,
    workers = workers
  )

  status_video <- fs::file_temp("iobed_status_video", ext = "txt")
  usethis::ui_info(
    "Video status file created in: {usethis::ui_code(status_video)}"
  )
  assign("status_video", status_video,  envir = .GlobalEnv)
  fire_starting(status_video)

  status_bed <- fs::file_temp("iobed_status_bed", ext = "txt")
  usethis::ui_info(
    "Bed status file created in: {usethis::ui_code(status_bed)}"
  )
  assign("status_bed", status_bed,  envir = .GlobalEnv)
  fire_starting(status_bed)

  onStop(function() {
    usethis::ui_info(
      "Final video status: {usethis::ui_value(get_status(status_video))}"
    )
    usethis::ui_info(
      "Final bed status: {usethis::ui_value(get_status(status_bed))}"
    )
    future::plan(future::sequential)
  })
}



# Run the application
run_app(onStart = on_start, golem_opts = list(workers = 4))







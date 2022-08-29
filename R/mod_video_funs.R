# functions -------------------------------------------------------

#' Title
#'
#' @return Value
#' @export
now <- function() {
  stringr::str_remove_all(Sys.time(), "\\W")
}

#' Title
#'
#' @return Value
#' @export
get_video_path <- function(folder, pid) {
  file.path(folder, glue::glue("{now()}-{pid}.mp4")) |>
    normalizePath(mustWork = FALSE)
}

#' Title
#'
#' @return Value
#' @export
get_frame_path <- function(folder, frame_id, pid) {
  file.path(folder, glue::glue("{now()}-{pid}_{frame_id}.png")) |>
    normalizePath(mustWork = FALSE)
}

#' Title
#'
#' @return Value
#' @export
get_status <- function(status_file) {
  readLines(status_file)
}

#' Title
#'
#' @return Value
#' @export
set_status <- function(status_file, msg) {
  writeLines(msg, status_file)
}

#' Title
#'
#' @return Value
#' @export
fire_interrupt <- function(status_file) {
  set_status(status_file, "Interrupt")
  message("Setting status: Interrupt")
}

#' Title
#'
#' @return Value
#' @export
fire_ready <- function(status_file) {
  set_status(status_file, "Ready")
}

#' Title
#'
#' @return Value
#' @export
fire_strange <- function(status_file) {
  set_status(status_file, "Strange things happened...")
}


#' Title
#'
#' @return Value
#' @export
fire_running <- function(status_file, perc = NULL) {
  if (is.null(perc)) return(set_status(status_file, "Running..."))
  set_status(status_file, paste0("Running... (", perc, " % completed)"))
}

#' Title
#'
#' @return Value
#' @export
is_status <- function(status_file, status) {
  stringr::str_detect(
    stringr::str_to_lower(get_status(status_file)),
    stringr::str_to_lower(status)
  )
}

#' Title
#'
#' @return Value
#' @export
print_progress <- function(i) {
  if (i %% 50 == 0) {
    cat(i, "\n")
  } else if (i %% 10 == 0) {
    cat("X")
  } else cat(".")
}


#' Title
#'
#' @param envir environment
#'
#' @return Value
#' @export
release_rvision <- function(envir = .GlobalEnv) {
  if (
    exists("my_writer", envir = envir) ||
    exists("my_buffer", envir = envir) ||
    exists("my_stream", envir = envir)
  ) {
    suppressWarnings({
      Rvision::release(my_writer)
      Rvision::release(my_buffer)
      Rvision::release(my_stream)
    })
    ## rm(..., envir = parent.frame(1)) is hard-coded in
    ## Rvision:::release.Rcpp_Stream(); so it warns and doesn't remove
    ## my_stream, but it success in releasing it.
    rm(my_stream, my_buffer, my_writer, envir = envir)
  }

}

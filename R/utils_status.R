#' Title
#'
#' @param status_file (chr) status file
#'
#' @return Value
#' @export
get_status <- function(status_file) {
  readLines(status_file)
}

#' Title
#'
#' @param status_file (chr) status file
#' @param status_msg (chr) message
#'
#' @importFrom usethis ui_field ui_value
#'
#' @return Value
#' @export
set_status <- function(status_file, status_msg) {
  writeLines(status_msg, status_file)
}


fire_status <- function(status_file, status_msg) {
  status_name <- basename(status_file)
  set_status(status_file, status_msg)
  usethis::ui_done(
    "Setting {ui_field(status_name)} status: {ui_value(status_msg)}"
  )
}


#' Title
#'
#' @param status_file (chr) status file
#'
#' @return Value
#' @export
fire_starting <- function(status_file) {
  fire_status(status_file, "Starting")
}

#' Title
#'
#' @param status_file (chr) status file
#'
#' @return Value
#' @export
fire_interrupt <- function(status_file) {
  fire_status(status_file, "Interrupt")
}

#' Title
#'
#' @param status_file (chr) status file
#'
#' @return Value
#' @export
fire_ready <- function(status_file) {
  fire_status(status_file, "Ready")
}

#' Title
#'
#' @param status_file (chr) status file
#'
#' @return Value
#' @export
fire_strange <- function(status_file) {
  fire_status(status_file, "Strange things happened...")
}


#' Title
#'
#' @param status_file (chr) status file
#' @param perc (num) complete rate
#'
#' @return Value
#' @export
fire_running <- function(status_file, perc = NULL) {
  if (is.null(perc)) return(set_status(status_file, "Running..."))
  status_file |>
    fire_status(paste0("Running... (", perc, " % completed)"))
}

#' Title
#'
#' @param status_file (chr) status file
#' @param status (chr) status to detect
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
#' @param i (int) index
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


would_start_when_running <- function(status_file) {
  if (is_status(status_file, "running")) {
    showNotification(
      "Already recording.
         You cannot start new recordings if one is ongoing.
         Please, interrupt the current run (stop button) if you need a new recording.
        ",
      type = "warning",
      duration = 10
    )
    message("Recording doesn't started (again)")
    return(TRUE)
  }
  FALSE
}

would_start_not_ready <- function(status_file) {
  if (!is_status(status_file, "ready")) {
    showNotification(
      "Not ready.
         Have you done all settings (and accepted them)?
         You need to set both the PID, and index/port to start recording.
        ",
      type = "warning",
      duration = 10
    )
    message("Cycle doesn't started (not ready)")
    return(TRUE)
  }
  FALSE
}


would_stop_stopped <- function(status_file) {
  if (is_status(status_file, "ready")) {
    showNotification(
      "Recording is not running.
         You cannot interrupt a not running recording...
         If you like, you can start a recording to interrupt ;-)
         (start button).
        ",
      type = "warning",
      duration = 10
    )
    message("Recording ready to start, it doesn't interrupted")
    return(TRUE)
  }
  FALSE
}

would_stop_interrupted <- function(status_file) {
  if (is_status(status_file, "interrupt")) {
    showNotification(
      "Recording already interrupted.
         You cannot interrupt a not running recording...
         If ready, you can start a recording to interrupt ;-)
         (start button).
        ",
      type = "warning",
      duration = 10
    )
    message("Recording doesn't interrupted (again)")
    return(TRUE)
  }
  FALSE
}

#' Title
#'
#' @return Value
#' @export
now <- function() {
  stringr::str_remove_all(Sys.time(), "\\W")
}


close_if_open_connection <- function(con = "con") {
  if (exists(con) && serial::isOpen(get(con))) {
    showNotification(
      "Something strange happen wiht the connection.
          Anywhay, that should not invalidate the results.
          Please, contact Corrado.Lanera@ubep.unipd.it reporting the issue.
          Thanks.
        ",
      type = "warning",
      duration = 10
    )
    usethis::ui_warn(
      "A connection is already open, it will be closed now!"
    )
    return(close(con))
  }
  TRUE
}

check_connection <- function(con) {
  if (isFALSE(con)) {
    showNotification(
      "Connection not established.
          Please contact Corrado.Lanera@ubep.unipd.it reporting the issue.
          Thanks.
        ",
      type = "error",
      duration = 10
    )
  }
  !isFALSE(con)
}



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

check_connection <- function(con,  session =  getDefaultReactiveDomain()) {
  if (isFALSE(con)) {
    showNotification(
      "Connection not established.
          Please contact Corrado.Lanera@ubep.unipd.it reporting the issue.
          Thanks.
        ",
      type = "error",
      duration = 10,
      session = session
    )
  }
  !isFALSE(con)
}


test_failed_or_not_run <- function(e = NULL, session =  getDefaultReactiveDomain()) {
  showNotification(
    HTML(
      "Test failed or not run.</br></br>
      Maybe the cable is unplugged from the PC? (Plug it!)</br>
      Maybe wrong connection port/ID? (Select a different one!)</br></br
      >
      Before to start recording, PLEASE, RUN A SUCESSFUL TEST.</br></br>

      If you checked the cable connection, tried all ports/IDs,
      and still receive this error in testing connection, please,
      contact Corrado.Lanera@ubep.unipd.it.</br>
      Thank you.
    "),
    type = "error",
    duration = 30, session = session
  )
  usethis::ui_warn("Bed test failed or not run")
  if (!is.null(e)) usethis::ui_warn("ERROR: {e}")
  NULL
}

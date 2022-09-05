# functions -------------------------------------------------------

#' Title
#'
#' @param folder (chr) folder
#' @param pid (chr) Personal ID
#'
#' @return Value
#' @export
get_video_path <- function(folder, pid) {
  file.path(folder, glue::glue("{now()}-{pid}.mp4")) |>
    normalizePath(mustWork = FALSE)
}

#' Title
#'
#' @param frame_id (chr) camera frame (0, 1, ...)
#' @param folder (chr) folder
#' @param pid (chr) Personal ID
#'
#' @return Value
#' @export
get_frame_path <- function(folder, frame_id, pid) {
  file.path(folder, glue::glue("{now()}-{pid}_{frame_id}.png")) |>
    normalizePath(mustWork = FALSE)
}
#'
#' #' Title
#' #'
#' #' @param envir environment
#' #'
#' #' @return Value
#' #' @export
#' release_rvision <- function(
#'     envir = parent.frame(1)
#' ) {
#'     suppressWarnings({
#'       Rvision::release(my_writer)
#'       Rvision::release(my_buffer)
#'       Rvision::release(my_stream)
#'     })
#'     ## rm(..., envir = parent.frame(1)) is hard-coded in
#'     ## Rvision:::release.Rcpp_Stream(); so it warns and doesn't remove
#'     ## my_stream, but it success in releasing it.
#'     rm(my_stream, my_buffer, my_writer, envir = envir)
#'
#' }

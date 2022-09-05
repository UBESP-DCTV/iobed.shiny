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

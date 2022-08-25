now <- function() {
  stringr::str_remove_all(Sys.time(), "\\W")
}

get_video_path <- function(folder, pid) {
  file.path(folder, glue::glue("{now()}-{pid}.mp4")) |>
    normalizePath()
}

get_frame_path <- function(folder, frame_id, pid) {
  file.path(folder, glue::glue("{now()}-{pid}_{frame_id}.png")) |>
    normalizePath()
}

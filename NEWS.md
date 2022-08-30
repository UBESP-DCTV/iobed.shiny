# iobed.shiny 0.2.0

* Video recording save mp4 video and all the frames as png images, in the `data/<pid>/` folder, prepending the timestamp (YYYYMMDDhhmmss).
* Used future async for video recording start and stop
* Used status file for detect ready, interrupted, running (w/ percentage), or anomalous status for the computation
* The user cannot start a stared or not ready recording, nor stop a stopped or not running one.
* PID and camera index are mandatory to activate buttons

# iobed.shiny 0.1.0

# iobed.shiny 0.0.0.9000

* Completed `{golem}`'s `dev/01-start.R` and setup from `dev/02-dev.R`.
* Added a `NEWS.md` file to track changes to the package.

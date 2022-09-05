# iobed.shiny (development version)

* Setup onStart to setup and cleanup globally parallel environment and workers, and status fiels (now by default the app uses 4 parallel processes to manage the modules and the main application). (#3)
* Extracted utility functions (status and time) on dedicated files, i.e. `utils.R`, and `utils_status.R`

# iobed.shiny 0.2.3

* Now the res reactive look for the connection in the .Globalenv directly, preventing a bug that caused an error in the very first access to the connection from the tcltk ("[tcl] error reading "file1fac46bee60": I/O error.") probably caused by the different environment in which it first look for it.

# iobed.shiny 0.2.2

* Removed check for _ready_ness.

# iobed.shiny 0.2.1

* activate `{renv}`

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

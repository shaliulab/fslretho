#' Configuration of FSLRetho
#'
#' Load and update the configuration of the program
#'
#' Anywhere in FSLRetho `FSLRethoConfiguration` can be instantiated so
#' default values for hardware defined parameters can be set dynamically
#' via a config file
#'
#' @importFrom R6 R6Class
#' @importFrom rjson fromJSON
#' @importFrom rjson toJSON
#' @export
#' @noRd
FSLRethoConfiguration <- R6::R6Class(classname = "FSLREthoConfiguration", public = list(

  content = list(),
  config_file = "",

  initialize = function(config_file = "/etc/fslretho.conf") {

    content <- list("debug" = TRUE, "ncores" = 2, "stop_backups" = TRUE, port = 3838)
    content$folders <- list(
      "dam" = list(
        "path" = "/DAM_data/results",
        "description" = "A path to a folder containing MonitorXX.txt files"
        ),
      "ethoscope" = list(
        "path" = "/ethoscope_data/results",
        "description" = "A path to a folder containing an ethoscope database of sqlite3 files"
      ),
      "ethoscope_cache" = list(
        "path" = "/ethoscope_data/cache",
        "description" = "A path to a folder containing rds files for fast reloading of data loaded in a previous run"
      )
    )
    sessions_folder <- file.path(content$folders$ethoscope_cache$path, "sessions")

    config_file <- get_writable_path(config_file)
    self$config_file <- config_file
    self$content <- content

    if (!dir.exists(content$folders$ethoscope_cache$path)) dir.create(content$folders$ethoscope_cache$path)
    if (!dir.exists(sessions_folder)) dir.create(sessions_folder)



    self$load()

  },

  save = function(config_file = NULL) {
    json <- rjson::toJSON(self$content)

    if (is.null(config_file))
      config_file <- self$config_file

    message(paste0("Saving ", config_file))
    write(x = json, file = config_file)
  },

  load = function() {
    if (!file.exists(self$config_file)) self$save()
    else {

      message(paste0("Loading ", self$config_file))
      json <- rjson::fromJSON(file = self$config_file)
      self$content <- modifyList(self$content, json)
    }

    return(self$content)
  }
))

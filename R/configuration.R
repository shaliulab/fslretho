#' Configuration of FSLRetho
#'
#' Load and update the configuration of the program
#'
#' Anywhere in FSLRetho `FSLRethoConfiguration` can be instantiated so
#' default values for hardware defined parameters can be set dynamically
#' via a config file
#'
#' @name FSLREthoConfiguration
#' @importFrom R6 R6Class
#' @importFrom rjson fromJSON toJSON
#' @method initialize something
#' @field content List of configuration items
#' @field config_file Default configuration file path
#'
FSLRethoConfiguration <- R6::R6Class(classname = "FSLREthoConfiguration", public = list(

  content = list(),
  config_file = "",

  #' Initialize a configuration instance
  #' Called by running `FSLRethoConfiguration$new()`
  #' @param config_file Path to the default configuration file
  initialize = function(config_file = file.path(c(file.path(Sys.getenv("HOME"), ".config"), "/etc"), "fslretho.conf")) {

    content <- scopr::scoprConfiguration$new()$content
    content <- append(content, list("debug" = FALSE, "stop_backups" = TRUE, port = 3838))

    content$folders <- append(content$folders, list(
      "dam" = list(
        "path" = "/DAM_data/results",
        "description" = "A path to a folder containing MonitorXX.txt files"
        ),
      "ethoscope_sessions" = list(
        "path" = "/ethoscope_data/sessions",
        "description" = "A path to a folder containign rds files for fast reloading of data loaded in a previous run.
        The files are created when the user presses the save button in fslretho. The whole dataset loaded there is saved to a single rds file."
      )
    ))



    index <- which(sapply(config_file, is.writable) & file.exists(config_file))
    if(length(index) == 0) index <- 1
    self$config_file <- get_writable_path(config_file[index])
    stopifnot(is.writable(dirname(self$config_file)))
    self$content <- content
    self$load()
    self$verify()
  },

  #' Make sure all folders that fslretho needs are writable
  #' if they are not, recreate them as needed in the HOME folder
  #' @param config_file Configuration file path
  verify = function(config_file = NULL) {

    content <- self$content
    old_content <- content
    folders <- content$folders

      # check if folder is writable
      for (i in 1:length(folders)) {
        f <- folders[[i]]
        if (!is.writable(path = dirname(f$path))) {
          writable_f <- file.path(Sys.getenv("HOME"), ".fslretho", f$path)
          content$folders[[i]]$path <- writable_f
        } else {
          writable_f <- f$path
        }

        # create folder if does not exist
        if (!dir.exists(writable_f)) dir.create(writable_f, recursive = TRUE)
      }
    self$content <- content
    self$save()

    if (!identical(self$content, old_content))
        self$save(self$config_file)
  },

  #' Save the configuration in to a config_file
  #'
  #' Save the configuration stored in self$content
  #' If the passed config_file is null, use the instance's default
  #' @param config_file Configuration file path
  save = function(config_file = NULL) {
    json <- rjson::toJSON(self$content)

    if (is.null(config_file))
      config_file <- self$config_file

    if(self$content$debug) message(paste0("Saving ", config_file))
    write(x = json, file = config_file)
  },

  #' Load a a configuration from a config_file
  #' If the passed config_file is null, use the instance's default
  #' If the passed file does not exist create it with the conf in the default file
  #' If it exists, load its contents and update the configuration
  #' @param config_file Configuration file path
  load = function(config_file=NULL) {

    if (is.null(config_file))
      config_file <- self$config_file

    if (!file.exists(config_file) & !is.null(config_file))
      self$save(config_file)
    else {
      if(self$content$debug) message(paste0("Loading ", config_file))
      json <- rjson::fromJSON(file = config_file)
      self$content <- modifyList(self$content, json)
    }

    return(self$content)
  }
))

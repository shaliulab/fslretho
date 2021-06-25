#' Configuration of FSLRetho
#'
#' Load and update the configuration of the program
#'
#' Anywhere in FSLRetho `FSLRethoConfiguration` can be instantiated so
#' default values for hardware defined parameters can be set dynamically
#' via a config file
#'
#' @importFrom R6 R6Class
#' @importFrom rjson fromJSON toJSON
#' @export
#' @method initialize something
#' @field content List of configuration items
#' @field config_file Default configuration file path
FSLRethoConfiguration <- R6::R6Class(classname = "FSLREthoConfiguration", public = list(

  content = list(),
  config_file = "",

  #' Initialize a configuration instance
  #' Called by running `FSLRethoConfiguration$new()`
  #' @param config_file Path to the default configuration file
  initialize = function(config_file = "/etc/fslretho.conf") {

    content <- list()

    # link scopr conf
    content$scopr <- scopr::scoprConfiguration$new()$content
    # TODO link damr conf
    # content$damr <- damr::damrConfiguration$new(testing=testing)$content
    content$damr <- list(folders = list(results = list(path = "/DAM_data/results")))
    content <- append(content, list("debug" = FALSE, port = 3838, testing=FALSE))
    content$binaries <- list('python' = system("which python", intern = TRUE))
    content$dependencies <- list("ethoscope_imager" = "/opt/ethoscope-imager/imager.py")

    content$folders <- append(content$folders, list(
      "ethoscope_sessions" = list(
        "path" = "/ethoscope_data/sessions",
        "description" = "A path to a folder containing rds files for fast reloading of data loaded in a previous run.
        The files are created when the user presses the save button in fslretho. The whole dataset loaded there is saved to a single rds file."
      ),
      "logs" = list(
        "path" = "/fslretho_data/logs",
        "description" = "A path to a folder containing log files tracking the application's usage"
      )
    ))

    self$config_file <- config_file
    self$content <- content
    self$load(self$config_file)
  },

  #' Save the configuration in to a config_file
  #'
  #' Save the configuration stored in self$content
  #' If the passed config_file is null, use the instance's default
  #' @param config_file Configuration file path
  save = function(config_file) {

    content <- self$content
    # unlink modules
    content$damr <- NULL
    content$scopr <- NULL

    json <- rjson::toJSON(content)

    if(self$content$debug) message(paste0("Saving ", config_file))
    write(x = json, file = config_file)
  },

  #' Load a a configuration from a config_file
  #' If the passed config_file is null, use the instance's default
  #' If the passed file does not exist create it with the conf in the default file
  #' If it exists, load its contents and update the configuration
  #' @param config_file Configuration file path
  load = function(config_file) {

    # if the config file is not does not exist or if it is empty
    if (!file.exists(config_file) | file.size(config_file) == 0)
      self$save(config_file)
    else {
      if(self$content$debug) message(paste0("Loading ", config_file))
      json <- rjson::fromJSON(file = config_file)
      self$content <- modifyList(self$content, json)
    }

    return(self$content)
  }
))

##
# fslretho employs a reactiveValues data structure to transfer data between the appliation's modules,
# which makes it very important to understand how they work and how they are structured

# Structure:

# reactiveValues(data = behavr(), name = character(), time = Sys.time())

# Updates to time are listened to by observeEvents throughout the application,
# which respond and reprocess the contents of data

# FSLretho has a load/save functionality, which makes it necessary to implement functions
# that encapsulate the save / load behavior

save_reactiveValuesRDS <- function(object = rv, ...) {
  d <- reactiveValuesToList(object)
  saveRDS(object = d, ...)
}

read_reactiveValuesRDS <- function(file, ...) {

  rv <- reactiveValues(data = NULL, name = NULL, time = NULL)
  d <- readRDS(file, ...)

  rv$data <- d$data
  rv$name <- d$name
  rv$time <- d$time

  return(rv)
}
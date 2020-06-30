library(shinytest)
library(shiny)

path_to_proj <- strsplit(getwd(), split = "fslretho")[[1]][1]
path_to_app <- file.path(path_to_proj, "fslretho", "inst/app")
options(shiny.testmode=TRUE)
# Run me from any directory but pass the path to the directory where app lives
# i.e. where the server.R and ui.R are in the appDir argument.
# testApp then sets the working directory to tests/ dir inside it
# and runs the test from tests/.
shinytest::testApp(appDir = path_to_app)


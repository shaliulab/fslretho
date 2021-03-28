library(testthat)
library(shiny)
context("esquisse")

test_that("communication with esquisse works", {


  rejoined_data <- reactiveValues(data=NULL, name=NULL, time=NULL)

  esquisserModule <- function(id, data = NULL,
                            dataModule = c("GlobalEnv", "ImportFile"),
                            sizeDataModule = "m", launchOnStart=TRUE) {
    moduleServer(id, module = esquisse::esquisserServer)
  }


  testServer(esquisserModule, args = list(data = rejoined_data), {
                  rejoined_data$data <- fslbehavr::rejoin(
                    fslsleepr::sleep_annotation(fslbehavr::toy_ethoscope_data(), velocity_correction_coef=0.048)
                  )
                  rejoined_data$name <- "toy"
                  rejoined_data$time <- 666
                  browser()
                  session$getReturned()
                  session$flushReact()
  })

})

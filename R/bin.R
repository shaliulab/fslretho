# source("R/bin_params.R")

  bin_data_UI <- function() {


    # choices <- purrr::map(choices, ~rlang::enexpr)
    shiny::tagList(
      shiny::sliderInput("summary_time_window", label = "Summary time window",
                  value = 30, min = 5, max = 120, step = 5),
      shiny::selectizeInput("summary_FUN", label = "Summary function", choices = choices)
    )
}

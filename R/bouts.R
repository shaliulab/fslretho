analyseBoutServer <- function(id, grouped_data) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, name = NULL)

      observeEvent(grouped_data$data, {
        req(grouped_data$data)
        bouts <- fslsleepr::bout_analysis(asleep, grouped_data$data)
        # browser()
        rv$data <- fslbehavr::bin_apply_all(bouts,
                                            duration,
                                            x = "t",
                                            x_bin_length = fslbehavr::mins(30),
                                            # TODO Support wrapping
                                            # wrap_x_by = time_wrap,
                                            FUN = mean)
        rv$name <- grouped_data$name
      })
      return(rv)
    }
  )
}

#' Datasets offered in the "Download datasets" menu: id -> label
#' @noRd
DOWNLOADS <- c(
  `sleep-binned` = "Sleep (binned, as plotted)",
  `sleep-scored` = "Sleep (scored, one row per window)",
  `raw-data` = "Raw data (Raw tab)",
  `bouts` = "Sleep bouts (as plotted)",
  `sleep-summary` = "Sleep summary",
  `bouts-summary` = "Sleep bouts summary",
  `periodogram` = "Periodogram",
  `spectrogram` = "Spectrogram"
)

downloadsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "fsl-downloads",
    downloadButton(ns("dbfiles"), "Ethoscope .db files (.zip)", class = "btn-block"),
    lapply(names(DOWNLOADS), function(name) downloadButton(ns(name), DOWNLOADS[[name]], class = "btn-block"))
  )
}

#' Write a dataset to csv. Metadata columns are added to every row so the file is self-contained
#' @noRd
write_dataset <- function(data, file) {
  if ("behavr" %in% class(data)) data <- behavr::rejoin(data)
  data <- data.table::as.data.table(data)
  list_columns <- names(data)[sapply(data, is.list)]
  if (length(list_columns) > 0) data[, (list_columns) := NULL]
  data.table::fwrite(data, file)
}

#' @param datasets named list of datasets (reactiveValues with data and name),
#' with the names of DOWNLOADS
#' @param loaded the loaded dataset, whose .db files can be downloaded
#' @noRd
downloadsServer <- function(id, datasets, loaded) {
  moduleServer(
    id,
    function(input, output, session) {

      # Read the dataset when the user clicks, not when the app starts
      nothing_to_download <- function(label) {
        showNotification(
          sprintf("'%s' is not available yet. Load data and open the corresponding tab first.", label),
          type = "warning"
        )
        req(FALSE)
      }

      lapply(names(DOWNLOADS), function(name) {
        rv <- datasets[[name]]
        output[[name]] <- downloadHandler(
          filename = function() paste0(dataset_basename(rv$name), "-", name, ".csv"),
          content = function(file) {
            if (is.null(rv$data) || nrow(rv$data) == 0) nothing_to_download(DOWNLOADS[[name]])
            write_dataset(rv$data, file)
          }
        )
      })

      output$dbfiles <- downloadHandler(
        filename = function() paste0(dataset_basename(loaded$name), "-dbfiles.zip"),
        content = function(file) {
          if (is.null(loaded$data)) nothing_to_download("Ethoscope .db files")
          dbfiles <- unique(as.character(unlist(behavr::meta(loaded$data)$file_info)))
          id <- showNotification(sprintf("Compressing %d .db file(s), please wait...", length(dbfiles)), duration = NULL)
          on.exit(removeNotification(id))
          # -j: store the files without their folders
          utils::zip(file, dbfiles, flags = "-j")
        },
        contentType = "application/zip"
      )
    }
  )
}

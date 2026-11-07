#' Snapshot viewer: the pictures (IMG_SNAPSHOTS) the ethoscope takes during the experiment
#'
#' The snapshots are extracted, annotated with their date and time, and turned into
#' videos by the ethoscope-imager python script. It writes its output next to the .db file,
#' so the .db file is linked into a writable folder (folders$snapshots in the configuration)
#' and the result folder can stay read-only.

# Recordings with more snapshots than this are browsed in blocks
SNAPSHOT_MAX_IDS <- 10
# Length of a block of snapshots, in seconds
SNAPSHOT_BLOCK_SIZE <- 600

#' Folder where the snapshots of a .db file are extracted
#' @noRd
snapshot_workspace <- function(dbfile) {
  root <- fslretho_conf()$folders$snapshots$path
  # e.g. <root>/ETHOSCOPE_008/2026-11-05_19-25-25/
  folder <- file.path(root, basename(dirname(dirname(dbfile))), basename(dirname(dbfile)))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  link <- file.path(folder, basename(dbfile))
  if (!file.exists(link)) file.symlink(dbfile, link)
  link
}

#' Run ethoscope-imager on a .db file
#' @param ids snapshot ids (IMG_SNAPSHOTS.id) to extract
#' @param video if TRUE, make a video of the (already extracted) snapshots with these ids
#' @return paths of the files produced (.jpg or .mp4)
#' @noRd
ethoscope_imager <- function(dbfile, ids, video = FALSE, fps = 10) {
  conf <- fslretho_conf()
  args <- c(conf$dependencies$ethoscope_imager, "--path", snapshot_workspace(dbfile), "--id", ids)
  if (video) args <- c(args, "--video", "--fps", fps)
  out <- suppressWarnings(system2(conf$binaries$python, shQuote(args), stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) stop("ethoscope-imager failed (exit status ", status, ")")
  files <- out[grepl("\\.(jpg|mp4)$", out) & file.exists(out)]
  if (length(files) == 0) stop("ethoscope-imager did not produce any file")
  sort(files)
}

#' The snapshots stored in a .db file (without the images)
#' @return data.table with id, t (ms since the recording started) and datetime
#' @noRd
list_snapshots <- function(dbfile) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile, flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(con))
  shots <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT id, t FROM IMG_SNAPSHOTS ORDER BY t"))
  start <- as.numeric(scopr::get_metadata(dbfile)$date_time)
  shots[, datetime := as.POSIXct(start + t / 1000, origin = "1970-01-01", tz = "GMT")]
  shots
}

#' Group snapshots in blocks of SNAPSHOT_BLOCK_SIZE seconds (one block per snapshot if there are few)
#' @return the snapshots with a block column: the id of the first snapshot of the block
#' @noRd
snapshot_blocks <- function(shots) {
  shots <- data.table::copy(shots)
  if (nrow(shots) <= SNAPSHOT_MAX_IDS) {
    shots[, block := id]
  } else {
    shots[, block := id[1], by = .(floor(t / 1000 / SNAPSHOT_BLOCK_SIZE))]
  }
  shots
}

#' Choices of the snapshot selector: block start id -> label with the date and time
#' @noRd
snapshot_choices <- function(blocks) {
  b <- blocks[, .(from = min(datetime), n = .N), by = block]
  choices <- as.character(b$block)
  names(choices) <- sprintf(
    "%s%s", format(b$from, "%Y-%m-%d %H:%M"),
    ifelse(b$n > 1, sprintf(" (%d shots)", b$n), "")
  )
  choices
}

#' Blocks with snapshots during the SD (plus one block before and after it)
#' @noRd
sd_blocks <- function(dbfile, blocks) {
  metadata <- scopr::get_metadata(dbfile)
  date_range <- scopr:::parse_date_range(metadata$selected_options$interactor$kwargs$date_range)
  if (all(date_range == -1)) stop("this recording has no SD schedule")
  t_range <- (date_range[1:2] - as.numeric(metadata$date_time)) * 1000
  margin <- SNAPSHOT_BLOCK_SIZE * 1000
  unique(blocks[t > t_range[1] - margin & t < t_range[2] + margin, block])
}

snapshotViewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        shinydashboard::box(
          width = NULL, title = "Recording", status = "primary", solidHeader = TRUE,
          uiOutput(ns("dbfile_ui")),
          tableOutput(ns("summary"))
        ),
        shinydashboard::box(
          width = NULL, title = "Snapshots",
          selectizeInput(ns("blocks"), label = "Snapshots to extract", choices = NULL, multiple = TRUE, width = "100%"),
          div(
            class = "fsl-inline-buttons",
            actionButton(ns("all"), "All snapshots"),
            actionButton(ns("sd"), "SD only"),
            actionButton(ns("clear"), "Clear")
          ),
          actionButton(ns("extract"), "Extract and show", icon = icon("images"), class = "btn-primary"),
          tags$hr(),
          numericInput(ns("fps"), label = "Video frames per second", value = 10, min = 1, max = 30),
          downloadButton(ns("video"), "Make .mp4 video of the selected snapshots")
        )
      ),
      column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          uiOutput(ns("index_ui")),
          imageOutput(ns("snapshot"), height = "auto")
        )
      )
    )
  )
}

#' @param input_rv the loaded dataset: its .db files are offered
#' @noRd
snapshotViewerServer <- function(id, input_rv) {
  moduleServer(
    id,
    function(input, output, session) {

      extracted <- reactiveVal(NULL)

      output$dbfile_ui <- renderUI({
        meta <- behavr::meta(req(input_rv$data))
        meta <- unique(meta[, .(machine_name, datetime, path = as.character(unlist(file_info)))], by = "path")
        choices <- meta$path
        names(choices) <- paste(meta$machine_name, meta$datetime)
        selectizeInput(session$ns("dbfile"), "Recording (.db file)", choices = choices, width = "100%")
      })

      blocks <- reactive({
        dbfile <- req(input$dbfile)
        shots <- safely(context = "reading the snapshots", list_snapshots(dbfile))
        validate(need(!is.null(shots) && nrow(shots) > 0, "This recording has no snapshots"))
        snapshot_blocks(shots)
      })

      observeEvent(blocks(), {
        extracted(NULL)
        updateSelectizeInput(session, "blocks", choices = snapshot_choices(blocks()), selected = character(0))
      })

      output$summary <- renderTable({
        b <- blocks()
        data.frame(
          Snapshots = nrow(b),
          From = format(min(b$datetime), "%Y-%m-%d %H:%M"),
          To = format(max(b$datetime), "%Y-%m-%d %H:%M")
        )
      })

      observeEvent(input$all, updateSelectizeInput(session, "blocks", selected = unique(as.character(blocks()$block))))
      observeEvent(input$clear, updateSelectizeInput(session, "blocks", selected = character(0)))
      observeEvent(input$sd, {
        metadata <- scopr::get_metadata(req(input$dbfile))
        if (!nzchar(trimws(paste(metadata$selected_options$interactor$kwargs$date_range, collapse = "")))) {
          showNotification("This recording has no SD schedule", type = "warning")
          return()
        }
        safely(context = "selecting the SD snapshots", {
          selected <- sd_blocks(input$dbfile, blocks())
          if (length(selected) == 0) showNotification("No snapshots were taken during the SD", type = "warning")
          updateSelectizeInput(session, "blocks", selected = as.character(selected))
        })
      })

      selected_ids <- function() {
        ids <- blocks()[as.character(block) %in% input$blocks, id]
        if (length(ids) == 0) {
          showNotification("Select some snapshots first", type = "warning")
          req(FALSE)
        }
        ids
      }

      observeEvent(input$extract, {
        ids <- selected_ids()
        notification <- showNotification(sprintf("Extracting %d snapshot(s)...", length(ids)), duration = NULL)
        on.exit(removeNotification(notification))
        extracted(safely(context = "extracting the snapshots", ethoscope_imager(input$dbfile, ids)))
      })

      output$index_ui <- renderUI({
        files <- req(extracted())
        sliderInput(session$ns("index"), "Snapshot", min = 1, max = length(files), value = 1, step = 1,
                    width = "100%", animate = animationOptions(interval = 1000))
      })

      output$snapshot <- renderImage({
        files <- req(extracted())
        list(src = files[min(req(input$index), length(files))], contentType = "image/jpeg", width = "100%")
      }, deleteFile = FALSE)

      output$video <- downloadHandler(
        filename = function() paste0(tools::file_path_sans_ext(basename(input$dbfile)), ".mp4"),
        content = function(file) {
          ids <- selected_ids()
          notification <- showNotification(sprintf("Making a video of %d snapshot(s)...", length(ids)), duration = NULL)
          on.exit(removeNotification(notification))
          # the video is made from extracted snapshots: extract them first (already extracted ones are reused)
          ethoscope_imager(input$dbfile, ids)
          file.copy(ethoscope_imager(input$dbfile, ids, video = TRUE, fps = input$fps), file)
        },
        contentType = "video/mp4"
      )
    }
  )
}

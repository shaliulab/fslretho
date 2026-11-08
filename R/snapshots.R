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

# ROIs whose animals the stimulators manipulate (_roi_to_channel in the ethoscope code,
# github.com/shaliulab/ethoscope). Every sleep depriver / optogenetic / state stimulator of
# the lab derives from SleepDepStimulator or RobustSleepDepriver and uses these ROIs;
# DefaultStimulator does nothing.
STIMULATED_ROIS <- c(1, 3, 5, 7, 9, 12, 14, 16, 18, 20)
NO_STIMULATION <- c("DefaultStimulator")

#' Stimulator of a recording and the ROIs it manipulates
#' @return list with class (e.g. RobustSleepDepriver), rois (integer) and date_range (numeric
#' start / end pairs in seconds since epoch, GMT; empty if there is no SD schedule)
#' @noRd
recording_stimulation <- function(dbfile) {
  metadata <- scopr::get_metadata(dbfile)
  interactor <- metadata$selected_options$interactor
  class <- if (is.null(interactor$class)) "DefaultStimulator" else sub(".*\\.", "", as.character(interactor$class))
  date_range <- scopr:::parse_date_range(interactor$kwargs$date_range)
  list(
    class = class,
    rois = if (class %in% NO_STIMULATION) integer(0) else STIMULATED_ROIS,
    date_range = if (all(date_range == -1)) numeric(0) else date_range,
    start = as.numeric(metadata$date_time),
    frame_height = as.numeric(metadata$frame_height),
    frame_width = as.numeric(metadata$frame_width)
  )
}

#' Is the SD running at a given time (seconds since epoch, GMT)?
#' @noRd
sd_active <- function(stimulation, time) {
  r <- stimulation$date_range
  if (length(r) < 2) return(FALSE)
  any(time >= r[seq(1, length(r), 2)] & time <= r[seq(2, length(r), 2)])
}

#' The ROI rectangles of a recording
#' @noRd
read_roi_map <- function(dbfile) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile, flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(con))
  data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT roi_value, x, y, w, h FROM ROI_MAP"))
}

#' A snapshot with the ROIs drawn on top: white, or blue for the manipulated ROIs while the SD runs
#' @param file extracted snapshot, named <id>_<t in ms>.jpg; the date banner is above the frame
#' @param show "all"; "sd" to black out the ROIs that are not manipulated by the stimulator; "no_sd" to
#' black out the manipulated ones
#' @noRd
snapshot_with_rois <- function(file, roi_map, stimulation, show = "all") {
  dims <- jpeg_size(file)
  banner <- max(0, dims[2] - stimulation$frame_height)
  t_ms <- as.numeric(strsplit(tools::file_path_sans_ext(basename(file)), "_")[[1]][2])
  sd_on <- sd_active(stimulation, stimulation$start + t_ms / 1000)

  rects <- lapply(seq_len(nrow(roi_map)), function(i) {
    r <- roi_map[i]
    stimulated <- r$roi_value %in% stimulation$rois
    if ((show == "sd" && !stimulated) || (show == "no_sd" && stimulated)) {
      return(tags$rect(x = r$x, y = r$y + banner, width = r$w, height = r$h, fill = "black"))
    }
    blue <- sd_on && stimulated
    tags$rect(
      x = r$x, y = r$y + banner, width = r$w, height = r$h, fill = "none",
      stroke = if (blue) "#1e90ff" else "white", `stroke-width` = if (blue) 4 else 2,
      tags$title(sprintf("ROI %s", r$roi_value))
    )
  })
  image <- paste0("data:image/jpeg;base64,", jsonlite::base64_enc(readBin(file, "raw", file.info(file)$size)))

  tagList(
    div(
      style = "position: relative;",
      tags$img(src = image, style = "width: 100%; display: block;"),
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg", viewBox = sprintf("0 0 %d %d", dims[1], dims[2]),
        style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
        rects
      )
    ),
    tags$p(
      tags$small(
        sprintf("Stimulator: %s. ", stimulation$class),
        if (length(stimulation$rois) > 0) sprintf("Manipulated ROIs: %s. ", paste(stimulation$rois, collapse = ", ")) else "No ROI is manipulated. ",
        if (length(stimulation$date_range) == 0) "No SD schedule."
        else if (sd_on) tags$b(style = "color: #1e90ff;", "SD active at this time.")
        else "SD not active at this time."
      )
    )
  )
}

#' Width and height of a jpeg file, read from its header
#' @noRd
jpeg_size <- function(file) {
  bytes <- readBin(file, "raw", min(file.info(file)$size, 65536))
  i <- 3
  while (i < length(bytes) - 8) {
    if (bytes[i] != as.raw(0xFF)) { i <- i + 1; next }
    marker <- as.integer(bytes[i + 1])
    len <- as.integer(bytes[i + 2]) * 256 + as.integer(bytes[i + 3])
    if (marker %in% c(0xC0:0xC3, 0xC5:0xC7, 0xC9:0xCB, 0xCD:0xCF)) {
      h <- as.integer(bytes[i + 5]) * 256 + as.integer(bytes[i + 6])
      w <- as.integer(bytes[i + 7]) * 256 + as.integer(bytes[i + 8])
      return(c(w, h))
    }
    i <- i + 2 + len
  }
  stop("cannot read the size of ", basename(file))
}

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
  link <- snapshot_workspace(dbfile)
  if (video) {
    # the video is named after the link: a link with a unique name keeps the videos of
    # different users (made at the same time from the same recording) apart
    unique_link <- file.path(dirname(link), paste0(basename(tempfile("video")), "_", basename(link)))
    file.symlink(dbfile, unique_link)
    on.exit(unlink(unique_link), add = TRUE)
    link <- unique_link
  }
  args <- c(conf$dependencies$ethoscope_imager, "--path", link, "--id", ids)
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

#' The snapshot closest in time to a time in the data of a fly
#' @param t time in seconds since ZT0, as in the loaded data
#' @param reference_hour reference hour (ZT0) of the fly
#' @return one row of list_snapshots
#' @noRd
closest_snapshot <- function(dbfile, t, reference_hour) {
  start <- as.POSIXct(as.numeric(scopr::get_metadata(dbfile)$date_time), origin = "1970-01-01", tz = "GMT")
  # scopr: t since ZT0 = t since the recording started + ms_after_ref / 1000
  offset <- scopr::get_ms_after_ref(list(date_time = start), reference_hour) / 1000
  # computed outside shots[...]: inside it, t would be the column of the snapshots
  target_ms <- (t - offset) * 1000
  shots <- list_snapshots(dbfile)
  shots[which.min(abs(shots$t - target_ms))]
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
          radioButtons(ns("show_rois"), "Show", inline = TRUE, choices = c(
            "All flies" = "all", "Only flies under SD" = "sd", "Only flies not under SD" = "no_sd"
          )),
          uiOutput(ns("index_ui")),
          uiOutput(ns("snapshot")),
          helpText("ROIs are drawn in white; the ROIs manipulated by the stimulator turn blue while the SD schedule is active.")
        )
      )
    )
  )
}

#' @param input_rv the loaded dataset: its .db files are offered
#' @param request reactiveVal set by the Raw tab to show the snapshot closest to a time:
#' list(dbfile, t (seconds since ZT0), reference_hour)
#' @noRd
snapshotViewerServer <- function(id, input_rv, request = reactiveVal(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      extracted <- reactiveVal(NULL)
      pending <- reactiveVal(NULL)

      # show the snapshot requested from the Raw tab, once its recording is selected
      show_requested <- function() {
        r <- pending()
        pending(NULL)
        safely(context = "finding the closest snapshot", {
          shot <- closest_snapshot(r$dbfile, r$t, r$reference_hour)
          updateSelectizeInput(session, "blocks", selected = as.character(blocks()[id == shot$id, block]))
          showNotification(sprintf("Snapshot of %s GMT, the closest to ZT %.2f h", format(shot$datetime, "%Y-%m-%d %H:%M"), r$t / 3600), type = "message")
          in_background("ethoscope_imager", list(dbfile = r$dbfile, ids = shot$id), context = "extracting the snapshot", on_done = extracted)
        })
      }
      observeEvent(request(), {
        pending(request())
        if (identical(input$dbfile, request()$dbfile)) show_requested()
        else updateSelectizeInput(session, "dbfile", selected = request()$dbfile)
      })
      observeEvent(input$dbfile, {
        r <- pending()
        if (!is.null(r) && !identical(input$dbfile, r$dbfile)) updateSelectizeInput(session, "dbfile", selected = r$dbfile)
      })

      output$dbfile_ui <- renderUI({
        meta <- behavr::meta(req(input_rv$data))
        validate(need(!is_dam_dataset(input_rv$data), "Snapshots are only taken by ethoscopes: the loaded data comes from DAM monitors."))
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
        if (!is.null(pending()) && identical(pending()$dbfile, input$dbfile)) show_requested()
      })

      outputOptions(output, "dbfile_ui", suspendWhenHidden = FALSE)

      stimulation <- reactive(recording_stimulation(req(input$dbfile)))
      roi_map <- reactive(read_roi_map(req(input$dbfile)))

      output$summary <- renderTable({
        b <- blocks()
        s <- stimulation()
        r <- s$date_range
        data.frame(
          Item = c("Snapshots", "From", "To", "Stimulator", "SD schedule"),
          Value = c(
            nrow(b), format(min(b$datetime), "%Y-%m-%d %H:%M"), format(max(b$datetime), "%Y-%m-%d %H:%M"), s$class,
            if (length(r) < 2) "none" else paste(format(as.POSIXct(r, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d %H:%M"), collapse = " > ")
          )
        )
      }, colnames = FALSE)

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
        in_background(
          "ethoscope_imager", list(dbfile = input$dbfile, ids = ids),
          context = sprintf("extracting %d snapshot(s)", length(ids)),
          on_done = extracted
        )
      })

      output$index_ui <- renderUI({
        files <- req(extracted())
        sliderInput(session$ns("index"), "Snapshot", min = 1, max = length(files), value = 1, step = 1,
                    width = "100%", animate = animationOptions(interval = 1000))
      })

      output$snapshot <- renderUI({
        files <- req(extracted())
        snapshot_with_rois(files[min(req(input$index), length(files))], roi_map(), stimulation(), input$show_rois %||% "all")
      })

      output$video <- downloadHandler(
        filename = function() paste0(tools::file_path_sans_ext(basename(input$dbfile)), ".mp4"),
        content = function(file) {
          ids <- selected_ids()
          notification <- showNotification(sprintf("Making a video of %d snapshot(s)...", length(ids)), duration = NULL)
          on.exit(removeNotification(notification))
          # the video is made from extracted snapshots: extract them first (already extracted ones are reused)
          ethoscope_imager(input$dbfile, ids)
          video <- ethoscope_imager(input$dbfile, ids, video = TRUE, fps = input$fps)
          on.exit(unlink(video), add = TRUE)
          file.copy(video, file)
        },
        contentType = "video/mp4"
      )
    }
  )
}

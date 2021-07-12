HEIGHT <- 960
MAX_IDS <- 750
MAX_IDS <- 10
BLOCK_SIZE <- 600 #s 10 mins


imageModule <- function(id, image_reactive, deleteFile = TRUE, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      image_file <- reactive({
        image_file <- tempfile(fileext = ".png")
        message(image_file)
        magick::image_write(image = image_reactive(), path = image_file)
        image_file
      })

      output$image <- renderImage({
        list(
          src = image_file(),
          contentType = 'image/png',
          ...
        )}, deleteFile = deleteFile)
    }
  )
}

imageModuleUI <- function(id) {

  ns <- NS(id)
  imageOutput(ns("image"))
}



snapshotViewerUI <- function(id) {

  ns <- NS(id)
  tagList(
    wellPanel(
      selectDBFileServerUI(ns("dbfile_selector")),
      dbfileSummaryModuleUI(ns("summary"))
    ),
    wellPanel(
      snapshotManagerUI(ns("manager")),
      numericInput(ns("fps"), label = "FPS", value = 10, min = 1, max = 30),
      downloadButton(ns("video"), label = "Make .mp4 video")
    ),
    wellPanel(
      imageModuleUI(ns("snapshot")),
      # roiManagerUI(ns("roi_manager")),
      style = paste0("height:", HEIGHT, "px")
    )
  )
}


dbfileSummaryModuleUI <- function(id) {

  ns <- NS(id)
  tableOutput(ns("table"))
}

dbfileSummaryModule <- function(id, dbfile, trigger) {

  moduleServer(
    id,
    function(input, output, session) {

      summary <- eventReactive(c(dbfile(), trigger()), {

        data <- data.table::data.table(path = character(), count = integer())

        data <- rbind(
          data,
          data.table::data.table(
            path = dbfile(),
            count = sqlite(dbfile(), "SELECT COUNT(id) as count FROM IMG_SNAPSHOTS;")$count
          )
        )
        data
      }, ignoreInit = TRUE)

      output$table <- renderTable({
        summary()
      })
    }
  )
}

selectDBFileServer <- function(id, input_rv, dbfile_user=reactiveVal(NULL)) {

  moduleServer(
    id,
    function(input, output, session) {

      absolute_paths <- eventReactive(input_rv$ethoscope$time, {
        req(behavr::meta(input_rv$ethoscope$data)$file_info)
      }, ignoreInit = TRUE)

      observe({
        absolute_paths()
      })

      # just keep the first 10 characters of the dbfile name
      dbfilenames <- reactive({
        sapply(absolute_paths(), basename) %>%
          sapply(., function(x) substr(x, 1, 32))
      })

      absolute_paths_named <- reactive({
        absolute_paths_named <- absolute_paths()
        names(absolute_paths_named) <- dbfilenames()
        absolute_paths_named
      })

      path_ui <- reactive({
        selectizeInput(session$ns("path"), label = "Select your dbfiles",
                       choices = dbfilenames(), selected = dbfilenames()[1], multiple=F)
      })


      output$path_ui <- renderUI(path_ui())

      selected_file <- reactive({
        absolute_paths_named()[req(input$path)]
      })

      dbfile <- reactive({
        if(is.null(input$path)) {
          req(dbfile_user())
        } else {
          unlist(selected_file())
        }
      })

      return(dbfile)
    }
  )
}

selectDBFileServerUI <- function(id) {

  ns <- NS(id)
  uiOutput(ns("path_ui"))
}


snapshotManager <- function(id, dbfile, metadata) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(ids = NULL, outfile = NULL, snapshots = NULL)

      # if this is TRUE, switch behavior of id selector to blocks
      LONG_MOVIE <- reactiveVal(FALSE)


      ids <- reactive({
        sqlite(file = dbfile(), statement = "SELECT id FROM IMG_SNAPSHOTS;")$id
      })

      available_ids <- reactive({
        filenames <- sort(ethoscope_imager(path=dbfile()))
        filenames_without_extension <- sapply(filenames, function(x) unlist(strsplit(basename(x), split = "\\."))[1])
        ids <- as.integer(sapply(filenames_without_extension, function(x) unlist(strsplit(basename(x), split = "_"))[1]))
        #t <- as.integer(sapply(filenames_without_extension, function(x) unlist(strsplit(basename(x), split = "_"))[2]))
        unique(c(1, ids))
      })

      observe({
        if (length(ids()) > MAX_IDS) {
          LONG_MOVIE(TRUE)
        } else {
          LONG_MOVIE(FALSE)
        }
      })



      # block_structure will contain the mapping from block start -> block components
      # i.e. under key B1 we find all the ids that belong to the block that starts with id 1
      # under B30, those of id 30
      # and so on
      block_structure <- reactiveVal()

      shown_ids <- reactive({
        if (!LONG_MOVIE()) {
          ids_all <- ids()
          block_str <- as.list(ids_all)
          names(block_str) <- paste0("B", ids_all)
          block_structure(block_str)
          available_ids()[available_ids() %in% ids_all]
        } else {
          t_ms <- sort(sqlite(file = dbfile(), statement = "SELECT t FROM IMG_SNAPSHOTS;")$t)
          #t_ms <- sort(as.integer(sapply(filenames_without_extension, function(x) unlist(strsplit(basename(x), split = "_"))[2])))
          t_s <- t_ms / 1000
          start_blocks <- which(!duplicated(floor(t_s / BLOCK_SIZE)))

          block_str <- lapply(1:(length(start_blocks)-1), function(i) seq(start_blocks[i], start_blocks[i+1]-1))
          names(block_str) <- paste0("B", start_blocks[1:(length(start_blocks)-1)])
          block_structure(block_str)
          available_ids()[available_ids() %in% ids()[start_blocks]]
        }
      })

      output$ids_ui <- renderUI({
        selectizeInput(session$ns("ids"), label = "ids", choices = names(block_structure()) %>% gsub(pattern = "B", replacement = "", x = .), selected = shown_ids(), multiple=T)
      })


      observeEvent(input$sd_ids, {

        date_range <- metadata()$selected_options$interactor$kwargs$date_range

        if (! is.null(date_range)) {

          date_range <- date_range  %>% strsplit(., split = "  ") %>% unlist
          date_range <- as.numeric(as.POSIXct(date_range, tz = "GMT"))
          t_range <- round((date_range - metadata()$date_time) * 1000) # ms

          # this should return the number of shots during SD
          sql_statement <- paste0("SELECT COUNT(id) AS count FROM IMG_SNAPSHOTS WHERE t > ", t_range[1], " AND t < ", t_range[2], ";")
          if (sqlite(dbfile(), sql_statement)$count != 0) {
            sql_statement <- paste0("SELECT id FROM IMG_SNAPSHOTS WHERE t > ", t_range[1], " AND t < ", t_range[2], ";")
            ids <- sqlite(dbfile(), sql_statement)$id
            updateSelectizeInput(inputId = "ids", selected = ids)

          } else {
            message("No snapshots detected during SD")
            updateSelectizeInput(inputId = "ids", selected = c(""))
            showNotification(ui = "No snapshots detected during SD", type = "warning")

          }
        } else {
          message("Interactor has no date_range")
          updateSelectizeInput(inputId = "ids", selected = c(""))

        }
      }, ignoreInit = FALSE)


      observeEvent(input$clear, {
        updateSelectizeInput(inputId = "ids", selected = c(""))
      })


      observeEvent(input$all_ids, {
        updateSelectizeInput(inputId = "ids", selected = ids())
      })

      observeEvent(input$annotate, {
        message("Updating snapshot list")
        if (LONG_MOVIE()) {
          ids <- sapply(input$ids[1:(length(input$ids)-1)], function(id) {
            #print(id);
            block_structure()[[paste0("B", id)]]
          }) %>% unlist %>% as.character
        } else {
          ids <- input$ids
        }
        output_rv$snapshots <- ethoscope_imager(path=dbfile(), id=ids)
      })

      selected_shot <- reactiveVal(1)

      index_ui <- reactive({
        sliderInput(session$ns("index"), label = "Shot", min = 1, max = length(req(output_rv$snapshots)), value = 1, step = 1)
      })


      output$index_ui <- renderUI({index_ui()})

      observeEvent(input$index, selected_shot(input$index), ignoreInit = TRUE)
      observeEvent(input$annotate, {
        updateSliderInput(session = session, inputId = "index", value = selected_shot())
      })


      observe({
        output_rv$outfile <- output_rv$snapshots[as.integer(req(input$index))]
      })

      return(output_rv)
    }
  )
}

snapshotManagerUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("ids_ui")),
    actionButton(ns("annotate"), label = "Annotate"),
    actionButton(ns("all_ids"), label = "All snapshots"),
    actionButton(ns("sd_ids"), label = "SD only"),
    actionButton(ns("clear"), label = "Clear"),
    uiOutput(ns("index_ui"))

  )
}


roiManager <- function(id, image, dbfile) {

  moduleServer(
    id,
    function(input, output, session) {
      roi_map <- reactive({

        roi_map <- as.data.table(sqlite(req(dbfile()), "SELECT roi_value,x,y,w,h FROM ROI_MAP"))
        width <- magick::image_info(image())$width

        roi_map[, side := NA_character_]
        roi_map[x < (width / 2), side := "left"]
        roi_map[x > (width / 2), side := "right"]
        roi_map
      })

      datetime_banner <- reactive({
        width <- magick::image_info(image())$width
        magick::image_crop(image(), paste0(width, "x", ANNOTATION_OFFSET, "+0+0"))
      })

      rois <- reactive({
        crop_regions(req(image()), req(roi_map()), offset=c(0, ANNOTATION_OFFSET))
      })
      rois_pasted <- reactive({
        paste_regions(rois(), roi_map())
      })

      gears_left <- reactive({
        crop_regions(req(image()), find_gears(req(roi_map()), "left"), offset=c(0, ANNOTATION_OFFSET))
      })
      gears_left_pasted <- reactive({
        paste_regions(gears_left(), find_gears(req(roi_map()), "left"))
      })

      gears_left <- reactive({
        crop_regions(req(image()), find_gears(req(roi_map()), "right"), offset=c(0, ANNOTATION_OFFSET))
      })
      gears_right_pasted <- reactive({
        paste_regions(gears_left(), find_gears(req(roi_map()), "right"))
      })

      rois_with_banner <- reactive({

        w <- magick::image_info(rois_pasted())$width
        banner_resized <- magick::image_scale(datetime_banner(), as.character(w))
        magick::image_append(c(banner_resized, rois_pasted()), stack=TRUE)
      })

      # imageModule("gears_left", gears_left_pasted, width=80, height = HEIGHT, alt = "gears_left")
      # imageModule("rois", rois_with_banner, width=1280, height = HEIGHT, alt = "ROIS")
      # imageModule("gears_right", gears_right_pasted, width=80, height = HEIGHT, alt = "gears_right")

    }
  )
}


roiManagerUI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(2, imageModuleUI(ns("gears_left"))),
    column(8, imageModuleUI(ns("rois"))),
    column(2, imageModuleUI(ns("gears_right")))
  )
}
#' Generate mp4 videos from a list of dbfiles
#' Ethoscopes automate high throughput behavioral tracking
#' however, their performance is not perfect.
#' Errors in the tracker or the SD delivery can bias the results
#' which is why the possibility to semi supervise the process is needed
#' Even if not needed, it is still useful to look at the rawest form of data: the images / videos
#' This module takes a set of dbfiles (from the metadata of the loaded dataset)
#' and produces a video that users can download.
#' The resulting frames are saved in the experiment_folder with name IMG_SNAPSHOTS
#' for manual frame-by-frame inspection
#' @import data.table
#' @import shiny
snapshotViewerServer <- function(id, input_rv, dbfile_user = reactiveVal(NULL), trigger=reactiveVal(0)) {

  moduleServer(
    id,
    function(input, output, session) {


      dbfile <- selectDBFileServer("dbfile_selector", input_rv, dbfile_user)

      metadata <- reactive({
        get_metadata(dbfile())
      })


      dbfileSummaryModule("summary", dbfile, trigger)
      manager <- snapshotManager("manager", dbfile, metadata)
      image <- reactive({
        req(manager$outfile)
        magick::image_read(manager$outfile)
      })

      ## --- ROIS
      imageModule("snapshot", image, width=1280, height = HEIGHT, alt = "snapshot")
      roiManager("roi_manager", image, dbfile)

      output$video <- downloadHandler(filename = function() {
        paste0(dbfile(), ".mp4")
      }, content = function(file) {
        video <- ethoscope_imager(path = dbfile(), video=TRUE, fps=input$fps)
        file.copy(video, file)
      })
    }
  )
}

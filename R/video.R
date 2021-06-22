ethoscope_imager <- function(path, id=NULL, video=F, fps=NULL) {

  binary <- "/home/antortjim/anaconda3/bin/python"
  script <- "/home/antortjim/Dropbox/FSLLab/Git/ethoscope-imager/imager.py"
  cmd <- paste0(binary, " ", script, " --path ", path)
  if (!is.null(id)) {
    cmd <- paste0(cmd, " --id ", paste0(id, collapse=" "))
  }

  if (video) {
    cmd <- paste0(cmd, " --video", " --fps", " ", fps)
  }

  print(cmd)
  files <- system(
    command = cmd,
    intern = TRUE
  )
  return(files)
}

#' @importFrom magick image_crop
crop_roi <- function(img, x, y, w, h, border=0) {
  img <- magick::image_crop(img, paste0(w, "x", h, "+", x, "+", y))
  if (border != 0) img <- magick::image_border(img, "#000080", paste0(border, border, sep="x"))
  img
}

append_roi <- function(roi_list, ...) {
  pairwise_append <- function(roi1, roi2) {
    magick::image_append(image = c(roi1, roi2), ...)
  }
  Reduce(pairwise_append, roi_list)
}


ANNOTATION_OFFSET <- 51
ANNOTATE <- TRUE

snapshotViewerUI <- function(id) {

  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("path_ui")),
      tableOutput(ns("summary"))
    ),
    wellPanel(
      uiOutput(ns("ids_ui")),
      actionButton(ns("annotate"), label = "Annotate"),
      uiOutput(ns("index_ui")),
      numericInput(ns("fps"), label = "FPS", value = 10, min = 1, max = 30),
      downloadButton(ns("video"), label = "Make .mp4 video")
    ),
  # wellPanel(
  #   imageOutput(ns("viewer"))
  # ),
  wellPanel(
    imageOutput(ns("rois"))
  )

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
snapshotViewerServer <- function(id, input_rv, dbfile=reactiveVal(NULL), trigger=reactiveVal(0)) {

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

      the_dbfile <- reactive({
        if(is.null(input$path)) {
          req(dbfile())
        } else {
          selected_file()
        }
      })


      summary <- eventReactive(c(the_dbfile(), trigger()), {

        data <- data.table::data.table(path = character(), count = integer())

        # for (i in 1:length(the_dbfile())) {
          data <- rbind(
            data,
            data.table::data.table(
              path = ifelse(is.null(input$path), dbfile(), input$path),
              count = sqlite(the_dbfile(), "SELECT COUNT(id) as COUNT FROM IMG_SNAPSHOTS;")$COUNT
            )
          )
        # }
        data
      }, ignoreInit = TRUE)

      output$summary <- renderTable({
        summary()
      })

      ids <- reactive({
        sqlite(the_dbfile(), "SELECT id FROM IMG_SNAPSHOTS;")$id
      })

      available_ids <- reactive({
        filenames <- ethoscope_imager(path=the_dbfile())
        ids <- as.integer(sapply(filenames, function(x) unlist(strsplit(basename(x), split = "_"))[1]))
        unique(c(1, ids))
      })

      # observeEvent(ids(), {
      #   browser()
      #   available_ids()
      # })

      output$ids_ui <- renderUI({
        selectizeInput(session$ns("ids"), label = "ids", choices = ids(), selected = available_ids(), multiple=T)
      })


      files <- eventReactive(input$annotate, {
        ethoscope_imager(path=the_dbfile(), id=input$ids)
      })


      selected_shot <- reactiveVal(1)

      index_ui <- reactive({
        sliderInput(session$ns("index"), label = "Shot", min = 1, max = length(files()), value = 1, step = 1)
      })


      output$index_ui <- renderUI({index_ui()})

      observeEvent(input$index, selected_shot(input$index), ignoreInit = TRUE)
      observeEvent(input$annotate, {
        updateSliderInput(session = session, inputId = "index", value = selected_shot())
      })

      outfile <- reactive({

        outfile <- files()[as.integer(req(input$index))]
        message(outfile)
        outfile
      })


      output$video <- downloadHandler(filename = function() {
        paste0(the_dbfile(), ".mp4")
      }, content = function(file) {
        video <- ethoscope_imager(path = the_dbfile(), video=TRUE, fps=input$fps)
        file.copy(video, file)
      })



      # output$viewer <- renderImage({
      #
      #   # Return a list containing the filename
      #   list(src = outfile(),
      #        contentType = 'image/png',
      #        width = as.integer(1280/5),
      #        height = as.integer(960/5),
      #        alt = "Snapshot")
      # }, deleteFile = FALSE)


      ## --- ROIS

      roi_map <- reactive({
        as.data.table(sqlite(req(the_dbfile()), "SELECT roi_value,x,y,w,h FROM ROI_MAP"))
      })

      image <- reactive({
        magick::image_read(outfile())
      })

      datetime_banner <- reactive({
        w <- magick::image_info(image())$width
        magick::image_crop(image(), paste0(w, "x", ANNOTATION_OFFSET, "+0+0"))
      })

      rois <- reactive({
        req(image())
        req(roi_map())
        # TODO Find a nice value that is always more than what the ethoscopes give us
        # for both w and h
        w <- max(roi_map()$w)
        h <- max(roi_map()$h)

        roi_images <- lapply(1:nrow(roi_map()), function(roi) {
          args <- roi_map()[roi_value == roi, .(x,y)] %>% as.list
          # I cannot use the roi's w and h because it must be the same for all rois
          args$w <- w
          args$h <- h
          if (ANNOTATE) args$y  <- args$y + ANNOTATION_OFFSET
          args <- append(list(img=image(), border=0), args)
          do.call(crop_roi, args)
        })

        names(roi_images) <- roi_map()$roi_value

        column1 <- append_roi(lapply(1:10, function(i) roi_images[[i]]), stack=TRUE)
        column2 <- append_roi(lapply(11:20, function(i) roi_images[[i]]), stack=TRUE)
        rois <- magick::image_append(c(column1, column2), stack=F)
      })

      rois_with_banner <- reactive({

        w <- magick::image_info(rois())$width
        banner_resized <- magick::image_scale(datetime_banner(), as.character(w))
        magick::image_append(c(banner_resized, rois()), stack=TRUE)
      })

      roi_file <- reactive({
        roi_file <- tempfile(fileext = ".png")
        magick::image_write(image = rois_with_banner(), path = roi_file)
        roi_file
      })


      output$rois <- renderImage({
        list(
          src = roi_file(),
          contentType = 'image/png',
          width = 1280,
          height = 960,
          alt = "Cropped rois"
      )}, deleteFile = TRUE)

    }
  )
}
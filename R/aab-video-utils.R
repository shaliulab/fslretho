ANNOTATION_OFFSET <- 51
ANNOTATE <- TRUE
GEAR_WIDTH <- 50

find_gears <- function(roi_map, roi_side) {
  roi_map <- data.table::copy(roi_map)
  roi_map[side == "left", x := x - GEAR_WIDTH]
  roi_map[side == "right", x := x + w]
  roi_map[, y := y - 10]
  roi_map[, w := GEAR_WIDTH]
  roi_map[, h := 50]
  roi_map[side %in% roi_side, ]
  return(roi_map)
}



crop_regions <- function(loaded_image, roi_map, offset = c(0, 0)) {
  width <- magick::image_info(loaded_image)$width
  height <- magick::image_info(loaded_image)$height

  # roi_map <- roi_map[roi_value %in% c(1, 2, 11, 12), ]

  # TODO Find a nice value that is always more than what the ethoscopes give us
  # for both w and h
  w <- max(roi_map$w) # to include the gear
  h <- max(roi_map$h)


  roi_images <- lapply(roi_map$roi_value, function(roi) {
    args <- roi_map[roi_value == roi, .(x,y)] %>% as.list
    # I cannot use the roi's w and h because it must be the same for all rois
    args$w <- w
    args$h <- h
    args$x  <- args$x + offset[1]
    args$y  <- args$y + offset[2]

    args <- append(list(border=0), args)
    args <- append(list(img=loaded_image), args)
    do.call(crop_roi, args)
  })

  names(roi_images) <- paste0("ROI_", roi_map$roi_value)

  return(roi_images)
}

#' Paste several images together
#' @param roi_images list where every element is a magick image object
#' storing the image of a ROI. The element must be named ROI_X
#' @param roi_map data.table providing a roi_value - side mapping
#' @return magick image with all ROIs pasted together
paste_regions <- function(roi_images, roi_map) {
  column1 <- append_roi(lapply(roi_map[side == "left", roi_value], function(i) roi_images[[paste0("ROI_", i)]]), stack=TRUE)
  column2 <- append_roi(lapply(roi_map[side == "right", roi_value], function(i) roi_images[[paste0("ROI_", i)]]), stack=TRUE)
  rois <- magick::image_append(c(column1, column2), stack=F)
  rois
}

#' Save ethoscope snapshots to disk with Python
#' The sqlite3 library in Python is well suited to interface
#' with dbfiles produced in the ethoscope platform
#' @param path Absolute path to a sqlite3 file produced with an ethoscope
#' @param id Numeric vector of ids matching ids in the IMG_SNAPSHOTS table of the dbfile
#' @param video If TRUE, produce a video with the supplied ids.
#' Alternatively, if no ids are passed,
#' then with all the cached (already saved outside of the dbfile) snapshots
#' @param fps Frame rate of the resulting video
ethoscope_imager <- function(path, id=NULL, video=F, fps=NULL) {

  conf <- FSLRethoConfiguration$new()
  binary <- conf$content$binaries$python
  script <- conf$content$dependencies$ethoscope_imager

  cmd <- paste0(binary, " ", script, " --path ", path)
  if (!is.null(id)) {
    cmd <- paste0(cmd, " --id ", paste0(id, collapse=" "))
  }

  if (video) {
    cmd <- paste0(cmd, " --video", " --fps", " ", fps)
  }

  print(cmd)
  files <- tryCatch({
    system(
      command = cmd,
      intern = TRUE
    )},
    error = function(e) {
      message("Could not run ethoscope_imager")
      warning(e)
    }
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




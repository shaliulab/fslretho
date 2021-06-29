#' Quality control module
#' Functionality to ensure data issues are exposed
#' # Check the light intensity is stable through the experiment
#'  An unstable IR strip can cause the data to be corrupted because the ethoscope
#'  tracker is designed to track pixel intensity changes.
#'  In normal conditions, they follow the animal behavior,
#'  however an unstable IR strip can produce intensity changes independent of the animal
#'  which confuses the tracker and bogs the results
#'
#' ## Check the flies move through out the tube during SD,
#' as opposed to stay in only a tiny fraction of the tube
#' SD is another component contributing to background instability for obvious reasons:
#' if the tube moves, there will be several pixels in the ROI that will change,
#' independent of the animal. The tracker is robust,
#' but sometimes this changes are noisy enough to again confuse the tracker.
#' This, combined with the fact that a very sleepy fly can actually not react
#' to a moving tube, drive the labeling algorithm to label a fly as awake when it really is asleep


#' Apply Pareto rule to wake behavior during SD
#' Require significant tube exploration to label behavior as awake
#' Minor movements (indicating little awakeness) or spiky artifacts during real sleep
#' both result in a very small fraction of the tube being explored by the animal in a given interval of time
#' This function checks at least > 20% of the tube is explored,
#' and otherwise labels the animal as sleep
#' This make sense to do during SD because the tracking quality decreases due to
#' background changes produced by the SD itself
#' @param x Position of the animal along the main axis of variation, normalized from 0 to 1
#' @param n Number of splits in the ROI. default 10
#' @return TRUE if the tube exploration is < expected with Pareto and FALSE otherwise
#' A result of TRUE indicates thus a problematic data
#' Apply Pareto rule to wake behavior during SD
#' @param x Position of the animal along the main axis of variation, normalized from 0 to 1
#' @return TRUE if the tube exploration is < expected with Pareto and FALSE otherwise
#' A result of TRUE indicates thus a problematic data
#' @usage dt <- behavr::bin_apply_all(dt_raw, x="t", y="x",  x_bin_length=behavr::mins(30), FUN=pareto_sd)
#' @example
#' \dontrun{
#' dt_raw <- behavr::toy_ethoscope_data()
#' dt <- sleepr::sleep_annotation(dt_raw)
#' dt_pareto <- behavr::bin_apply_all(data = dt_raw, y = "x", x = "t", x_bin_length = behavr::mins(30), FUN=pareto_sd)
#' setkey(dt_pareto, id, t)
#' setkey(dt, id, t)
#' dt_qc <- dt[dt_pareto]
#' setkey(dt_qc, id)
#' }

pareto_sd <- function(x, n = 10, min_max_crop=TRUE, p=0.2, accum=0.8) {

  #     if (min_max_crop) x <- (x  - min(x)) / max(x)

  bins <- seq(from = 0, to = 1 - 1/n, length.out=n)

  #divide the tube in n regions
  x_round <- floor(x * n) / n

  # compute fraction of datapoints on each region
  split_count <- sapply(bins, function(b) sum(abs(b - x_round) < 0.00001))
  split_fraction <- split_count / sum(split_count)

  names(split_fraction) <- paste0(bins, " %")
  # get the up to 40% most frequent regions
  n_splits <- min(n*p, length(split_fraction))
  top_p <- sort(split_fraction, decreasing = T)[1:n_splits]

  # did the animal spend more than accum % of the time in these p regions?
  more_than_accum_time_in_top_p <- sum(top_p) > accum
  #     if (is.na(more_than_accum_time_in_top_p)) {
  #     }
  return(more_than_accum_time_in_top_p)
}

setattr(pareto_sd, "var_name", "pareto")


#' Wrap bin_apply_all and pareto_sd
#' @param scored_dataset behavr timeseries data with at least columns x and t
#' @param binned_dataset behavr timeseries produced by applying bin_apply_all with a function other than pareto_sd,
#' typically the mean
#' @return a new binned_dataset with an extra field 'pareto' stating whether the animal fulfills the pareto principle of
#' awakeness for the given bin.
#' @rdname pareto_sd
#' @export
#'
apply_pareto_rule <- function(scored_dataset, binned_dataset, ...) {
  pareto_dataset <- behavr::bin_apply_all(
    data = scored_dataset,
    y = "x",
    x = "t",
    FUN = pareto_sd,
    ...
  )

  setkey(binned_dataset, id, t)
  setkey(pareto_dataset, id, t)
  merged_dataset <- merge_behavr_all(binned_dataset, pareto_dataset)
  merged_dataset[, asleep := sapply(as.numeric(pareto * 1) + asleep, function(a) min(1, a))]
  setkey(merged_dataset, id)
  return(merged_dataset)
}


load_ethoscope_qc <- function(metadata) {

  qc <- tryCatch({
    qcs <- lapply(metadata$file_info, function(x) {
      message(paste0("Loading QC from ", x$path))
      qc <- data.table::as.data.table(sqlite(file = x$path, "SELECT * FROM QC;"))
      roi_map <- data.table::as.data.table(sqlite(file = x$path, "SELECT * FROM ROI_MAP;"))
      ids <- paste0(substr(basename(x$path), 1, 26), "|", stringr::str_pad(string = roi_map$roi_value, width = 2, pad = "0"))

      ids <- rep(ids, each=nrow(qc))
      qc <- qc[rep(1:nrow(qc), times=nrow(roi_map))]
      qc$id <- as.factor(ids)
      setkey(qc, id, t)
      qc
    })

    qcs <- lapply(qcs, function(x) {
      x[, .(id, t, mean, min, max)]
    })


    qc <- do.call(rbind, qcs)
    setkey(qc, id)
    meta <- data.table(id = unique(qc$id))
    setkey(meta, id)
    behavr::setmeta(qc, meta)
    qc
  }, error = function(e) {
    warning(e)
    message("Cant load QCs")
    NULL
  })
  return(qc)
}

# metadata <- data.table(id = "")
# metadata[, file_info := NA]
# metadata[, file_info := list(list(path = "/ethoscope_data/results/008aad42625f433eb4bd2b44f811738e/ETHOSCOPE_008/2021-06-18_16-52-43/2021-06-18_16-52-43_008aad42625f433eb4bd2b44f811738e.db", file = ""))]
# get_qc_table(metadata)

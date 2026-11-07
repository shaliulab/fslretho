#' Annotate a behavr with a SD treatment progress
#' A new `sd_on` column stating the SD treatment status is added to the passed behavr
#' @param data behavr table
#' @return the same behavr table with a new column `sd_on`.
#' @export
sd_inprogress_annotation <- function(data, time_window_length=10, ...) {

  d <- sleepr::prepare_data_for_motion_detector(data, c("id", "t"), time_window_length=time_window_length, curate=F)
  meta <- behavr::meta(d)
  path <- sapply(meta$file_info, function(x) x$path)
  d_small <- d
  # d_small <- d[!duplicated(t_round), ]
  d_small$t <- NULL
  data.table::setnames(d_small, "t_round", "t")
  # d_small must have keys like the data in a behavr
  data.table::setkey(d_small, id)
  behavr::setmeta(d_small, behavr::meta(data))
  d_small <- parse_sd_daterange(d_small)
  d_small[, sd_on := ((t > xmv(start_sd)) & (t < xmv(end_sd)))]
  return(d_small)
}
attr(sd_inprogress_annotation, "needed_columns") <- function() {
  c()
}
attr(sd_inprogress_annotation, "variables") <- function() {
  c("sd_on")
}
attr(sd_inprogress_annotation, "parameters") <- function() {
  c()
}
attr(sd_inprogress_annotation, "use_meta") <- function() {
  TRUE
}

minimal_micromovement_detector <- function(data,
                                           mm_window_length,
                                           mm_threshold = 20,
                                           masking_duration = 6,
                                           curate=TRUE
                                           ) {

  d <- sleepr::prepare_data_for_motion_detector(data,
                                   c("t", "body_movement"),
                                   mm_window_length,
                                   "has_interacted",
                                   curate=curate)

  ## Masking
  ## Intended to ignore activity within masking_duration (default 6) seconds
  ## after an interaction happens
  ## ----
  if(!"has_interacted" %in% colnames(d)){
    if(masking_duration > 0)
      warning("Data does not contain an `has_interacted` column.
              Cannot apply masking!.
              Set `masking_duration = 0` to ignore masking")
    d[, has_interacted := 0]
  }

  # create a unique identifier for every interaction
  # this is done so we can split d
  # so rows within the same block share last interaction
  d[,interaction_id := cumsum(has_interacted)]

  # masked becomes TRUE if t is within masking_duration
  # after t[1]
  # since the first row of the block
  # represents the last interaction
  # the time of the interaction is t[1]
  d[,
    masked := t < (t[1] + masking_duration),
    by=interaction_id
  ]

  d[ , body_movement := ifelse(masked & interaction_id != 0, 0, body_movement)]

  # remove the interaction_id and masked columns
  # to preserve state
  d[,interaction_id := NULL]
  d[,masked := NULL]


  d_small <- d[, .(
    max_movement = max(body_movement[min(2, .N):.N]),
    interactions = as.integer(sum(has_interacted))
  ), by = "t_round"]

  d_small[, micromovement := max_movement > mm_threshold]

  data.table::setnames(d_small, "t_round", "t")
  return(d_small)
}

microbehavior_annotation <- function(
  data,
  mm_window_length = 10, #s
  mm_time_immobile = 1, #s
  motion_detector_FUN = minimal_micromovement_detector,
  columns_to_keep = c("t", "max_movement", "interactions","microbehavior", "micromovement",
                      "beam_crosses", "is_interpolated"),
  ...
){

  wrapped <- function(d){
    if(nrow(d) < 100) {
      warning("Dataset is very small")
      return(NULL)
    }


    # todo if t not unique, stop
    d_small <- motion_detector_FUN(d, mm_window_length,...)

    if(key(d_small) != "t")
      stop("Key in output of motion_classifier_FUN MUST be `t'")

    if(nrow(d_small) < 1)
      return(NULL)
    # the times to  be queried
    time_map <- data.table::data.table(t = seq(from=d_small[1,t], to=d_small[.N,t], by=mm_window_length),
                                       key = "t")
    missing_val <- time_map[!d_small]

    d_small <- d_small[time_map,roll=T]
    d_small[,is_interpolated := FALSE]
    d_small[missing_val,is_interpolated:=TRUE]
    d_small[is_interpolated == T, max_movement := 0]
    d_small[is_interpolated == T, micromovement := F]
    d_small[, microbehavior := sleepr:::sleep_contiguous(!micromovement,
                                                         fs=1/mm_window_length,
                                                         min_valid_time=mm_time_immobile)]

    d_small <- stats::na.omit(d[d_small,
                                on=c("t"),
                                roll=T])
    d_small[, intersect(columns_to_keep, colnames(d_small)), with=FALSE]
  }

  # browser()

  if(is.null(key(data)))
    return(wrapped(data))
  data[,
       wrapped(.SD),
       by=key(data)]

}


attr(microbehavior_annotation, "needed_columns") <- function(motion_detector_FUN = minimal_micromovement_detector,
                                                     ...){
  needed_columns <- attr(motion_detector_FUN, "needed_columns")
  if(!is.null(needed_columns))
    needed_columns(...)
}

attr(microbehavior_annotation, "variables") <- function() {
  c("microbehavior", "micromovement")
}

attr(microbehavior_annotation, "parameters") <- function(motion_detector_FUN = minimal_micromovement_detector, ...) {

  # arguments of microbehavior_annotation
  args <- names(formals(microbehavior_annotation))
  args <- unique(c(args, names(formals(minimal_micromovement_detector))))
  args <- args[args != "..."]
  args <- args[args != "data"]
  args <- args[args != "motion_detector_FUN"]
  return(args)
}

integrate_annotations <- function(data) {

  if (all(c("micromovement", "asleep") %in% colnames(data))) {
    data[, micromovement := micromovement & asleep]
  }
  if (all(c("microbehavior", "asleep") %in% colnames(data))) {
    data[, microbehavior := microbehavior & asleep]
  }
  return(data)
}

#' Annotations the user can choose from in the scoring parameters
#' @noRd
ANNOTATIONS <- list(
  `sleep annotation` = sleepr::sleep_annotation,
  `distance annotation` = sleepr::sum_movement_detector,
  `SD in-progress annotation` = sd_inprogress_annotation,
  `Microbehavior annotation` = microbehavior_annotation
)


#' Incorporate daterange information to a behavr table
#' @param dt A behavr table
#' @return The same behavior table where the metadata now features columns start_sd and end_sd
#' which contain the number of seconds since ZT0 until SD start and end
#' @noRd
parse_sd_daterange <- function(dt) {

  meta <- behavr::meta(dt)
  sd_range <- lapply(seq_len(nrow(meta)), function(i) scopr::load_sd_daterange(meta[i, , drop = FALSE]))
  daterange <- data.table::data.table(
    id = meta$id,
    start_sd = as.numeric(sapply(sd_range, `[`, 1)),
    end_sd = as.numeric(sapply(sd_range, `[`, 2)),
    key = "id"
  )
  meta <- meta[daterange]
  behavr::setmeta(dt, meta)
  dt
}

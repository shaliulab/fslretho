#' Help tab: how the app works and what every parameter means

help_table <- function(rows) {
  tags$table(
    class = "fsl-help-table",
    tags$tbody(lapply(rows, function(r) tags$tr(tags$td(tags$b(r[[1]])), tags$td(r[[2]]))))
  )
}

help_workflow <- function() {
  tagList(
    tags$p("FSLRetho turns the tracking data recorded by ethoscopes into sleep and circadian rhythm measurements."),
    tags$ol(
      tags$li(tags$b("Load"), ": upload a metadata file (one row per fly). The app checks it, finds the recordings in the ethoscope database and scores sleep."),
      tags$li(tags$b("Scoring parameters"), " (top bar): decide when a fly counts as moving and when as asleep. Changing them re-scores the loaded data."),
      tags$li(tags$b("Sleep / Bout binning"), " (top bar): choose the bin length and how each bin is summarised."),
      tags$li(tags$b("Sleep analysis / Bout analysis"), ": plot the binned data (top) and one value per fly (bottom)."),
      tags$li(tags$b("Period analysis"), ": periodograms and spectrograms of the data shown in Sleep analysis, with its filters applied."),
      tags$li(tags$b("Raw"), ": look at the tracking of a single fly, to check that the scoring makes sense."),
      tags$li(tags$b("Snapshot viewer"), ": the pictures the ethoscope took during the experiment, as images or as a video."),
      tags$li(tags$b("Download datasets"), " (top bar): export any table as .csv; ", tags$b("Reproducibility script"), ": the same analysis as an R script.")
    ),
    tags$p("Times are in hours since ZT0 (lights on) of the day the experiment started, as given by the reference_hour of each fly.")
  )
}

help_metadata <- function() {
  tagList(
    tags$p(
      "The metadata is a .csv file with one row per fly and one column per piece of information. ",
      "The app supports ethoscopes and DAM monitors; it tells which one a file is for from its columns ",
      "(one kind per file)."
    ),
    tags$h4("Ethoscopes"),
    tags$p("Example:"),
    tags$pre("machine_name,date,region_id,reference_hour,genotype\nETHOSCOPE_008,2021-06-18,1,9,wild type\nETHOSCOPE_008,2021-06-18,2,9,mutant"),
    help_table(list(
      list("machine_name", "Name of the ethoscope, exactly as in the database, e.g. ETHOSCOPE_008."),
      list("date", "Day the ethoscope started recording, as YYYY-MM-DD (e.g. 2021-06-18). Not the day the SD or the experiment of interest started."),
      list("region_id", "Number of the tube (ROI), 1 to 20. Without this column, all 20 tubes are loaded with the same metadata."),
      list("reference_hour", "Hour of the day when the lights turn on (ZT0), e.g. 9 or 9.5. All times in the app are relative to it."),
      list("time (optional)", "Time the recording started, as HH:MM:SS. Only needed if the ethoscope was started more than once that day; otherwise the last recording of the day is used."),
      list("Any other column", "Genotype, sex, treatment... They can be used to colour, group and filter the plots. Cells must not be empty: write NONE instead.")
    )),
    tags$h4("DAM monitors"),
    tags$pre("file,start_datetime,stop_datetime,region_id,genotype\nMonitor22.txt,2019-06-17 08:00:00,2019-06-20,1,wild type\nMonitor22.txt,2019-06-17 08:00:00,2019-06-20,2,mutant"),
    help_table(list(
      list("file", "Name of the monitor file, relative to the DAM result folder (set in the Load tab), e.g. Monitor22.txt."),
      list("start_datetime", "First day and time to load, as YYYY-MM-DD HH:MM:SS. It is also ZT0: use the time the lights turn on."),
      list("stop_datetime", "Last day (YYYY-MM-DD) or day and time to load."),
      list("region_id", "Channel of the monitor, 1 to 32. Without this column, all 32 channels are loaded with the same metadata."),
      list("Any other column", "As for ethoscopes.")
    )),
    tags$p("The check also verifies that the monitor files exist and have data between start_datetime and stop_datetime."),
    tags$h4("Problems with Excel and other spreadsheet programs"),
    tags$p(
      "Spreadsheet programs often change dates when they open or save a .csv. The date can end up as a number ",
      "(e.g. 44365, the number of days since 30 December 1899, which means 2021-06-18), or in your regional format ",
      "(e.g. 18/06/2021). The app does not recognise those as dates. When this happens, the metadata check tells you ",
      "which rows are affected and which date they most likely mean."
    ),
    tags$p(
      "To avoid it, format the date column as Text before typing the dates (right click > Format cells > Text), ",
      "or check the .csv in a text editor before uploading it. Excel files saved in some regions use ; instead of , ",
      "as separator: the app accepts both."
    ),
    tags$h4("The metadata check"),
    tags$p(
      "Before loading, every row is checked: missing columns, empty cells, dates, times, reference hours and region_ids ",
      "that cannot be read, duplicated flies, and ethoscopes or dates that are not in the database ",
      "(listing the closest recordings that do exist). All problems are listed at once, with the rows as numbered in Excel. ",
      "Flies whose tube has no tracking data are skipped and listed, instead of stopping the whole load."
    )
  )
}

help_scoring <- function() {
  tagList(
    tags$p(
      "The ethoscope records the position of each fly several times per second. The app splits the recording into ",
      "short windows and decides, for each window, whether the fly moved. A fly is ", tags$b("asleep"),
      " when it has not moved for at least the minimum time immobile (classically 5 minutes)."
    ),
    help_table(list(
      list("Window length", "Length (s) of the windows the data is split into (default 10 s). Each window is scored as moving or not moving."),
      list("Movement threshold (velocity correction coefficient)",
           "A window is scored as moving if, between two consecutive frames, the fly moved more than this fraction of the tube width. Lower values detect smaller movements (less sleep); higher values ignore them (more sleep). Default 0.0048."),
      list("Minimum time immobile", "Time (s) a fly must stay immobile to be considered asleep. 300 s is the standard 5 minute rule."),
      list("Annotations", tagList(
        tags$b("sleep annotation"), ": the moving and asleep variables (always needed). ",
        tags$b("SD in-progress annotation"), ": the sd_on variable, TRUE while the sleep deprivation is running. ",
        tags$b("distance annotation"), ": the distance moved in each window. ",
        tags$b("Microbehavior annotation"), ": small movements (e.g. grooming) detected while the fly is otherwise asleep."
      )),
      list("Curate", "Discard minutes with too little tracking data (20 positions or fewer per minute), typically when the fly was not detected.")
    )),
    tags$p(
      "Movement in the 6 s after the sleep depriver moves the tube is ignored, so the SD itself does not count as the fly waking up."
    ),
    tags$h4("DAM monitors"),
    tags$p(
      "DAM monitors count how many times the fly crosses an infrared beam each minute (", tags$code("activity"), "). ",
      "A minute with at least one beam cross is a minute moving, and the fly is asleep after the minimum time immobile without crossing. ",
      "The other parameters (window length, movement threshold, annotations, curate, SD) only apply to ethoscopes, ",
      "and so do the Raw tab and the snapshot viewer."
    ),
    tags$h4("Different parameters during sleep deprivation (SD)"),
    tags$p(
      "The tube rotations of the SD can make the tracking noisier. Tick ", tags$i("Use different parameters during SD"),
      " to score the SD period with its own window length, threshold and minimum time immobile. The SD period is read ",
      "from the SD schedule (date range) set in the ethoscope, e.g. ", tags$code("2021-06-19 17:00:00 > 2021-06-20 05:00:00"),
      ". The data then has an ", tags$code("interval"), " column (default or sd) telling which parameters were used."
    )
  )
}

help_binning <- function() {
  tagList(
    tags$p(
      "The scored data has one row every few seconds. To plot it, it is summarised in bins (default 30 minutes): ",
      "each bin gets one value per fly and variable."
    ),
    help_table(list(
      list("Bin length", "Length of each bin, in minutes."),
      list("mean", "Average over the bin. For asleep, this is the fraction of the time spent asleep (0 to 1)."),
      list("median, max, min", "Median, maximum and minimum value in the bin."),
      list("P_doze", "Probability that an awake fly falls asleep from one window to the next (P(doze | wake))."),
      list("P_wake", "Probability that an asleep fly wakes up from one window to the next (P(wake | doze)).")
    )),
    tags$p(
      "Sleep binning applies to the Sleep analysis tab, Bout binning to the Bout analysis tab. During SD, ",
      tags$code("sd_on"), " is TRUE only for bins where the SD was running the whole time, and ",
      tags$code("pareto"), " flags bins where an awake fly stayed in a small part of the tube (a sign of poor tracking)."
    ),
    tags$h4("Bouts"),
    tags$p(
      "A bout is a run of consecutive windows in the same state. The bout analysis lists every sleep and wake bout with its ",
      tags$code("duration"), " (seconds) and start time, and bins them by start time: ",
      tags$code("duration"), " is then the average length of the bouts that started in the bin."
    ),
    tags$h4("Summaries"),
    tags$p(
      "The plot at the bottom of the Sleep and Bout tabs shows one value per fly: the mean, standard deviation (std), ",
      "minimum, maximum and median of each variable over all the bins shown in the plot above, after its filters. ",
      "For example, mean-asleep is the average fraction of time asleep."
    )
  )
}

help_plots <- function() {
  tagList(
    tags$p(
      "The Sleep, Bout and Period tabs use esquisse: drag variables to the X, Y, fill, colour or facet boxes to build the plot. ",
      "The panel below the plot lets you change the labels, the appearance, filter the data and see the R code of the plot."
    ),
    tags$p(
      "Filters matter: the Period analysis uses the data of the Sleep analysis tab ", tags$i("after"), " its filters, ",
      "and the summaries use the data of the plot above them."
    ),
    tags$p(tags$b("Premade"), " shows the sleep and SD interactions of every fly, without any configuration."),
    tags$h4("Snapshot viewer"),
    tags$p(
      "During the experiment the ethoscope saves a picture of the arena every few minutes. The snapshot viewer shows ",
      "them for each recording of the loaded flies, to check the tracking, the tubes or the sleep deprivation."
    ),
    help_table(list(
      list("Recording", "One of the .db files of the loaded dataset. The table shows how many snapshots it has and when they were taken."),
      list("Snapshots to extract", "Long recordings are grouped in 10 minute blocks. All snapshots selects everything; SD only selects the snapshots taken during the SD (with one block before and after)."),
      list("Extract and show", "Extracts the selected snapshots with their date and time, and shows them. Use the slider (or its play button) to go through them. Snapshots already extracted are reused."),
      list("Make .mp4 video", "Downloads a video of the selected snapshots at the chosen frames per second.")
    )),
    tags$h4("Raw tab"),
    tags$p("Shows the position of one fly in the tube (x along the tube, y across), at full resolution, next to its sleep and the SD interactions. Use it to check whether the scoring makes sense."),
    help_table(list(
      list("Fly", "The fly to show."),
      list("Time range", "Hours since ZT0 to load. Loading days of raw data takes long: start with a short range."),
      list("Keep 1 point every", "Show 1 of every n positions, to keep the plot fast."),
      list("SD only", "Show only the data recorded while the SD was running. Untick it for experiments without SD.")
    ))
  )
}

help_period <- function() {
  tagList(
    tags$p("The Period analysis tab looks for daily (circadian) rhythms in the data shown in the Sleep analysis tab."),
    tags$h4("Periodogram"),
    tags$p("For each fly, how strongly the signal repeats with each period. A fly with a 24 h rhythm has a peak at 24 h."),
    help_table(list(
      list("Signal", "Variable to analyse, usually asleep."),
      list("Algorithm", "Chi-square (Sokolove-Bushell, the most common), autocorrelation or Lomb-Scargle (tolerates missing data)."),
      list("Period range", "Range of periods (hours) to test."),
      list("Significance level (alpha)", "A period is significant when its power is above the threshold for this alpha. signif_power is power minus that threshold: above 0 means significant. The highest peaks are marked in the peak column.")
    )),
    tags$h4("Spectrogram"),
    tags$p(
      "A continuous wavelet transform: the power of each period (y axis) over time (x axis). ",
      "It shows whether the rhythm changes during the experiment, e.g. after a treatment."
    )
  )
}

help_other <- function() {
  tagList(
    tags$h4("Download datasets"),
    help_table(list(
      list("Ethoscope .db files", "The raw recordings of the loaded flies, in a .zip."),
      list("Sleep (scored)", "One row per fly and window: moving, asleep, sd_on..."),
      list("Sleep (binned) / Sleep bouts", "The data of the Sleep / Bout analysis plots, with their filters."),
      list("Summaries", "One row per fly, as in the plots at the bottom of the Sleep and Bout tabs."),
      list("Raw data", "The raw data of the fly last plotted in the Raw tab."),
      list("Periodogram / Spectrogram", "The results of the Period analysis tab.")
    )),
    tags$p("Every file includes the metadata columns, so it can be analysed on its own."),
    tags$h4("Sessions"),
    tags$p(
      "The Sessions menu (top bar) saves the loaded dataset on the server under the name you type, and opens ",
      "a saved one, which is much faster than loading from the database again. Sessions are visible to every user. ",
      "The reload button (top right) loads the current metadata again."
    ),
    tags$h4("Other tabs"),
    tags$p(tags$b("Revise metadata"), " shows the metadata of the loaded flies. ",
           tags$b("Reproducibility script"), " gives an R script that repeats the analysis outside the app.")
  )
}

helpPageUI <- function(id) {
  navlistPanel(
    widths = c(2, 10), well = FALSE,
    tabPanel("How the app works", help_workflow()),
    tabPanel("Metadata", help_metadata()),
    tabPanel("Scoring parameters", help_scoring()),
    tabPanel("Binning and bouts", help_binning()),
    tabPanel("Plots and raw data", help_plots()),
    tabPanel("Period analysis", help_period()),
    tabPanel("Downloads and sessions", help_other())
  )
}

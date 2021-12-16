# DATA LOADER

# functions ---------------------------------------------------------------

# loads paths of all files in folders structured as original
load_paths <- function(in_dir = "./data/raw", groups = c("CLIENT", "PARTNER"), complete = FALSE) {
  # in_dir: directory with raw data
  # groups: names of groups
  # re: regular expressions for ids of groups

  client_files <- list.files(path = file.path(in_dir, groups[1]), pattern = SID_REGEX, full.names = TRUE)
  partner_files <- list.files(path = file.path(in_dir, groups[2]), pattern = SID_REGEX, full.names = TRUE)

  cid_list <- data.frame(
    id = as.numeric(substring(stringr::str_extract(client_files, SID_REGEX), 2)),
    CLIENT = client_files
  )
  pid_list <- data.frame(
    id = as.numeric(substring(stringr::str_extract(partner_files, SID_REGEX), 2)),
    PARTNER = partner_files
  )

  wide_dinfo <- dplyr::full_join(cid_list, pid_list, by = "id")
  if (complete) {
    # list of id and complete client + partner data
    dinfo <- dplyr::full_join(cid_list, pid_list, by = "id")
    cat("Returning data filtered by availability of both, client and partner.")
    dinfo <- dplyr::filter(dinfo, !is.na(cfile) & !is.na(pfile))
  }

  dinfo <- wide_dinfo |>
    tidyr::pivot_longer(cols = c("CLIENT", "PARTNER"), names_to = "group", values_to = "filepath") |>
    dplyr::mutate(sid = paste0(substring(.data$group, 1, 1), .data$id))


  return(dinfo)
}

# parses file to extract statistics
parse_statistics <- function(f) {
  # read group and id
  sid <- stringr::str_extract(f, SID_REGEX)
  group <- substring(sid, first = 1, last = 1)
  id <- as.numeric(substring(sid, first = 2))

  # check existence
  if (!(file.exists(f))) {
    message(sprintf("parse_statistics:\n\tFile of %s does not exist. Parsing skipped...", sid))
    return(NULL)
  }

  # valid colnames
  stats_vars <- c(
    "interval_type", "interval", "start_date", "start_time", "end_date",
    "end_time", "duration", "off-wrist", "perc_off-wrist", "perc_invalid_sw",
    "onset_latency", "efficiency", "waso", "wake_time", "perc_wake",
    "sleep_time", "perc_sleep", "immobile_time", "exposure_white", "avg_white",
    "max_white", "talt_white", "perc_invalid_white", "exposure_red", "avg_red",
    "max_red", "talt_red", "perc_invalid_red", "exposure_green", "avg_green",
    "max_green", "talt_green", "perc_invalid_green", "exposure_blue",
    "avg_blue", "max_blue", "talt_blue", "perc_invalid_blue"
  )


  # read csv
  maxcol <- max(unlist(lapply(strsplit(readLines(f), ","), length)))
  cdata <- read.csv(f, header = FALSE, fill = TRUE, col.names = paste0("V", 1:maxcol))

  # split by excel headers "--- str ---"
  headeridx <- grep("(\\-+)(\\s+(.*))(\\-+)", cdata[, 1])
  lcdata <- split(cdata, cumsum(1:nrow(cdata) %in% c(headeridx, length(cdata))))

  # cleanup statistics, table #5 (check existing features, filter rows and columns, rename, reformat)
  names_idx <- which((lcdata[[5]])[, 1] == "Interval Type")
  statistics <- lcdata[[5]] |>
    dplyr::slice(-c(1:names_idx, names_idx + 1))
  colnames(statistics) <- sub("#", "", sub("%", "perc_", sub(" ", "_", tolower((lcdata[[5]])[names_idx, ]))))

  if (!(all(stats_vars %in% colnames(statistics)))) {
    message("read_file:\n\tMissing features in statistics table. Returning NULL")
    return(NULL)
  } else {
    statistics <- statistics |>
      dplyr::select(all_of(stats_vars)) |>
      dplyr::mutate(
        sid = sid,
        id = id,
        group = group,
        interval_type = as.character(.data$interval_type)
      ) |>
      dplyr::mutate_at(dplyr::vars(3, 5), ~ as.Date(., "%d/%m/%Y")) |>
      dplyr::mutate_at(dplyr::vars(4, 6), ~ format(strptime(., "%I:%M:%S %p"), "%H:%M:%S")) |>
      dplyr::mutate_at(
        dplyr::vars(7:length(stats_vars)),
        ~ as.numeric(.)
      )
    return(statistics)
  }
}

# parses file to extract epochs
parse_epochs <- function(f) {
  # read group and id
  sid <- stringr::str_extract(f, SID_REGEX)
  group <- substring(sid, first = 1, last = 1)
  id <- as.numeric(substring(sid, first = 2))

  # check existence
  if (!(file.exists(f))) {
    message(sprintf("parse_epochs:\n\tFile of %s does not exist. Parsing skipped...", sid))
    return(NULL)
  }

  # valid colnames
  epoch_vars <- c(
    "line", "date", "time", "interval_status", "off-wrist_status", "activity",
    "marker", "white_light", "red_light", "green_light", "blue_light", "sleep_wake"
    )

  # read csv
  maxcol <- max(unlist(lapply(strsplit(readLines(f), ","), length)))
  cdata <- read.csv(f, header = FALSE, fill = TRUE, col.names = paste0("V", 1:maxcol))

  # split by excel headers "--- str ---"
  headeridx <- grep("(\\-+)(\\s+(.*))(\\-+)", cdata[, 1])
  lcdata <- split(cdata, cumsum(1:nrow(cdata) %in% c(headeridx, length(cdata))))

  # cleanup epochs, table #7 (check existing features, filter rows and columns, rename, reformat)
  names_idx <- which((lcdata[[7]])[, 1] == "Line")
  epochs <- lcdata[[7]] |>
    dplyr::slice(-c(1:names_idx)) |>
    dplyr::select(1:12)
  colnames(epochs) <- sub("/", "_", sub(" ", "_", (tolower(lcdata[[7]][names_idx, 1:12]))))
  if (!(all(epoch_vars %in% colnames(epochs)))) {
    message("read_file:\n\tMissing features in epoch-by-epoch table. Returning NULL.")
    epochs <- NULL
  } else {
    epochs <- epochs |>
      dplyr::mutate(
        sid = sid,
        id = id,
        group = group,
        line = as.numeric(epochs$line),
        date = as.Date(epochs$date, "%d/%m/%Y"),
        time = format(strptime(epochs$time, "%I:%M:%S %p"), "%H:%M:%S"),
        interval_status = as.character(epochs$interval_status)
      ) |>
      dplyr::mutate_at(
        dplyr::vars(4:11),
        ~ as.numeric(.)
      ) |>
      dplyr::mutate(
        date_time = lubridate::ymd_hms(paste(.data$date, .data$time), tz = "UTC"), # info from file
        time = hms::as_hms(.data$time),
        date = lubridate::ymd(.data$date)
      )
  }
  return(epochs)
}

# processing stats
process_stats <- function(dinfo, return_data = FALSE) {
  all_stats <- list()
  for (i in seq_len(nrow(dinfo))) {
    # new filepath
    new_dir <- sub("raw", "statistics", dirname(dinfo$filepath[i]))
    group_prefix <- ifelse(dinfo$group[i] == "CLIENT", "C", "P")
    basename <- paste0(group_prefix, dinfo$id[i], "_stats", ".rds")
    new_filepath <- file.path(new_dir, basename)

    # parsing
    cat(sprintf("Reading summary statistics of dyad #%s, ID %s.\n", i, dinfo$sid[i]))
    aggstats <- rds_cached(
      filename = new_filepath,
      fun = parse_statistics,
      f = dinfo$filepath[i]
    )
    if (return_data & all(!is.null(aggstats))) {
      all_stats[[dinfo$sid[i]]] <- aggstats
    }
  }
  all_stats <- do.call(rbind, all_stats)
  return(all_stats)
}

# processing epochs
process_epochs <- function(dinfo, return_data = FALSE) {
  all_epochs <- list()
  for (i in seq_len(nrow(dinfo))) {
    # new filepath
    new_dir <- sub("raw", "epochs", dirname(dinfo$filepath[i]))
    group_prefix <- ifelse(dinfo$group[i] == "CLIENT", "C", "P")
    basename <- paste0(group_prefix, dinfo$id[i], "_epochs", ".rds")
    new_filepath <- file.path(new_dir, basename)

    # parse client epochs
    cat(sprintf("Reading epochs of dyad #%s, ID %s.\n", i, dinfo$sid[i]))
    epochs <- rds_cached(
      filename = new_filepath,
      fun = parse_epochs,
      f = dinfo$filepath[i]
    )
    if (return_data & all(!is.null(epochs))) {
      all_epochs[[dinfo$sid[i]]] <- epochs
    }
  }
  all_epochs <- do.call(rbind, all_epochs)
  return(all_epochs)
}

# Loads epochs from original file
load_data <- function(select_id = FALSE, return_data = FALSE){
  # Cave: if files have never been parsed and saved before, this may take some
  #   minutes depending on the number of select_id s.
  #
  # select_id: can be FALSE or subject id, e.g. C1045, or vector of subject ids, e.g. c("C1045", "P1045")

  dinfo <- load_paths(in_dir = "./data/raw")

  if (select_id != FALSE){
    dinfo <- dinfo[(dinfo$sid %in% select_id),]
  }

  all_epochs <- process_epochs(dinfo, return_data)

  return(all_epochs)
}

# Loads statistics from original file
load_stats <- function(select_id = FALSE, return_data = FALSE){
  # Cave: if files have never been parsed and saved before, this may take some
  #   minutes depending on the number of select_id s.
  #
  # select_id: can be FALSE or subject id, e.g. C1045, or vector of subject ids, e.g. c("C1045", "P1045")

  dinfo <- load_paths(in_dir = "./data/raw")

  if (select_id != FALSE){
    dinfo <- dinfo[(dinfo$sid %in% select_id),]
  }

  all_stats <- process_stats(dinfo, return_data)

  return(all_stats)
}

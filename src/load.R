# DATA LOADER

# constants ---------------------------------------------------------------

groups <- c("CLIENT", "PARTNER")
data_dir <- "./data/raw"
re <- c("(C\\d{4})", "(P\\d{4})")


# functions ---------------------------------------------------------------

# loads paths of all files in folders structured as original
load_paths <- function(data_dir = "./data/raw", groups = c("CLIENT", "PARTNER"), re = c("(C\\d{4})", "(P\\d{4})"), complete=FALSE) {
  # data_dir: directory with raw data
  # groups: names of groups
  # re: regular expressions for ids of groups

  client_files <- list.files(path = file.path(data_dir, groups[1]), pattern = re[1], full.names = TRUE)
  partner_files <- list.files(path = file.path(data_dir, groups[2]), pattern = re[2], full.names = TRUE)

  cid_list <- data.frame(
    id = as.numeric(substring(stringr::str_extract(client_files, re[1]), 2)),
    cfile = client_files
  )
  pid_list <- data.frame(
    id = as.numeric(substring(stringr::str_extract(partner_files, re[2]), 2)),
    pfile = partner_files
  )

  # list of id and complete client + partner data
  dinfo <- dplyr::full_join(cid_list, pid_list, by = "id")
  if (complete) {
    # filter
    cat("Returning data filtered by availability of both, client and partner.")
    dinfo <- dplyr::filter(dinfo, !is.na(cfile) & !is.na(pfile))
  }
  return(dinfo)
}

# parses file to extract statistics
parse_statistics <- function(f) {
  if (!(file.exists(f))){
    warning("File does not exist.")
    return(NULL)
  }
  stats_vars <- c("interval_type", "interval", "start_date", "start_time", "end_date", "end_time", "duration", "off-wrist", "perc_off-wrist", "perc_invalid_sw", "onset_latency", "efficiency", "waso", "wake_time", "perc_wake", "sleep_time", "perc_sleep", "immobile_time", "exposure_white", "avg_white", "max_white", "talt_white", "perc_invalid_white", "exposure_red", "avg_red", "max_red", "talt_red", "perc_invalid_red", "exposure_green", "avg_green", "max_green", "talt_green", "perc_invalid_green", "exposure_blue", "avg_blue", "max_blue", "talt_blue", "perc_invalid_blue")
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
  if (!(all(stats_vars %in% colnames(statistics)))){
    message("read_file:\n\tMissing features in statistics table. Returning NULL")
    return(NULL)
  }else{

    statistics <- statistics |>
      dplyr::select(all_of(stats_vars)) |>
      dplyr::mutate("interval_type" = as.character(.data$interval_type)) |>
      dplyr::mutate_at(dplyr::vars(3, 5), ~ as.Date(., "%d/%m/%Y")) |>
      dplyr::mutate_at(dplyr::vars(4, 6), ~ format(strptime(., "%I:%M:%S %p"), "%H:%M:%S")) |>
      dplyr::mutate_at(dplyr::vars(7:length(stats_vars)),
                       ~ as.numeric(.))
    return(statistics)
  }
}

# parses file to extract epochs
parse_epochs <- function(f) {
  if (!(file.exists(f))){
    warning("File does not exist.")
    return(NULL)
  }
  epoch_vars <- c("line","date","time", "interval_status","off-wrist_status", "activity", "marker", "white_light","red_light", "green_light", "blue_light", "sleep_wake")
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
      dplyr::mutate("line" = as.numeric(epochs$line)) |>
      dplyr::mutate("date" = as.Date(epochs$date, "%d/%m/%Y")) |>
      dplyr::mutate("time" = format(strptime(epochs$time, "%I:%M:%S %p"), "%H:%M:%S")) |>
      # time zone irrelevant
      dplyr::mutate("interval_status" = as.factor(epochs$interval_status)) |>
      dplyr::mutate_at(
        dplyr::vars(4:11),
        ~ as.numeric(.)
      )
    epochs <- epochs |>
      dplyr::mutate(
        date_time = lubridate::ymd_hms(paste(.data$date, .data$time), tz = "UTC"),
        time = as.numeric(lubridate::hms(.data$time)),
        date = lubridate::ymd(.data$date)
      )
  }
  return(epochs)
}

# loading
load_statistics <- function(data_dir) {
  # load id and both paths
  dinfo <- load_paths(data_dir, groups, re)

  # loop over files
  all_statistics <- list()
  for (i in seq_len(nrow(dinfo))) {
    cat(sprintf("Reading statistics of dyad #%s, ID %s.\n", i, dinfo$id[i]))
    # parse client
    new_basename = gsub(".csv$", ".rds", basename(dinfo$cfile[i]))
    cstats <- rds_cached(
      filename=sub("/raw/", "/transformed/statistics/", file.path(data_dir, new_basename), fixed = TRUE),
      fun=parse_statistics,
      f=dinfo$cfile[i]
    )
    if(!is.null(cstats)){
      cstats <- dplyr::mutate(cstats, group = "C")
    }
    # parse partner epochs
    new_basename = gsub(".csv$", ".rds", basename(dinfo$pfile[i]))
    pstats <- rds_cached(
      filename=sub("/raw/", "/transformed/statistics/", file.path(data_dir, new_basename), fixed = TRUE),
      fun=parse_statistics,
      f=dinfo$pfile[i]
    )
    if(all(!is.null(pstats))){
      pstats <- dplyr::mutate(pstats, group = "P")
    }
    # join in one table
    both_stats <- rbind(cstats, pstats) |> dplyr::mutate(id = dinfo$id[i])
    # collect dyads in list
    all_statistics[[paste0("ID", dinfo$id[i])]] <- both_stats
  }
}

load_epochs <- function(data_dir) {
  # load id and both paths
  dinfo <- load_paths(data_dir, groups, re)

  # loop over files
  all_epochs <- list()
  for (i in seq_len(nrow(dinfo))) {
    cat(sprintf("Reading epochs of dyad #%s, ID %s.\n", i, dinfo$id[i]))
    # parse client
    new_basename = gsub(".csv$", ".rds", basename(dinfo$cfile[i]))
    cepochs <- rds_cached(
      filename=sub("/raw/", "/transformed/epochs/", file.path(data_dir, new_basename), fixed = TRUE),
      fun=parse_epochs,
      f=dinfo$cfile[i]
      )

    if(all(!is.null(cepochs))){
       cepochs <- dplyr::mutate(cepochs, group = "C")
    }
    # parse partner epochs
    new_basename = gsub(".csv$", ".rds", basename(dinfo$pfile[i]))
    pepochs <- rds_cached(
      filename=sub("/raw/", "/transformed/epochs/", file.path(data_dir, new_basename), fixed = TRUE),
      fun=parse_epochs,
      f=dinfo$pfile[i]
      )
    if(!is.null(nrow(pepochs))){
      pepochs <- dplyr::mutate(pepochs, group = "P")
    }
    # join in one table
    both_epochs <- rbind(cepochs, pepochs) |> dplyr::mutate(id = dinfo$id[i])
    # collect dyad in list
    all_epochs[[paste0("ID", dinfo$id[i])]] <- both_epochs
  }
  # TODO prune non-matching couples
}


# Execution ---------------------------------------------------------------

#load_epochs(data_dir)
#load_statistics(data_dir)

# Tests -------------------------------------------------------------------

# TODO maybe dynamic?
test_cols_exist <- function() {
  TRUE
}

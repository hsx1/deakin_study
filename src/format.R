# FORMAT

# Bin time series RELATIVE to anker
bin_series_at_anker <- function(data, target_cols, rel_col, bin_size){
  # Computes average for target variables and bin interval (in steps of anker unit)
  #
  # target_cols: vector, columns to summarize for bins
  # rel_col: numeric, relative column, with 0 where individuals time series needs is centered; recommended that intervals code for minutes
  # bin_size: numeric, e.g. is minutes if anker is oriented at time

  data$bin <- data[[rel_col]] %/% bin_size #floor(data[[rel_col]] / 2)

  # drop invalid bins, e.g. at edges
  invalid <- data |>
    dplyr::count(bin = .data$bin) |>
    dplyr::filter(.data$n < bin_size)
  invalid_bins <- intersect(data$bin, invalid$bin) # maximum 2 if not aligned

  # add average bin time
  offset <- (bin_size - 1)/2
  data$bin <- data$bin + offset

  # filter bins that are too small
  if(length(invalid_bins) > 0){
    tmp <- data[!(data$bin %in% invalid_bins),]
  }else{
    tmp <- data
  }

  # summarize bins -> mean relative time, and mean values
  binned_data <- tmp |>
    dplyr::group_by(.data$bin) |>
    dplyr::summarize_at(dplyr::all_of(c(rel_col, target_cols)), mean, na.rm=T)
  return(binned_data)
}

# Bin time series ABSOLUTE with date_time column
bin_series_at_datetime <- function(data, target_cols, minute_interval = 30){
  # Computed average for target variables and bin interval [in minutes]
  #
  # data: dataset
  # minute_interval: interval size in minutes, e.g. 5, 30 or 60

  # does not delete NA while averaging
  min_steps <- minute_interval
  data$bin <- lubridate::as_datetime(cut(data$date_time, breaks=paste(minute_interval, "mins")))

  # drop invalid bins, e.g. at edges
  invalid <- data |>
    dplyr::count(bin = as.factor(.data$bin)) |>
    dplyr::mutate(bin = lubridate::as_datetime(.data$bin)) |>
    dplyr::filter(.data$n < min_steps)
  #print(invalid)
  invalid_bins <- intersect(data$bin, invalid$bin)

  # add average bin time
  offset <- hms::as_hms(((minute_interval - 1)/2)*60)
  data$mean_bin_time <- data$bin + offset

  if(length(invalid_bins) > 0){
    tmp <- data[!(data$bin %in% invalid_bins),]
  }else{
    tmp <- data
  }
  tmp$date_time <- NULL
  colnames(tmp)[colnames(tmp) == "mean_bin_time"] <- "date_time"

  binned_data <- tmp |>
    dplyr::group_by(.data$date_time) |>
    dplyr::summarize_at(dplyr::all_of(c(target_cols)), mean, na.rm=T)

  # number of valid values per bin
  # tmp |>
  #   dplyr::group_by(.data$bin) |>
  #   dplyr::summarize_at(binned_2, dplyr::all_of(target_cols), ~ sum(!is.na(.)))

  return(binned_data)
}

# Construct moving average at bin size [UNIFINISHED]
moving_time_average <- function(data, target, min_interval = 30){
  size = min_interval
  data$new_var <- stats::filter(data[[target]], filter = rep(1 / size, size), sides = 2)
  colnames(data[,"new_var"]) <- paste0(target, "_mov")
}


# Get autocorrelations of light and activity
get_autocor <- function(data, max_lag = Inf){
  # data: dataset
  # max_lag: maximum lag in correlation
  # bin_size = (data$date_time[2] - data$date_time[1]); day_span = 2
  # maximum_lag = (60/bin_size) * 24 * day_span

  # resource (https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html#correlogram-by-ggplot2)
  ac <- list()
  maximum_lag <- max_lag
  for (light in c("activity", light_cols)) {
    light_acf <- stats::acf(
      plot = FALSE,
      x = data[[light]],
      type = "correlation",
      na.action = na.pass,
      lag.max = maximum_lag
    )
    light_pacf <- stats::acf(
      plot = FALSE,
      x = data[[light]],
      type = "partial",
      na.action = na.pass,
      lag.max = maximum_lag
    )

    # collect (note, first element of pacf not existent, thus NA)
    ac[[light]] <- data.frame(
      lag = light_acf$lag,
      acf = light_acf$acf,
      pacf = c(NA, light_pacf$acf)
    )

    # set NA correlations to 0
    ac[[light]]$acf  <- ifelse(is.na(ac[[light]]$acf), 0, ac[[light]]$acf)
    ac[[light]]$pacf <- ifelse(is.na(ac[[light]]$pacf), 0, ac[[light]]$pacf)#
    # group and participant id
    ac[[light]]$id <- data$id
    ac[[light]]$group <- data$group
  }
  return(ac)
}

# Get cross correlation of activity with light
get_crosscor <- function(data, max_lag = Inf){
  # data: dataset
  # max_lag: maximum lag in correlation
  #   bin_size = (data$date_time[2] - data$date_time[1]); day_span = 2
  #   maximum_lag = (60/bin_size) * 24 * day_span

  cc <- list()
  maximum_lag <- Inf
  for (light in light_cols) {
    cc[[light]] <- stats::ccf(
      plot = FALSE,
      data$activity,
      data[[light]],
      type = "correlation",
      na.action = na.pass,
      lag.max = maximum_lag
    )
    cc[[light]] <- data.frame(
      lag = cc[[light]]$lag,
      ccf = cc[[light]]$acf
    )
  }
  return(cc)
}

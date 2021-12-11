# FORMAT

# Bin time series RELATIVE to anker
bin_series_at_anker <- function(anker, bin_size = 30){
  # computes average for target variables and bin interval (in steps of anker unit)
  #
  # anker: numeric, with 0 where individuals time series needs is centered
  # bin_size: numeric, e.g. is minutes if anker is oriented at time

}

# Bin time series ABSOLUTE with date_time column
bin_series_at_datetime <- function(data, target_cols, minute_interval = 30){
  # computed average for target variables and bin interval [in minutes]
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
  offset <- minutes((minute_interval - 1)/2)
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
    dplyr::summarize_at(dplyr::all_of(c(target_cols)), mean, na.rm=F)

  # number of valid values per bin
  # tmp |>
  #   dplyr::group_by(.data$bin) |>
  #   dplyr::summarize_at(binned_2, dplyr::all_of(target_cols), ~ sum(!is.na(.)))

  return(binned_data)
}

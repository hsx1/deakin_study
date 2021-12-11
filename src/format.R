# FORMAT

bin_series_at_anker <- function(){}


# computed average bin time and bin value for target variable and interval [in minutes]
bin_series_at_datetime <- function(data, minute_interval = 30){
  target_cols = c("activity", "white_light", "red_light", "blue_light", "green_light")

  # does not delete NA while averaging
  min_steps <- minute_interval
  data$bin <- lubridate::as_datetime(cut(data$date_time, breaks=paste(minute_interval, "mins")))

  # drop invalid bins, e.g. at edges
  invalid <- data |>
    dplyr::count(as.factor(.data$bin)) |>
    dplyr::filter(.data$n < min_steps)
  #print(invalid)
  invalid_bins <- intersect(data$bin, invalid$bin)
  if(length(invalid_bins) > 0){
    tmp <- data[!(data$bin %in% invalid_bins),]
  }else{
    tmp <- data
  }

  data$new_var <- NA
  bin_time <- paste0("bin", minute_interval)
  colnames(data)[colnames(data) == "new_var"] <- bin_time

  # add average bin time
  offset <- minutes((minute_interval - 1)/2)
  data$mean_bin_time <- data$bin + offset

  binned_data <- tmp |>
    dplyr::group_by(.data$bin) |>
    dplyr::summarize_at(dplyr::all_of(target_cols), mean, na.rm=F)

  # number of valid values per bin
  # tmp |>
  #   dplyr::group_by(.data$bin) |>
  #   dplyr::summarize_at(binned_2, dplyr::all_of(target_cols), ~ sum(!is.na(.)))

  return(binned_data)
}

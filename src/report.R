# report


# Functions ---------------------------------------------------------------

extract_na_periods <- function(data, target) {
  # Extracts infos on start and stop time for periods with NA
  #
  # data: e.g. cdata or pdata, data frame
  # target: str, column of interest
  #
  # return: data frame with infos of NA

  # extract valid date_times and diff to previous date time vector
  valids <- data$date_time[!is.na(data[[target]])]
  # where there is x = diff > 1, there are NAs in between x-1 and x
  diff_to_prev <- c(0, difftime(valids[2:length(valids)], valids[1:length(valids) - 1]))
  t_after_na <- valids[diff_to_prev > 1]
  # jump_in_valid_t <- which(diff_to_prev > 1)

  na_periods <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(na_periods) <- c("first_na", "last_na", "first_idx", "last_idx", "duration")
  for (t in t_after_na) {
    first_na <- (valids[which(valids == t) - 1] + minutes(1))
    last_na <- (valids[which(valids == t) - minutes(1)])
    # cat(paste0("Unavailable measurements for ", target, " at t = [", first_na, "; ", last_na, "]\n"))
    first_idx <- which(data$date_time == first_na)
    last_idx <- which(data$date_time == last_na)
    dur <- (last_na + minutes(1)) - first_na
    na_periods <- rbind(na_periods, data.frame(first_na = first_na, last_na = last_na, first_idx = first_idx, last_idx = last_idx, duration = dur))
    # check
    # before_start_idx <- start_idx - 1
    # after_stop_idx <- ifelse((stop_idx+1) > nrow(data), nrow(data), (stop_idx+1))
    # print(c(!is.na(data[[target]][before_start_idx]), !is.na(data[[target]][after_stop_idx])))
  }
  return(na_periods)
}
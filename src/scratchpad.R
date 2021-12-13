# SCRATCHPAD
source("./src/load.R")

# check if file
a <- parse_file("./data/raw/manuel/C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")$epochs
b <- parse_file("./data/raw/CLIENT/C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis.csv")$epochs
all(a[!is.na(a)] == b[!is.na(a)])


extract_na_periods <- function(data, target) {
  # Extracts infos on start and stop time for periods with NA
  #
  # data: e.g. cdata or pdata, data frame
  # target: str, column of interest
  #
  # return: data frame with infos of NA

  # extract valid date_times and diff to previous date time vector
  valids <- data$date_time[!is.na(data[[target]])]
  # duplicate first time point for 1) diff_to_prev has same length, 2) in case series stats with NA
  valids <- c(data$date_time[1], valids)
  # where there is x = diff > 1, there are NAs in between x-1 and x
  diff_to_prev <- c(difftime(valids[2:length(valids)], valids[1:length(valids) - 1]))
  t_after_na <- valids[diff_to_prev > 1]
  # jump_in_valid_t <- which(diff_to_prev > 1)

  na_periods <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(na_periods) <- c("first_na", "last_na", "first_idx", "last_idx", "duration")
  for (t in t_after_na) {
    first_na <- (valids[which(valids == t)] + minutes(1))
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


# misc --------------------------------------------------------------------
x <- 1479974700
x1 <- as_datetime(as.numeric(x))
x2 <- as_datetime("2016-11-24 08:05:00 UTC")
cdata$time <- lubridate::hms(cdata$time)
cdata$time <- lubridate::date(cdata$date)
lubridate::parse_time(cdata$time)


# REPORT

# Functions ---------------------------------------------------------------

# Extracts periods of TRUE for boolean target variables similar to "Statistics" overview
periods_of_target <- function(data, target){
  # data: data frame
  # target: bool, e.g. target = "invalid",

  # always include the first row (preliminary)
  data$previous_different <- TRUE
  for (i in 2:nrow(data)){
    # next value i + 1 same as value before i
    data$previous_different[i] = (data[i, target] != data[i-1, target])
  }

  # extract blocks starting with first block (whether TRUE or FALSE doesn't matter, because first row is always true)
  transitions <- data[data$previous_different, c("date_time", target)]
  # drop first block, if is doesn't start with target
  if (transitions[1, target] == FALSE) {
    transitions <- transitions[-1,]
  }

  # first transition always "to TRUE", e.g. NA
  transitions_start <- transitions[seq(from=1, to=nrow(transitions), by=2), "date_time"]
  transitions_end <- transitions[seq(from=2, to=nrow(transitions), by=2), "date_time"]
  # for cases without second transition "to FALSE", e.g. ending with NA, add last minute+1
  if (length(transitions_start) != length(transitions_end)) {
    transitions_end <- c(transitions_end, data$date_time[nrow(data)]+lubridate::minutes(1))
  }

  overview <- data.frame(
    target = target,
    start_time = transitions_start,
    end_time = transitions_end,
    duration = transitions_end - transitions_start
  )
  return(overview)
}

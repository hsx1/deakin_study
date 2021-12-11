# Main analysis script

# packages
# TODO install if not available
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

source("./src/utils.R")

#main <- function(){

# Load data ---------------------------------------------------------------
# 1.	What is the data format?


# parse files
cdata <- parse_epochs(
  f = file.path(
    data_dir, "single",
    "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
)
pdata <- parse_epochs(
  f = file.path(
    data_dir, "single",
    "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
)
cstats <- parse_statistics(
  f = file.path(
    data_dir, "single",
    "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
)
pstats <- parse_statistics(
  f = file.path(
    data_dir, "single",
    "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
)

ctables <- list(data=cdata, istat=cstats)
ptables <- list(data=cdata, istat=cstats)
# for (tabl in c(ctables, ptables)){...}
data <- tabl$data
istat <- tabl$istat


# Minor formatting --------------------------------------------------------

# reformat date and time
data <- data |>
  dplyr::mutate(
    date_time = lubridate::ymd_hms(paste(.data$date, .data$time), tz = "UTC"),
    time = as.numeric(lubridate::hms(.data$time)),
    date = lubridate::ymd(date)
    )


# Time series characteristics ---------------------------------------------

# how many different periods?
count_periods <- istat |>
  dplyr::group_by(.data$interval_type) |>
  dplyr::summarise(n = dplyr::n())
cprint(
  sprintf(
    "Time series spans %.0f days, including %0.f ACTIVE periods, %0.f REST periods and %0.f SLEEP periods.",
    count_periods$n[count_periods$interval_type == "DAILY"],
    count_periods$n[count_periods$interval_type == "ACTIVE"],
    count_periods$n[count_periods$interval_type == "REST"],
    count_periods$n[count_periods$interval_type == "SLEEP"]),
  colour="y"
  )

# how long is the data?
cprint(
  sprintf(
    "Dataset properties: %.0f observations, %.0f complete cases with %s in time (%.2f%%).",
    nrow(data), sum(complete.cases(data)),
    hms::as_hms(sum(!complete.cases(data))*60),
    sum(!complete.cases(data))/nrow(data) * 100),
  col = "y"
)

# regularity of time steps
time_steps_1min <- (difftime(data[2:nrow(data), "date_time"], data[1:(nrow(data)-1), "date_time"]) == 1)
if(all(time_steps_1min)){
  cprint("Data is sampled with 1 Hz.", colour = "g")
}else{
  cprint("Irregular time bins. Data needs to be sampled with 1 Hz. Further operations for binning wil fail.\n", colour = "r")
}



# missing data
data$invalid <- ifelse(complete.cases(data), FALSE, TRUE)
periods_of_target(data, target = "invalid")


# Mean light and activity -------------------------------------------------

# in 1-minute bins, in 30-minute bins, in 60 minute bins
targets <- c("activity", "white_light", "red_light", "blue_light", "green_light")
orig_data <- data
data <- data |>
  bin_series_at_datetime(target_cols = targets, minute_interval = 5) |>
  bin_series_at_datetime(target_cols = targets, minute_interval = 30) |>
  bin_series_at_datetime(target_cols = targets, minute_interval = 60)
# in 1-minute bins

moving_time_average(data, target, min_interval = 30){

  size = min_interval
  data$new_var <- stats::filter(data[[target]], filter = rep(1 / size, size), sides = 2)
  colnames(data[,"new_var"]) <- paste0(target, "_mov")
}



# get sorting
# sort by time
# create vector bin_period for binning
# summarize by bin_groups




# Regularity --------------------------------------------------------------
# 5.	How regular is the activity and light exposure?

# visual inspection
plot_colorlight(data)
plot_whitelight(data)
plot_activity(data)
# autocorrelation with 24-hour lag (or various lags)


# Correlation -------------------------------------------------------------
# 6.	Are light and activity correlated?

# GLM? Smoothing?


# Unit tests --------------------------------------------------------------
# 7.	When you write specific functions to process the data, how can you be sure that the functions are doing the right thing?


# Plots -------------------------------------------------------------------

# light by time separate per day

# colored light
longdata <- data |> tidyr::pivot_longer(cols = c("red_light", "green_light", "blue_light"))
ggplot(data = longdata, aes(x = .data$time, color = name)) +
  geom_line(aes(y = (.data$value))) + # maybe rather log?
  facet_grid(facets = .data$date ~ .) +
  scale_x_time(
    breaks = (0:23)*60*60,
    minor_breaks = (0:23)*60*60,
    limits = c(0, 60*60*24)) +
  xlab("Time [sec]") + ylab("Light [\u00B5W / m\u00B2]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# white light [lux]
ggplot(data = data, aes(x = .data$time)) +
  geom_line(aes(y = .data$white_light)) +
  facet_grid(facets = .data$date ~ .) +
  scale_x_time(
    breaks = (0:23)*60*60,
    minor_breaks = (0:23)*60*60,
    limits = c(0, 60*60*24)) +
  xlab("Time [sec]") + ylab("White Light [lux]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# light by time - filtered by sleep - per day

# light by date_time separate per group

# activity by time
ggplot(data = data, aes(x = .data$time)) +
  geom_line(aes(y = .data$activity)) +
  facet_grid(facets = .data$date ~ .) +
  scale_x_time(
    breaks = (0:23)*60*60,
    minor_breaks = (0:23)*60*60,
    limits = c(0, 60*60*24)) +
  xlab("Time [sec]") + ylab("Acitivity [> 20 counts]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# misc --------------------------------------------------------------------
x <- 1479974700
x1 <- as_datetime(as.numeric(x))
x2 <- as_datetime("2016-11-24 08:05:00 UTC")
cdata$time <- lubridate::hms(cdata$time)
cdata$time <- lubridate::date(cdata$date)
lubridate::parse_time(cdata$time)



# Analysis ----------------------------------------------------------------

# detrending necessary?
# logistic regression of current light and different lags
# compare odds ratio(?) between C and P
res <- glm(cdata$sleep_wake ~ time + 1, data=cdata, family=binomial)


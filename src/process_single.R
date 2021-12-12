# ANALYSIS

# temporary until called by main
source("./src/utils.R")

# analysis <- function() {

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
na_periods <- periods_of_target(data, target = "invalid")
cprint(sprintf("%.0f periods of at least 1 minute have one or more missing oberservations in any variable.", nrow(na_periods)), colour = "y")

# Mean light and activity -------------------------------------------------

# in 1-minute bins, in 30-minute bins, in 60 minute bins
target_cols <- c("activity", "white_light", "red_light", "blue_light", "green_light", "date_time")
orig_data <- data

# construct relative time at anker = experiment begin
data <- dplyr::mutate(data, nrel_date_time = (as.numeric(.data$date_time - .data$date_time[1]) / 60))

# bin time series
data5 <- bin_series_at_anker(data, target_cols, rel_col = "nrel_date_time",
                             bin_size = 5)
data30 <- bin_series_at_anker(data, target_cols, rel_col = "nrel_date_time",
                              bin_size = 30)
data60 <- bin_series_at_anker(data, target_cols, rel_col = "nrel_date_time",
                              bin_size = 60)


moving_time_average(data, target, min_interval = 30){
  size = min_interval
  data$new_var <- stats::filter(data[[target]], filter = rep(1 / size, size), sides = 2)
  colnames(data[,"new_var"]) <- paste0(target, "_mov")
}



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


#}

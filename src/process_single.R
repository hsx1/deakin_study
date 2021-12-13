# ANALYSIS

# temporary until called by main
source("./src/utils.R")

# analysis <- function() {
light_cols <- c("white_light", "red_light", "blue_light", "green_light")
target_cols <- c("date_time", "activity", light_cols)

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
    count_periods$n[count_periods$interval_type == "SLEEP"]
  ),
  colour = "y"
)

# how long is the data?
cprint(
  sprintf(
    "Dataset properties: %.0f observations, %.0f complete cases with %s in time (%.2f%%).",
    nrow(data), sum(complete.cases(data)),
    hms::as_hms(sum(!complete.cases(data)) * 60),
    sum(!complete.cases(data)) / nrow(data) * 100
  ),
  col = "y"
)

# regularity of time steps
time_steps_1min <- (difftime(data[2:nrow(data), "date_time"], data[1:(nrow(data) - 1), "date_time"]) == 1)
if (all(time_steps_1min)) {
  cprint("Data is sampled with 1 Hz.", colour = "g")
} else {
  cprint("Irregular time bins. Data needs to be sampled with 1 Hz. Further operations for binning wil fail.\n", colour = "r")
}

# missing data
data$invalid <- ifelse(complete.cases(data), FALSE, TRUE)
na_periods <- periods_of_target(data, target = "invalid")
cprint(sprintf("%.0f periods of at least 1 minute have missing oberservations in one or more variables.", nrow(na_periods)), colour = "y")


# Mean light and activity -------------------------------------------------

# in 1-minute bins, in 30-minute bins, in 60 minute bins
orig_data <- data

# construct relative time at anker = experiment begin
data <- dplyr::mutate(data, nrel_date_time = (as.numeric(.data$date_time - .data$date_time[1]) / 60))

# bin time series
data5 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 5
  ) |>
  dplyr::mutate(
    time = hms::as_hms(.data$date_time),
    date = lubridate::as_date(.data$date_time)
  )

data30 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 30
  ) |>
  dplyr::mutate(
    time = hms::as_hms(.data$date_time),
    date = lubridate::as_date(.data$date_time)
  )

data60 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 60
  ) |>
  dplyr::mutate(
    time = hms::as_hms(.data$date_time),
    date = lubridate::as_date(.data$date_time)
  )


# Regularity --------------------------------------------------------------

# Regularity activity and light exposure?

cprint("Further analysis will be carried out binned data with bin size = 60 minutes...", colour="y")

# visual inspection of raw data
plot_colorlight(data5, title = "Light Exposure")
plot_whitelight(data5)
plot_activity(data5)

# autocorrelation and partial autocorrelation
ac_list <- get_autocor(data30, max_lag = Inf)

# plot correlograms
bin_size = as.numeric(data30$time[2]-data30$time[1]) / 60
var_names <- names(ac_list)
ac_plot_list <- list(); pac_plot_list <- list()
for (i in seq(ac_list)){
  var_name <- sub("_", " ", var_names[i])
  current_ac <- ac_list[[i]]

  # autocorrelation plot
  ac_plot_list[[var_name]] <- plot_correlogram(
    cordata = current_ac,
    kind = "auto",
    title = sprintf("Autocorrelogram of %s for %.0f-minute bins", var_name, bin_size)
    )

  # partial autocorrelation plot
  pac_plot_list[[var_name]] <- plot_correlogram(
    cordata = current_ac,
    kind = "p-auto",
    title = sprintf("Partial autocorrelogram of %s for %.0f-minute bins", var_name, bin_size)
  )
  p <- cowplot::plot_grid(ac_plot_list[[var_name]], pac_plot_list[[var_name]])
  plot(p)
}

cprint("All variables visually show periodicity of 24 hour.", colour = "y")

# TODO entropy


# Correlation -------------------------------------------------------------

# cross correlations of activity and lights
cc_list <- get_crosscor(data30, max_lag = Inf)

# plot correlograms
cc_plot_list <- list()
for (i in seq(cc_list)){
  var_name <- sub("_", " ", var_names[i])
  current_cc <- cc_list[[i]]

  # autocorrelation plot
  cc_plot_list[[var_name]] <- plot_correlogram(
    cordata = current_cc,
    kind = "cross",
    title = sprintf("Cross-correlogram of %s and activity for %.0f-minute bins", var_name, bin_size)
  )
  plot(cc_plot_list[[var_name]])
}

# cumsum(ifelse(is.na(x), 0, x)) + x*0
# lagged regression?


# GLM? Smoothing?

# Analysis ----------------------------------------------------------------

# https://online.stat.psu.edu/stat510/lesson/8/8.2
# detrending necessary? low-pass filter?
# logistic regression of current light and different lags
# compare odds ratio(?) between C and P
periodity
data_length
sin24h <- sin(2*pi*data_length)/(periodicity)

res <- glm(data$sleep_wake ~ sin24h + data$sleep_wake sw_lag + 1, data = cdata, family = binomial)
# sin*24 + cos*24 (account for daily rythm)
# further betas to explain rest


# }

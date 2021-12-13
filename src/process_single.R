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
cprint(sprintf("%.0f periods of at least 1 minute have one or more missing oberservations in any variable.", nrow(na_periods)), colour = "y")

# Mean light and activity -------------------------------------------------

# in 1-minute bins, in 30-minute bins, in 60 minute bins
light_cols <- c("white_light", "red_light", "blue_light", "green_light")
target_cols <- c("date_time", "activity", light_cols)
orig_data <- data

# construct relative time at anker = experiment begin
data <- dplyr::mutate(data, nrel_date_time = (as.numeric(.data$date_time - .data$date_time[1]) / 60))

# bin time series
data5 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 5
)
data30 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 30
)
data60 <- bin_series_at_anker(data, target_cols,
  rel_col = "nrel_date_time",
  bin_size = 60
)

# Regularity --------------------------------------------------------------
# 5.	How regular is the activity and light exposure?

cprint("Further analysis will be carried out ")

# visual inspection of raw data
plot_colorlight(data)
plot_whitelight(data)
plot_activity(data)

# autocorrelation with 24-hour lag
ac_list <- get_autocor(data30, max_lag = Inf)

# plot, one example
plotlist_correlogram <- plot_correlogram(cordata = ac_list$white_light, kind = "auto")

grid.arrange(
  grobs = gl,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2, NA),
                        c(3, 3, 4))
)

# Correlation -------------------------------------------------------------
# 6.	Are light and activity correlated?

# cross correlations of activity and lights
cc_list <- get_crosscor(data30, max_lag = Inf)

# plot, one example
plot_correlogram(cordata = cc_list$white_light, kind = "cross")


# cumsum(ifelse(is.na(x), 0, x)) + x*0
# lagged regression?


# GLM? Smoothing?


# Unit tests --------------------------------------------------------------
# 7.	When you write specific functions to process the data, how can you be sure that the functions are doing the right thing?



# Analysis ----------------------------------------------------------------

# https://online.stat.psu.edu/stat510/lesson/8/8.2
# detrending necessary?
# logistic regression of current light and different lags
# compare odds ratio(?) between C and P
res <- glm(cdata$sleep_wake ~ time + 1, data = cdata, family = binomial)


# }

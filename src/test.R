# UNIT TESTS

library(testthat)

# source everything
source("./src/utils.R")

# TODO test loader and parser

# periods_of_target()
test_periods_of_target <- function(){
  start_time <- as.POSIXct("2013-06-17 14:20:00 UTC")
  steps <- 4
  end_time <- start + as.difftime(steps, units = "mins")
  data <- data.frame(
    line = 1:5,
    date_time = seq(start_time, end_time, by = "mins"),
    value = c(0.5, 0.8, NA, NA, 7.5)
  ) |>
    dplyr::mutate(
      invalid = is.na(.data$value)
    )
  count_na_blocks <- nrow(periods_of_target(data, target = "invalid"))
  testthat::expect_equal(count_na_blocks, 1)

  data <- data.frame(
    line = 1:5,
    date_time = seq(start_time, end_time, by = "mins"),
    value = c(NA, NA, 0.5, 0.8, NA)
  ) |>
    dplyr::mutate(
      invalid = is.na(.data$value)
    )
  count_na_blocks <- nrow(periods_of_target(data, target = "invalid"))
  testthat::expect_equal(count_na_blocks, 2)

  data <- data.frame(
    line = 1:5,
    date_time = seq(start_time, end_time, by = "mins"),
    value = c(NA, 0.5, NA, 0.8, NA)
  ) |>
    dplyr::mutate(
      invalid = is.na(.data$value)
    )
  count_na_blocks <- nrow(periods_of_target(data, target = "invalid"))
  testthat::expect_equal(count_na_blocks, 3)
}

# bin_series_at_datetime()
test_bin_series_at_datetime <- function(){
  # data must be sampled in 1 Hz
  start_time <- as.POSIXct("2013-06-17 14:20:00 UTC")
  steps <- 10
  end_time <- start_time + as.difftime(steps-1, units = "mins")
  filter_out <- 3
  data <- data.frame(
    line = 1:steps,
    small_value = rnorm(steps),
    value = round(rnorm(steps)*10),
    date_time = seq(start_time, end_time, by = "mins")
    ) |>
    dplyr::filter(!(.data$line %in% filter_out))

  new_data <- bin_series_at_datetime(data, target_cols = "value", minute_interval = 3)
  testthat::expect_equal(nrow(new_data), 2)

  new_data <- bin_series_at_datetime(data, target_cols = c("small_value", "value"), minute_interval = 3)
  testthat::expect_equal(ncol(new_data), 3)
}

cprint("All tests passed.", col = "g")

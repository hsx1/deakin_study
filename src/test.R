# UNIT TESTS

library("testthat")

# source everything
source("./src/utils.R")

# TODO test loader and parser

# test single file equals downloaded file
compare_files_email_vs_download <- function(){

  # client files
  f_cepochs_email = parse_epochs(
    f = file.path(
      raw_dir, "single",
      "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
  )
  f_cepochs_download = parse_epochs(
    f = file.path(
    raw_dir, "CLIENT",
    "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis.csv")
  )

  testthat::expect_true(all(f_cepochs_email$white_light == f_cepochs_download$white_light, na.rm = T))

  # partner files
  f_pepochs_email <- parse_epochs(
    f = file.path(
      raw_dir, "single",
      "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
  )
  f_pepochs_download <- parse_epochs(
    f = file.path(
      raw_dir, "PARTNER",
      "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis.csv")
  )
  testthat::expect_true(all(f_cepochs_email$white_light == f_cepochs_download$white_light, na.rm = T))

  # client stats
  f_cstats_email <- parse_statistics(
    f = file.path(
      raw_dir, "single",
      "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
  )
  f_cstats_download = parse_statistics(
    f = file.path(
      raw_dir, "CLIENT",
      "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis.csv")
  )
  testthat::expect_true(all(f_cstats_email$start_time == f_cstats_download$start_time, na.rm = T))

  # partner stats
  f_pstats_email <- parse_statistics(
    f = file.path(
      raw_dir, "single",
      "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
  )
  f_pstats_download <- parse_statistics(
    f = file.path(
      raw_dir, "PARTNER",
      "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis.csv")
  )
  testthat::expect_true(all(f_pstats_email$start_time == f_pstats_download$start_time, na.rm = T))

}

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


# bin_series_at_anker()
test_bin_series_at_anker <- function(){
  start_time <- as.POSIXct("2013-06-17 14:20:00 UTC")
  steps <- 12
  end_time <- start_time + as.difftime(steps-1, units = "mins")
  filter_out <- 3
  data <- data.frame(
    line = 1:steps,
    small_value = rnorm(steps),
    value = round(rnorm(steps)*10),
    date_time = seq(start_time, end_time, by = "mins")
    ) #|>
    #dplyr::filter(!(.data$line %in% filter_out))

  anker_time <- data$date_time[steps/2]
  # numeric relative datetime in minutes
  data$nrel_tmin <- as.numeric(data$date_time - anker_time) / 60
  new_data <- bin_series_at_anker(data, target_cols = "value", rel_col = "nrel_tmin", bin_size = steps/4)
  testthat::expect_equal(new_data$bin[2], 0)
}

# plot_activity()
test_plot_activity <- function(){
  start_time <- as.POSIXct("2013-06-17 14:20:00 UTC")
  steps <- 1000
  end_time <- start_time + as.difftime(steps-1, units = "mins")
  data <- data.frame(
    line = 1:steps,
    small_value = rnorm(steps),
    value = round(rnorm(steps)*10),
    date_time = seq(start_time, end_time, by = "mins")
  )

  anker_time <- data$date_time[steps/2]
  # numeric relative datetime in minutes
  data$nrel_tmin <- as.numeric(data$date_time - anker_time) / 60
  new_data <- bin_series_at_anker(data, target_cols = "value", rel_col = "nrel_tmin", bin_size = steps/4)

  # plot works for original time series
  plot_on_original <- plot_activity(data)
  testthat::expect_true(plot_on_original)

  # plot works for binned time series
  plot_on_binned <- plot_activity(new_data)
  testthat::expect_true(plot_on_binned)

}


cprint("All tests passed.", col = "g")

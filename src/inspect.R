# INSPECTION


# Inspection single individuals data
inspect <- function(data, aggstats, plotting = FALSE, verbose = TRUE) {
  # data: data of epochs of ONE case only
  # aggstats: statistics of aggregated data

  if (length(unique(data$sid)) > 1){
    warning("Invalid request. This dataset contains more than one case. Consider iterating over subsets.")
    return(data)
  }

  # Time series characteristics ---------------------------------------------

  if (verbose) {
    # how many different periods?
    count_periods <- aggstats |>
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
    all_time_diffs <- difftime(data[2:nrow(data), "date_time"], data[1:(nrow(data) - 1), "date_time"])
    regular_1hz <- all(all_time_diffs == 1)
    if (!regular_1hz) {
      cprint("Irregular time bins. Data needs to be sampled with 1 Hz. Further operations for binning wil fail.", colour = "r")
    }

    # missing data
    data$invalid <- ifelse(complete.cases(data), FALSE, TRUE)
    na_periods <- periods_of_target(data, target = "invalid")
    cprint(sprintf("%.0f periods of at least 1 minute have missing oberservations in one or more variables.", nrow(na_periods)), colour = "y")
  }


  # Mean light and activity -------------------------------------------------

  # construct relative time at anker = experiment begin
  data <- dplyr::mutate(data, nrel_date_time = (as.numeric(.data$date_time - .data$date_time[1]) / 60))

  # in 5-minute bins, in 30-minute bins, in 60 minute bins
  binned_data <- calc_multiple_bins(data, bin_sizes = c(5,10,30,60))
  data5 <- binned_data$data5
  data30 <- binned_data$data30


  # Regularity --------------------------------------------------------------

  cprint("Visualisations will be made on data binned over time.", colour = "y")

  # visual inspection of raw data
  p_color <- plot_colorlight(data5, title = "Light exposure of for 5-minute bins")
  p_white <- plot_whitelight(data5, title = "White light exposure of for 5-minute bins")
  p_activity <- plot_activity(data5, title = "Activity of for 5-minute bins")
  p_sleep <- plot_sleep(data5, title = "% Sleep for 5-minute bins")

  if (plotting) {
    plot(p_color)
    plot(p_white)
    plot(p_activity)
    plot(p_sleep)
  }

  # autocorrelation and partial autocorrelation
  ac_list <- get_autocor(data30, max_lag = Inf)
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
      title = sprintf("Autocorrelogram of %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
      )

    # partial autocorrelation plot
    pac_plot_list[[var_name]] <- plot_correlogram(
      cordata = current_ac,
      kind = "p-auto",
      title = sprintf("Partial autocorrelogram of %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
    )

    if (plotting){
      plot(cowplot::plot_grid(ac_plot_list[[var_name]], pac_plot_list[[var_name]]))
    }
  }

  cprint("All variables visually show periodicity of 24 hour.\n", colour = "y")

  # Correlation -------------------------------------------------------------

  # cross correlations of activity and lights
  cc_list <- get_crosscor(data30, max_lag = Inf)

  # plot cross-correlograms
  cc_plot_list <- list()
  for (i in seq(cc_list)){
    var_name <- sub("_", " ", var_names[i])
    current_cc <- cc_list[[i]]

    # autocorrelation plot
    cc_plot_list[[var_name]] <- plot_correlogram(
      cordata = current_cc,
      kind = "cross",
      title = sprintf("Cross-correlogram of activity and %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
    )
    if (plotting){
      plot(cc_plot_list[[var_name]])
    }
  }

  return(binned_data)
}

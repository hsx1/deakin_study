# INSPECTION


# Inspection single individuals data
inspect <- function(data, aggstats, plotting = FALSE, verbose = TRUE) {
  # data: data of epochs of ONE case only
  # aggstats: statistics of aggregated data

  # checks
  if (length(unique(data$sid)) > 1) {
    warning(
      paste(
        "Invalid request. This dataset contains more than one",
        "cases (participants). Consider iterating over subsets."
      )
    )
    return(data)
  }

  if (!is_regular1hz(data)) {
    cprint(
      paste(
        "Irregular time bins. Data needs to be sampled with 1 Hz.",
        "Further operations and output may not be reliable or fail."
      ),
      colour = "r"
    )
  }

  # Time series characteristics ---------------------------------------------

  info <- timeseries_characteristics(data, aggstats, verbose = verbose)

  # Mean light and activity -------------------------------------------------

  # construct relative time at anker = experiment begin
  data <- dplyr::mutate(data, nrel_date_time = (as.numeric(.data$date_time - .data$date_time[1]) / 60))

  # in 5-minute bins, in 30-minute bins, in 60 minute bins
  binned_data <- calc_multiple_bins(data, rel_col = "nrel_date_time", bin_sizes = c(5, 10, 30, 60))
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
  bin_size <- as.numeric(data30$time[2] - data30$time[1]) / 60
  var_names <- names(ac_list)
  ac_plot_list <- list()
  pac_plot_list <- list()

  for (i in seq(ac_list)) {
    var_name <- sub("_", " ", var_names[i])
    current_ac <- ac_list[[i]]

    # autocorrelation plot
    ac_plot_list[[var_name]] <- plot_correlogram(
      cordata = current_ac,
      kind = "auto",
      data = data30,
      title = sprintf("Autocorrelogram of %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
    )

    # partial autocorrelation plot
    pac_plot_list[[var_name]] <- plot_correlogram(
      cordata = current_ac,
      kind = "p-auto",
      data = data30,
      title = sprintf("Partial autocorrelogram of %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
    )

    if (plotting) {
      plot(cowplot::plot_grid(ac_plot_list[[var_name]], pac_plot_list[[var_name]]))
    }
  }

  cprint("All variables visually show periodicity of 24 hour.\n", colour = "y")

  # Correlation -------------------------------------------------------------

  # cross correlations of activity and lights
  cc_list <- get_crosscor(data30, max_lag = Inf)

  # plot cross-correlograms
  cc_plot_list <- list()
  for (i in seq(cc_list)) {
    var_name <- sub("_", " ", var_names[i])
    current_cc <- cc_list[[i]]

    # autocorrelation plot
    cc_plot_list[[var_name]] <- plot_correlogram(
      cordata = current_cc,
      data = data30,
      kind = "cross",
      title = sprintf("Cross-correlogram of activity and %s for %.0f-minute bins", var_name, bin_size),
      file_spec = var_name
    )
    if (plotting) {
      plot(cc_plot_list[[var_name]])
    }
  }

  return(binned_data)
}

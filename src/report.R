# REPORT


# checks ------------------------------------------------------------------

is_regular1hz <- function(data) {
  all_time_diffs <- difftime(data[2:nrow(data), "date_time"], data[1:(nrow(data) - 1), "date_time"])
  regular_1hz <- all(all_time_diffs == 1)
  return(regular_1hz)
}


# descriptions ------------------------------------------------------------

timeseries_characteristics <- function(data, aggstats, verbose = FALSE) {
  # how many different periods?
  info <- list()
  info$status_tab <- aggstats |>
    dplyr::group_by(.data$interval_type) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(!grepl("Summary", .data$interval_type))
  # how long is the data?
  info$complete_tab <- data.frame(
    n_obs = nrow(data),
    n_complete = sum(complete.cases(data)),
    t_interval_missing = hms::as_hms(sum(!complete.cases(data)) * 60),
    perc_missing = sum(!complete.cases(data)) / nrow(data) * 100
  )
  # missing data
  info$na_tab <- data |>
    dplyr::mutate(invalid = ifelse(complete.cases(data), FALSE, TRUE)) |>
    periods_of_target(target = "invalid")


  if (verbose) {
    # how many different periods?
    cprint(
      sprintf(
        "Time series spans %.0f days, including %0.f ACTIVE periods, %0.f REST periods and %0.f SLEEP periods.",
        info$status_tab$n[info$status_tab$interval_type == "DAILY"],
        info$status_tab$n[info$status_tab$interval_type == "ACTIVE"],
        info$status_tab$n[info$status_tab$interval_type == "REST"],
        info$status_tab$n[info$status_tab$interval_type == "SLEEP"]
      )
    )
    # how long is the data?
    cprint(
      sprintf(
        "Dataset contains %.0f observations, %.0f complete observations with data in all variables, %s hours are not covered (%.2f%%).",
        info$complete_tab$n_obs, info$complete_tab$n_complete,
        info$complete_tab$t_interval_missing, info$complete_tab$perc_missing
      )
    )
    # missing data
    cprint(
      sprintf(
        "%.0f periods of at least 1 minute have missing oberservations in one or more variables.",
        nrow(info$na_tab)
      )
    )
  }
  return(info)
}


# tables ------------------------------------------------------------------

# Extracts periods of TRUE for boolean target variables similar to "Statistics" overview
periods_of_target <- function(data, target) {
  # data: data frame
  # target: bool, e.g. target = "invalid"

  # always include the first row (preliminary)
  data$previous_different <- TRUE
  for (i in 2:nrow(data)) {
    # next value i + 1 same as value before i
    data$previous_different[i] <- (data[i, target] != data[i - 1, target])
  }

  # extract blocks starting with first block (whether TRUE or FALSE doesn't matter, because first row is always true)
  transitions <- data[data$previous_different, c("date_time", target)]
  # drop first block, if is doesn't start with target
  if (transitions[1, target] == FALSE) {
    transitions <- transitions[-1, ]
  }

  # first transition always "to TRUE", e.g. NA
  transitions_start <- transitions[seq(from = 1, to = nrow(transitions), by = 2), "date_time"]
  transitions_end <- transitions[seq(from = 2, to = nrow(transitions), by = 2), "date_time"]
  # for cases without second transition "to FALSE", e.g. ending with NA, add last minute+1
  if (length(transitions_start) != length(transitions_end)) {
    transitions_end <- c(transitions_end, data$date_time[nrow(data)] + lubridate::minutes(1))
  }

  overview <- data.frame(
    target = target,
    start_time = transitions_start,
    end_time = transitions_end,
    duration = transitions_end - transitions_start
  )
  return(overview)
}


# correlograms ------------------------------------------------------------

plot_correlogram <- function(cordata, data, kind = "auto",
                             plot_ylim = NULL,
                             plot_xlim = NULL,
                             title = "Correlogram",
                             subtitle = NULL,
                             breaks, file_spec = FALSE, save_plot = TRUE) {
  # kind: can be "auto" for autocorrelation, "p-auto" for partial autocorrelation or "cross" for cross-correlation
  # file_spec: file specification for saving the plot

  x <- "lag"
  if (is.null(plot_ylim)) {
    plot_ylim <- c(min(cordata$acf, cordata$pacf, na.rm = T), 1)
  }

  if (kind == "auto") {
    title <- ifelse(missing(title), "Autocorrelation", title)
    raw_p <- ggplot(
      cordata,
      aes(x = .data[["lag"]], y = .data[["acf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        limits = plot_ylim
      )

  } else if (kind == "p-auto") {
    title <- ifelse(missing(title), "Partial Autocorrelation", title)
    raw_p <- ggplot(
      cordata,
      aes(x = .data[["lag"]], y = .data[["pacf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        limits = plot_ylim
      )

  } else if (kind == "cross") {
    title <- ifelse(missing(title), "Cross Correlation", title)
    raw_p <- ggplot(
      cordata,
      aes(x = .data[["lag"]], y = .data[["ccf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7)

  } else {
    warning("No valid specification.")
    return("")
  }


  p <- raw_p +
    labs(
      x = "Lag",
      y = "Effect size",
      title = title,
      subtitle = subtitle
    ) +
    theme_bw()

  if (!(is.null(plot_xlim))) {
    p <- p + xlim(plot_xlim)
  }


  # save
  if (save_plot) {
    file_spec <- ifelse(missing(file_spec), "", sprintf("_%s", file_spec))
    group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
    id_folder <- data$id[1]
    save_plot2pdf(
      filename = file.path(group_folder, id_folder, sprintf(
        "plot_%s%s.pdf", kind,
        file_spec
      )),
      plot = p,
      w = 200, h = 100
    )
  }
  return(p)
}


# Descriptive activity/exposure plots -------------------------------------

# Plot of colored light exposure
plot_colorlight <- function(data, title = "Colored light exposure by day", log = FALSE, save_plot = TRUE) {

  # structural part
  longdata <- data |>
    tidyr::pivot_longer(cols = c("red_light", "green_light", "blue_light")) |>
    tidyr::drop_na()
  raw_p <- ggplot(data = longdata, aes(x = .data$time, color = .data$name, na.rm = TRUE)) +
    geom_line(aes(y = .data$value)) +
    facet_grid(facets = .data$date ~ .)
  if (log) {
    raw_p <- raw_p +
      scale_y_continuous(trans = "log10", labels = scales::comma)
  }

  # design part
  p <- raw_p +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24) # ,
      # labels = \(time) paste(strsplit(as.character(time), ":")[[1]][1:2], collapse = ":")
    ) +
    scale_color_manual(
      limits = c("red_light", "green_light", "blue_light"),
      values = c("#CC0000", "#009900", "#0066CC"),
      labels = c("red", "green", "blue")
    ) +
    labs(
      color = "Color",
      x = "Time [hh:mm:ss]",
      y = "Light [\u00B5W / m\u00B2]"
    ) +
    ggtitle(title) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 45, vjust = 0.5, hjust = 1,
        margin = margin(t = -8, r = 0, b = 15, l = 0)
      )
    )
  if (save_plot) {
    group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
    id_folder <- data$id[1]
    save_plot2pdf(
      filename = file.path(group_folder, id_folder, "plot_colorlight.pdf"),
      plot = p,
      w = 200, h = 250
    )
  }
  return(p)
}

# Plot of white light [lux] exposure
plot_whitelight <- function(data, title = "While light exposure by day", save_plot = TRUE) {

  # plot
  p <- ggplot(data = data, aes(x = .data$time)) +
    geom_line(aes(y = .data$white_light)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [hh:mm:ss]") +
    ylab("White Light [lux]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 45, vjust = 0.5, hjust = 1,
        margin = margin(t = -8, r = 0, b = 15, l = 0)
      )
    )
  # save
  if (save_plot) {
    group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
    id_folder <- data$id[1]
    save_plot2pdf(
      filename = file.path(group_folder, id_folder, "plot_whitelight.pdf"),
      plot = p,
      w = 200, h = 250
    )
  }
  return(p)
}

# Plot activity by day
plot_activity <- function(data, title = "Activity by day", save_plot = TRUE) {
  p <- ggplot(data = data, aes(x = .data$time)) +
    # geom_col(aes(y = 1 - .data$activity), color = my_colors[4]) +
    geom_line(aes(y = .data$activity), color = my_colors[4]) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [hh:mm:ss]") +
    ylab("Acitivity [> 20 counts]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 45, vjust = 0.5, hjust = 1,
        margin = margin(t = -8, r = 0, b = 15, l = 0)
      )
    )
  # save
  if (save_plot) {
    group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
    id_folder <- data$id[1]
    save_plot2pdf(
      filename = file.path(group_folder, id_folder, "plot_activity.pdf"),
      plot = p,
      w = 200, h = 250
    )
  }
  return(p)
}

plot_sleep <- function(data, title = "Sleep recording", save_plot = TRUE) {
  p <- ggplot(data = data, aes(x = .data$time)) +
    geom_col(aes(y = 1 - .data$sleep_wake), width = 1 * 60 * 10, color = "black") +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [hh:mm:ss]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 45, vjust = 0.5, hjust = 1,
        margin = margin(t = -8, r = 0, b = 15, l = 0)
      )
    )

  if (length(unique(data$sleep_wake[!is.na(data$sleep_wake)])) > 2) {
    p <- p + ylab("Sleep recording for bins [%] (> 10 minute immobile)")
  }
  # save
  if (save_plot) {
    group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
    id_folder <- data$id[1]
    save_plot2pdf(
      filename = file.path(group_folder, id_folder, "plot_sleep.pdf"),
      plot = p,
      w = 200, h = 250
    )
  }
  return(p)
}
# light by time - filtered by sleep - per day

# light by date_time separate per group

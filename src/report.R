# REPORT


# Tables ------------------------------------------------------------------

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



# Correlograms ------------------------------------------------------------

plot_correlogram <- function(cordata, kind = "auto", ylim, title = "Correlogram", breaks, file_spec) {
  # kind: can be "auto" for autocorrelation, "p-auto" for partial autocorrelation or "cross" for cross-correlation
  # file_spec: file specification for saving the plot

  x <- "lag"
  ylim <- ifelse(missing(ylim), c(min(cordata$acf, cordata$pacf, na.rm = T), 1), ylim)
  if (kind == "auto") {
    title <- ifelse(missing(title), "Autocorrelation", title)
    plot_ac <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["acf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        limits = c(min(cordata$acf, cordata$pacf, na.rm = T), 1)
      ) +
      # scale_x_continuous(breaks = seq(0, 400, 50)) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle(title) +
      theme_bw()
    p <- plot_ac

  } else if (kind == "p-auto") {
    title <- ifelse(missing(title), "Partial Autocorrelation", title)
    plot_pac <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["pacf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        limits = c(min(cordata$acf, cordata$pacf, na.rm = T), 1)
      ) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle(title) +
      theme_bw()
    p <- plot_pac

  } else if (kind == "cross") {
    title <- ifelse(missing(title), "Cross Correlation", title)
    plot_cc <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["ccf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle(title) +
      theme_bw()
    p <- plot_cc

  } else {
    warning("No valid specification.")
    return("")
  }

  # save
  file_spec <- ifelse(missing(file_spec), "", sprintf("_%s", file_spec))
  group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
  id_folder <- data$id[1]
  save_plot2pdf(
    filename = file.path(group_folder, id_folder, sprintf("plot_%s%s.pdf", kind,
                                                          file_spec)),
    plot = p,
    w = 200, h = 100
  )
  return(p)
}


# Descriptive activity/exposure plots -------------------------------------

# Plot of colored light exposure
plot_colorlight <- function(data, title = "Colored light exposure by day", log = FALSE) {

  # structural part
  longdata <- data |>
    tidyr::pivot_longer(cols = c("red_light", "green_light", "blue_light")) |>
    tidyr::drop_na()
  if (log) {
    raw_p <- ggplot(data = longdata, aes(x = .data$time, color = .data$name, na.rm = TRUE)) +
      geom_line(aes(y = log(.data$value))) +
      facet_grid(facets = .data$date ~ .)
  } else {
    raw_p <- ggplot(data = longdata, aes(x = .data$time, color = .data$name, na.rm = TRUE)) +
      geom_line(aes(y = .data$value)) +
      facet_grid(facets = .data$date ~ .)
  }

  # design part
  p <- raw_p +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    scale_color_manual(
      limits = c("red_light", "green_light", "blue_light"),
      values = c("#CC0000", "#009900", "#0066CC"),
      labels = c("red", "green", "blue")
    ) +
    labs("Light color") +
    xlab("Time [h:m:s]") +
    ylab("Light [\u00B5W / m\u00B2]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1)
    )
  group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
  id_folder <- data$id[1]
  save_plot2pdf(
    filename = file.path(group_folder, id_folder, "plot_colorlight.pdf"),
    plot = p,
    w = 200, h = 250
  )

  return(p)
}


# Plot of white light [lux] exposure
plot_whitelight <- function(data, title = "While light exposure by day") {

  # plot
  p <- ggplot(data = data, aes(x = .data$time)) +
    geom_line(aes(y = .data$white_light)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ylab("White Light [lux]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1)
    )
  # save
  group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
  id_folder <- data$id[1]
  save_plot2pdf(
    filename = file.path(group_folder, id_folder, "plot_whitelight.pdf"),
    plot = p,
    w = 200, h = 250
  )
  return(p)
}

# Plot activity by day
plot_activity <- function(data, title = "Activity by day") {

  p <- ggplot(data = data, aes(x = .data$time)) +
    geom_col(aes(y = 1 - .data$sleep_wake), color = "grey50") +
    geom_line(aes(y = .data$activity)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ylab("Acitivity [> 20 counts]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1)
    )
  # save
  group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
  id_folder <- data$id[1]
  save_plot2pdf(
    filename = file.path(group_folder, id_folder, "plot_activity.pdf"),
    plot = p,
    w = 200, h = 250
  )
  return(p)
}

plot_sleep <- function(data, title = "Sleep probability") {
  p <- ggplot(data = data, aes(x = .data$time)) +
    geom_col(aes(y = 1 - .data$sleep_wake)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = seq(0, 24, 2) * 60 * 60,
      minor_breaks = (0:24) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1)
    )

  if (length(unique(data$sleep_wake[!is.na(data$sleep_wake)])) > 2) {
    p <- p + ylab("Sleep probability for bins [%] (> 10 minute immobile)")
  }
  # save
  group_folder <- ifelse(data$group[1] == "C", "CLIENT", "PARTNER")
  id_folder <- data$id[1]
  save_plot2pdf(
    filename = file.path(group_folder, id_folder, "plot_sleep.pdf"),
    plot = p,
    w = 200, h = 250
  )
  return(p)
}
# light by time - filtered by sleep - per day

# light by date_time separate per group

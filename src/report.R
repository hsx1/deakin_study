# REPORT


# Tables ------------------------------------------------------------------


# Extracts periods of TRUE for boolean target variables similar to "Statistics" overview
periods_of_target <- function(data, target) {
  # data: data frame
  # target: bool, e.g. target = "invalid",

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


# Plots -------------------------------------------------------------------

plot_correlogram <- function(cordata, x = "lag", kind = "auto") {
  # kind: can be "auto" or "cross"
  if (kind == "auto") {
    plot_ac <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["acf"]])
      ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        name = element_blank(),
        limits = c(min(cordata$acf, cordata$pacf, na.rm = T), 1)
        ) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle("ACF") +
      theme_bw()

    plot_pac <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["pacf"]])
      ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_y_continuous(
        name = element_blank(),
        limits = c(min(cordata$acf, cordata$pacf, na.rm = T), 1)
      ) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle("PACF") +
      theme_bw()

    p <- list(plot_ac, plot_pac)
    #p <- cowplot::plot_grid(plot_ac, plot_pac, nrow = 1)
  } else if (kind == "cross") {
    plot_cc <- ggplot(
      cordata,
      aes(x = .data[[x]], y = .data[["ccf"]])
    ) +
      geom_col(fill = "#4373B6", width = 0.7) +
      xlab("Lag") +
      ylab("Effect size") +
      ggtitle("CCF") +
      theme_bw()
    p <- plot_cc
  } else {
    warning("No valid specification.")
    return("")
  }


  return(p)
}

# TODO adapt for binned time series

# light by time separate per day

# colored light
plot_colorlight <- function(data) {
  longdata <- data |> tidyr::pivot_longer(cols = c("red_light", "green_light", "blue_light"))
  ggplot(data = longdata, aes(x = .data$date_time, color = name)) +
    geom_line(aes(y = (.data$value))) + # maybe rather log?
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = (0:23) * 60 * 60,
      minor_breaks = (0:23) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ylab("Light [\u00B5W / m\u00B2]") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  longdata <- data5 |> tidyr::pivot_longer(cols = c("red_light", "green_light", "blue_light"))
  ggplot(data = longdata, aes(x = .data$nrel_date_time, color = name)) +
    geom_line(aes(y = (.data$value))) + # maybe rather log?
    # facet_grid(facets = .data$date ~ .) +
    scale_x_continuous(name = "Time [sec]", breaks = ) +
    xlab("Time [sec]") +
    ylab("Light [\u00B5W / m\u00B2]") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


# white light [lux]
plot_whitelight <- function(data) {
  ggplot(data = data, aes(x = .data$time)) +
    geom_line(aes(y = .data$white_light)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = (0:23) * 60 * 60,
      minor_breaks = (0:23) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ylab("White Light [lux]") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# light by time - filtered by sleep - per day

# light by date_time separate per group

# activity by time
plot_activity <- function(data) {
  ggplot(data = data, aes(x = .data$time)) +
    geom_line(aes(y = .data$activity)) +
    facet_grid(facets = .data$date ~ .) +
    scale_x_time(
      breaks = (0:23) * 60 * 60,
      minor_breaks = (0:23) * 60 * 60,
      limits = c(0, 60 * 60 * 24)
    ) +
    xlab("Time [sec]") +
    ylab("Acitivity [> 20 counts]") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

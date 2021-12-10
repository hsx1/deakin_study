# Main analysis script

library(ggplot2)
library(lubridate)

source("./src/load.R")
source("./src/report.R")



# Load data ---------------------------------------------------------------
# 1.	What is the data format?

# parse files
cdata <- parse_epochs(f = file.path(data_dir, "single", "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv"))
pdata <- parse_epochs(f = file.path(data_dir, "single", "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv"))
cstats <- parse_statistics(f = file.path(data_dir, "single", "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv"))
pstats <- parse_statistics(f = file.path(data_dir, "single", "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv"))


data <- cdata
data <- pdata
# reformat date and time
data <- data |>
  dplyr::mutate(
    date_time = lubridate::ymd_hms(paste(.data$date, .data$time), tz = "UTC"),
    time = as.numeric(lubridate::hms(.data$time)),
    date = lubridate::ymd(date)
    )

# group by day
# data <- data |> dplyr::group_by(.data$date)


# Time series characteristics ---------------------------------------------
# 2.	How long is the data set?
nrow(data)

# Missing data ------------------------------------------------------------
# 3.	Is there any missing data?
data$na <- ifelse(complete.cases(data), TRUE, NA)

extract_na_periods(data=data, target="na")
extract_na_periods(data=data, target="sleep_wake")
extract_na_periods(data=data, target="activity")

# Mean light and activity -------------------------------------------------
# 4.	What are the mean light and activity data in hour / 30-minute / 1-minute bins?

#


# Regularity --------------------------------------------------------------
# 5.	How regular is the activity and light exposure?

# visual inspection
# autocorrelation with 24-hour lag (or various lags)


# Correlation -------------------------------------------------------------
# 6.	Are light and activity correlated?


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


# CONFIGURATIONS

# This is the only script that contains library calls and source commands
# except for the main script that ONLY contains a source command for the
# present script, to make sure there are no recursive actions.
# Do not add constant values to other scripts, to make sure nothing is overwritten.


# constants ---------------------------------------------------------------

data_dir <- "./data"
raw_dir <- file.path(data_dir, "raw")
sid_regex <- "(C|P)\\d{4}"
light_cols <- c("white_light", "red_light", "blue_light", "green_light")
target_cols <- c("date_time", "sleep_wake", "activity", light_cols)


# libraries ---------------------------------------------------------------

# TODO install required
library("lubridate")
library("ggplot2")
library("dplyr")
library("tidyr")
library("hms")


# source ------------------------------------------------------------------

source("./src/load.R")
source("./src/report.R")
source("./src/transform.R")
source("./src/inspect.R")
source("./src/utils.R")


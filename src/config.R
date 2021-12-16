# CONFIGURATIONS

# This is the only script that contains library calls and source commands
# except for the main script that ONLY contains a source command for the
# present script, to make sure there are no recursive actions.
# Do not add constant values to other scripts, to make sure nothing is overwritten.


# libraries ---------------------------------------------------------------

# install required
all_packages <-
  c("lubridate",
    "ggplot2",
    "dplyr",
    "tidyr",
    "hms",
    "RColorBrewer",
    "testthat",
    "stargazer",
    "knitr",
    "kableExtra",
    "gridExtra",
    "stringr")
new_packages <-
  all_packages[!(all_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}
# load packages
lapply(all_packages, require, character.only = TRUE)

# source ------------------------------------------------------------------

source("./src/load.R")
source("./src/report.R")
source("./src/transform.R")
source("./src/inspect.R")
source("./src/utils.R")


# constants ---------------------------------------------------------------

DFLT_COLORS <- RColorBrewer::brewer.pal(n=9,"Set1")
DATA_DIR <- "./data"
RAW_DIR <- file.path(DATA_DIR, "raw")
SID_REGEX <- "(C|P)\\d{4}"
LIGHT_COLS <- c("white_light", "red_light", "blue_light", "green_light")
TARGET_COLS <- c("date_time", "sleep_wake", "activity", LIGHT_COLS)

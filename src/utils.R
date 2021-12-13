# UTILS


# packages ----------------------------------------------------------------

# TODO install if not available
library("lubridate")
library("ggplot2")
library("dplyr")
library("tidyr")
library("hms")


# source everything -------------------------------------------------------

source("./src/load.R")
source("./src/report.R")
source("./src/format.R")

# save --------------------------------------------------------------------

# uses fun() to create object and caches the result as an rds object
rds_cached <- function(filename, fun, ...) {
  if (file.exists(filename)) {
    re <- readRDS(filename)
  } else {
    re <- fun(...)
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    if (!(all(is.null(re)) | all(is.na(re)))){
      saveRDS(re, file = filename)
    }
  }
  return(re)
}


# console -----------------------------------------------------------------

# uses colored printing for terminal output
cprint <- function(txt, colour) {
  if (missing(colour)){
    colcode = 99
  }else{
    df <- data.frame(
      code = c(
        "red", "green", "yellow", "blue", "purple", "turquoise", "backred",
        "backgreen", "backyellow", "backblue", "backpurple", "backturquois", "white",
        "r", "g", "y", "b", "p", "t", "br", "bg", "by", "bb", "bp", "bt", "w"
      ),
      num = c(
        31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 99,
        31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 99
      )
    )
    colcode <- df$num[df$code == colour]
  }

  if (colcode == 99){
    cat(txt)
  }else{
    cat(paste0("\033[0;", colcode, "m", txt, "\033[0m", "\n"))
  }
}


# formatting --------------------------------------------------------------

# adds empty variable with chose name (for functions only)
add_newvar <- function(data, new_name){
  if (new_name %in% colnames(data)){
    warning(sprintf("%s already exists.", new_name))
    return(data)
  }
  data$new_var <- NA
  colnames(data)[colnames(data) == "new_var"] <- new_name
  return(data)
}

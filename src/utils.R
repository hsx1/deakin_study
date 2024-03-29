# UTILS

# save --------------------------------------------------------------------

# uses fun() to create object and caches the result as an rds object
rds_cached <- function(filename, fun, ...) {
  if (file.exists(filename)) {
    re <- readRDS(filename)
  } else {
    re <- fun(...)
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    if (!(all(is.null(re)) | all(is.na(re)))) {
      saveRDS(re, file = filename)
    }
  }
  return(re)
}

# save plot
save_plot2pdf <- function(filename, plot, w, h) {
  main_dir <- file.path(".", "notebook", "figures")
  filepath <- file.path(
    main_dir,
    filename
  )
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    file = filepath,
    plot = plot,
    width = w,
    height = h,
    units = "mm"
  )
  cprint(txt = sprintf("Figure saved in: %s\n", filepath), colour = "w")
}


# console -----------------------------------------------------------------

# uses colored printing for terminal output
cprint <- function(txt, colour) {
  if (missing(colour)) {
    colcode <- 99
  } else {
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

  if (colcode == 99) {
    cat(txt)
  } else {
    cat(paste0("\033[0;", colcode, "m", txt, "\033[0m", "\n"))
  }
}

# percent of zero values
perc_zero <- function(v){
  # v: numeric vector
  sprintf("%.1f%%",sum(v == 0)/length(v) * 100)
}


# formatting --------------------------------------------------------------

# adds empty variable with chose name (for functions only)
add_newvar <- function(data, new_name) {
  if (new_name %in% colnames(data)) {
    warning(sprintf("%s already exists.", new_name))
    return(data)
  }
  data$new_var <- NA
  colnames(data)[colnames(data) == "new_var"] <- new_name
  return(data)
}

# Capitalize first letter of string
str_capitalize <- function(s) {
  # s: string
  first <- toupper(substring(s, 1, 1))
  rest <- substring(s, 2)
  return(paste0(first, rest))
}

# Remove snake case and capitalize
str_snake2human <- function(s){
  no_snake <- sub("_", " ", s)
  return(str_capitalize(no_snake))
}

datetime2time <- function(dttm){

}

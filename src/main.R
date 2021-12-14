# Main analysis script

source("./src/utils.R")

main <- function(select_id = "C1045"){
  # select_id: can be FALSE or numeric ID (without group prefix, e.g. 1045)

  # Load data ---------------------------------------------------------------

  # process raw files and transform
  dinfo <- load_paths(raw_dir, groups, re)

  process_epochs(dinfo, return_object = FALSE)
  process_stats(dinfo, return_object = FALSE)

  # list and merge files to preprocessed data
  epochsinfo <- load_paths(in_dir = file.path(data_dir, "preprocessed")) |>
    tidyr::pivot_longer(cols = c("cfile", "pfile"), names_to = "group")
  statsinfo <- load_paths(in_dir = file.path(data_dir, "statistics")) |>
    tidyr::pivot_longer(cols = c("cfile", "pfile"), names_to = "group")
  infofiles <- full_join(epochsinfo, statsinfo, by = c("id", "group")) |>
    dplyr::mutate(group = dplyr::recode(.x = .data$group, pfile = "P", cfile = "C"))
  colnames(infofiles) <- c("id", "group", "epochs", "aggstats")


  if (select_id) {
    infofiles <- infofiles[infofiles$id == select_id, ]
  }

  for (i in seq_len(nrow(infofiles))) {

    if (!is.na(infofiles$epochs[i])){
      data <- readRDS(infofiles$epochs[i])
      aggstats <- readRDS(infofiles$aggstats[i])
      #preprocess(data, aggstats)
    }
  }
  return()

}

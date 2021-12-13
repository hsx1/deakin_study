# Main analysis script

source("./src/utils.R")

main <- function(select_id = 1045){
  # select_id: can be FALSE or numeric ID (without group prefix, e.g. 1045)

  # Load data ---------------------------------------------------------------

  process_epochs(raw_dir, return_object = FALSE)
  process_stats(raw_dir, return_object = FALSE)
  epochsinfo <- load_paths(in_dir = file.path(data_dir, "transforms"))
  statsinfo <- load_paths(in_dir = file.path(data_dir, "statistics"))

  # merge
  infofiles <- full_join(epochsinfo, statsinfo, by = "id")
  colnames(infofiles) <- c("id", "cepoch", "pepoch", "cstats", "pstats")

  if (select_id) {
    infofiles <- infofiles[infofiles$id == select_id, ]
  }

  for (dyad in seq_len(nrow(infofiles))) {
    cdata <- readRDS(infofiles$cepoch)
    pdata <- readRDS(infofiles$pepoch)
    cstats <- readRDS(infofiles$cstats)
    pstats <- readRDS(infofiles$pstats)

    # analysis(data, st)
  }

}

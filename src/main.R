# Main analysis script

source("./src/utils.R")

#main <- function(){

# Load data ---------------------------------------------------------------
# 1.	What is the data format?

# parse files
cdata <- parse_epochs(
  f = file.path(
    data_dir, "single",
    "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
)
pdata <- parse_epochs(
  f = file.path(
    data_dir, "single",
    "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
)
cstats <- parse_statistics(
  f = file.path(
    data_dir, "single",
    "C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")
)
pstats <- parse_statistics(
  f = file.path(
    data_dir, "single",
    "P1045_Acti1_Week_1_22_11_2016_5_10_00_PM_New_Analysis[1][1].csv")
)

ctables <- list(data=cdata, istat=cstats)
ptables <- list(data=cdata, istat=cstats)
# for (tabl in c(ctables, ptables)){...}
data <- tabl$data
istat <- tabl$istat

# analysis(data, istat)

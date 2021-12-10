# SCRATCHPAD
source("./src/load.R")

# check if file
a <- parse_file("./data/raw/manuel/C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis[1][1].csv")$epochs
b <- parse_file("./data/raw/CLIENT/C1045_Acti_1_Week_1_22_11_2016_5_03_00_PM_New_Analysis.csv")$epochs
all(a[!is.na(a)] == b[!is.na(a)])

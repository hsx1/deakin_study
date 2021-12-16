# Basic stats and exploratory analysis on of the Deakin Study

## Description

This repository contains all script and a RMarkdown notebook to run some basic descriptive and exploratory analysis on one example dataset of the Deakin Study on chronic insomnia with clients and their bed partners.

## Usage

You can either view the results or compute them yourself.

### View Results

Open [index.html](https://hsx1.github.io/deakin_study/analysis.html).

### Compute

Step 1: Download the original dataset [here](https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.b8gtht7bh)

Step 2: Unzip the downloaded dataset move the folders `CLIENT` and `PARTNER` to folder `data`/`raw`

Step 3: Open `analysis.Rmd` and press "Knit to HTML"

## Dependencies

Only tested on `R` version 4.1.1 on windows. 

Scripts depend on the following packages:

- lubridate
- ggplot2
- dplyr
- tidyr
- hms
- RColorBrewer
- testthat
- stargazer
- knitr
- kableExtra
- gridExtra

## Reference & License

This project is licensed Creative Common License (CC0 1.0) .

To run this code, the Deakin study dataset of chronic insomnia and bed partner actigraphy data is **required**.  You can [download the dataset here](https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.b8gtht7bh)  (CC0 1.0) .

Angelova, Maia et al. (2021), Chronic insomnia and bed partner actigraphy data, Dryad, Dataset, https://doi.org/10.5061/dryad.b8gtht7bh
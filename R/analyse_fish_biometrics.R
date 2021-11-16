################################
################################
#### analyse_fish_biometrics

#### This code:
# 1) Analyses the length-frequency data from IGFS leg 1

#### Steps preceding this code
# 1) Processing of raw data via process_data_raw.R


################################
################################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(magrittr)
library(prettyGraphics)

#### Load data
biometrics <- readRDS("./data/fish/biometrics.rds")


################################
################################
#### Sampling levels

#### Table of sampling levels (see table 4, page 19)


################################
################################
#### Length frequency charts

#### Length frequency charts across all species (with n of obs)


#### End of code.
################################
################################

################################
################################
#### analyse_fish_catches

#### This code:
# 1) Analyses the catch data from IGFS leg 1
# ... i.e., the number of species caught, their weights etc.

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
catches <- readRDS("./data/fish/catches.rds")


################################
################################
#### Species richness

#### Summarise the number of species per haul
catches %>%
  dplyr::group_by(Haul) %>%
  dplyr::summarise(n_spp = length(unique(Species))) %>%
  dplyr::pull(n_spp) %>%
  utils.add::basic_stats()

#### Map the number of species per haul

#### Table of the top 5 species wth the widest distribution
hauls_league_tbl <-
  catches %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(n_hauls = length(unique(Haul))) %>%
  dplyr::arrange(dplyr::desc(n_hauls)) %>%
  dplyr::slice(1:5L) %>%
  dplyr::select(Species,
                `N` = n_hauls)
write.table(hauls_league_tbl,
            file = "./fig/hauls_league_tbl.txt",
            quote = FALSE, sep = ",", row.names = FALSE)


################################
################################
#### Biomass

#### Summarise the catch weight per haul
catches %>%
  dplyr::group_by(Haul) %>%
  dplyr::summarise(n_spp = sum(Catch_Kg)) %>%
  dplyr::pull(n_spp) %>%
  utils.add::basic_stats()

#### Map the catch weight per haul


#### Table of the top 5 species wth the widest distribution
hauls_league_tbl <-
  catches %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(n_hauls = length(unique(Haul))) %>%
  dplyr::arrange(dplyr::desc(n_hauls)) %>%
  dplyr::slice(1:5L) %>%
  dplyr::select(Species,
                `N` = n_hauls)
write.table(hauls_league_tbl,
            file = "./fig/hauls_league_tbl.txt",
            quote = FALSE, sep = ",", row.names = FALSE)


################################
################################
#### Abundance

#### Summarise standardised fish abundance per haul

#### Map standardised fish abundance per haul

#### Table of the top 5 species with the highest abundance


#### End of code.
################################
################################

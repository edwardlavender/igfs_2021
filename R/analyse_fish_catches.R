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
source("./R/define_global_param.R")

#### Essential packages
library(magrittr)
library(prettyGraphics)

#### Load data
catches <- readRDS("./data/fish/catches.rds")
coast   <- readRDS("./data/spatial/coast/coast_s.rds")
bathy   <- raster::raster("./data/spatial/bathy/bathy.tif")


################################
################################
#### Species richness

#### Summarise the number of species per haul
spp_per_haul <-
  catches %>%
  dplyr::group_by(Haul) %>%
  dplyr::mutate(n_spp = length(unique(Species))) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup()
spp_per_haul %>%
  dplyr::pull(n_spp) %>%
  utils.add::basic_stats()
# min  mean median max   sd IQR  MAD
#  17 29.24   28.5  48 6.52 9.5 7.41


#### Map the number of species per haul
## Set up plot to save
png("./fig/map_n_spp_per_haul.png", height = 10, width = 10, units = "in", res = 600)
## Define colour scheme
spp_per_haul_zlim <- c(15, 50)
spp_per_haul_col_param <- pretty_cols_brewer(zlim = spp_per_haul_zlim)
spp_per_haul$col <- spp_per_haul_col_param$col[findInterval(spp_per_haul$n_spp, spp_per_haul_col_param$breaks)]
spp_per_haul_paa <- pretty_axis(side = 4,
                                lim = list(range(spp_per_haul_col_param$breaks)),
                                control_axis = list(las = TRUE),
                                add = FALSE)
## Base map
pretty_map(add_rasters = list(x = bathy,
                              plot_method = raster::plot,
                              col = bathy_col_param$col,
                              legend = FALSE),
           add_polys = list(x = coast, col = "grey"),
           add_points = list(x = spp_per_haul$Lon, y = spp_per_haul$Lat,
                             pch = 21,
                             cex = spp_per_haul$n_spp/10,
                             bg = spp_per_haul$col,
                             col = spp_per_haul$col),
           xlim = c(-11, -5),
)
arrows(-10.5, y0 = 56, y1 = 56.8, length = 0.1, lwd = 2)
raster::scalebar(d = 100,
                 label = "100 km",
                 xy = c(-8, 53.1),
                 lonlat = TRUE)
## Add colour bar
TeachingDemos::subplot(add_colour_bar(data.frame(x = spp_per_haul_col_param$breaks,
                                                 col = c(spp_per_haul_col_param$col, NA)),
                                      pretty_axis_args = spp_per_haul_paa,
                                      mtext_args = list(side = 4, "Species richness", line = 3)
),
x = c(-4.8, -4.5), y = c(54, 56))
## Add titles
mtext(side = 1, expression("Longitude (" * degree * ")"), cex = 1.25, line = 2)
mtext(side = 2, expression("Latitude (" * degree * ")"), cex = 1.25, line = -2)
dev.off()

#### Table of the top 5 species with the widest distribution
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

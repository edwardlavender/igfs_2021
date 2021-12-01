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
spp_per_haul_col_param <- pretty_cols_brewer(zlim = spp_per_haul_zlim, select = 2:8)
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
                                      mtext_args = list(side = 4, "Species richness", line = 3, cex = 1.25)
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
#### Catch composition (by numbers or weight)

#### Method
# Here we plot the catch composition (by weight or numbers) for selected
# ... species in selected groups. This selection  reduces the number of
# ... species on each plot, which helps to keep plots interpretable.

#### Define haul-specific catch numbers/weights
catches_by_haul <-
  catches %>%
  dplyr::group_by(.data$Haul, .data$Species) %>%
  dplyr::mutate(n = dplyr::n(),
                wt = sum(.data$Catch_Kg)) %>%
  dplyr::slice(1L)

#### Define variable to plot ('wt' or 'n')
composition <- "n" # "wt"

#### Set up plot
## Plotting window
png(paste0("./fig/map_composition_by_", composition, ".png"),
    height = 10, width = 6, units = "in", res = 600)
pp <- par(mfrow = c(3, 2), oma = c(3, 3, 2, 2), mar = c(2, 2, 2, 2))
## List of groups
grps <- list(Commerical = commerical,
             Elasmobranchs = elasmobranchs,
             Pelagics = pelagics,
             `Commerical flat` = commerical_flat,
             `Non-commerical` = non_commercial)
## The 'scaling' factors (to scale point size appropriately for each group)
# ... These were derived via trial and error.
scaling <- list(wt = c(27.5, 18.5, 22.5, 10, 20),
                n = c(7.5, 10, 7.5, 7.5, 7.5))

#### Make the map for each group
purrr::walk(seq_len(length(grps)), function(i){

  #### Isolate data for group
  # i <- 1L
  grp <- grps[[i]]
  catches_for_grp <- catches_by_haul %>% dplyr::filter(.data$Species %in% grp)

  #### Assign species unique colours
  catches_for_grp_cols <-
    data.frame(spp = unique(catches_for_grp$Species),
               col = RColorBrewer::brewer.pal(length(grp), "Spectral"))
  catches_for_grp$col <-
    catches_for_grp_cols$col[match(catches_for_grp$Species, catches_for_grp_cols$spp)]

  #### Map species composition (by weight)
  ## Base map [updated from the base map above with a faded colour scheme to improve pie chart clarity]
  pretty_map(add_rasters = list(x = bathy,
                                plot_method = raster::plot,
                                col = bathy_col_param$col,
                                legend = FALSE),
             add_polys = list(x = coast, col = scales::alpha("grey", 0.25), border = "grey"),
             xlim = c(-11, -5)
             )
  arrows(-10.5, y0 = 56, y1 = 56.8, length = 0.1, lwd = 2)
  raster::scalebar(d = 100,
                   label = "100 km",
                   xy = c(-8, 53.1),
                   lonlat = TRUE)
  ## Plot title
  mtext(side = 3,
        text = bquote(bold(.(LETTERS[i])) ~ "(" *.(names(grps)[i]) * ")"),
        cex = 1.25)

  #### Add species composition for each haul
  purrr::walk(split(catches_for_grp, catches_for_grp$Haul), function(h){
    # h <- split(catches_for_grp, catches_for_grp$Haul)[[1]]
    mapplots::add.pie(z = h[, composition, drop = TRUE], x = h$Lon[1], y = h$Lat[1],
                      col = h$col,
                      border = NA,
                      labels = "",
                      radius = log10(sum(h[, composition]))/scaling[[composition]][i])
  })

  #### Add colour key for species
  mapplots::add.pie(z = rep(1, nrow(catches_for_grp_cols)),
                    x = -6.7, y = 53.8, radius = 0.4,
                    col = catches_for_grp_cols$col, border = NA,
                    labels = catches_for_grp_cols$spp, font = 2
  )

})
#### Add titles
mtext(side = 1, expression("Longitude (" * degree * ")"), cex = 1.25, line = 0.5, outer = TRUE)
mtext(side = 2, expression("Latitude (" * degree * ")"), cex = 1.25, line = 0.5, outer = TRUE)
dev.off()



################################
################################
#### Biomass

#### Summarise the catch weight per haul
wt_per_haul <-
  catches %>%
  dplyr::group_by(Haul) %>%
  dplyr::mutate(wt = sum(.data$Catch_Kg)) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup()
wt_per_haul %>%
  dplyr::pull(.data$wt) %>%
  utils.add::basic_stats()
# min   mean median     max    sd    IQR    MAD
# 45.45 817.85 549.44 5219.06 917.8 584.89 459.91

#### Map the catch weight per haul
## Code duplication
# Note that this code is copied from the code for the map above,
# ... with 'n_spp' replaced by 'wt', 'spp_per_haul' replaced by 'wt_per_haul' and any other
# ... lines that have been updated flagged by '[updated ...]'.
## Set up plot to save [updated title]
png("./fig/map_catch_weight_per_haul.png", height = 10, width = 10, units = "in", res = 600)
## Define colour scheme [updated zlim and findInterval]
wt_per_haul_zlim <- c(45, 5250)
wt_per_haul_col_param <- pretty_cols_brewer(zlim = wt_per_haul_zlim, select = 2:8)
wt_per_haul$col <- wt_per_haul_col_param$col[findInterval(wt_per_haul$wt, wt_per_haul_col_param$breaks)]
wt_per_haul_paa <- pretty_axis(side = 4,
                                lim = list(range(wt_per_haul_col_param$breaks)),
                                control_axis = list(las = TRUE),
                                add = FALSE)
## Base map [update scaling]
pretty_map(add_rasters = list(x = bathy,
                              plot_method = raster::plot,
                              col = bathy_col_param$col,
                              legend = FALSE),
           add_polys = list(x = coast, col = "grey"),
           add_points = list(x = wt_per_haul$Lon, y = wt_per_haul$Lat,
                             pch = 21,
                             cex = wt_per_haul$wt/400, # log10(wt_per_haul$wt/100),
                             bg = wt_per_haul$col,
                             col = wt_per_haul$col),
           xlim = c(-11, -5),
)
arrows(-10.5, y0 = 56, y1 = 56.8, length = 0.1, lwd = 2)
raster::scalebar(d = 100,
                 label = "100 km",
                 xy = c(-8, 53.1),
                 lonlat = TRUE)
## Add colour bar [updated zlab and zline]
TeachingDemos::subplot(add_colour_bar(data.frame(x = wt_per_haul_col_param$breaks,
                                                 col = c(wt_per_haul_col_param$col, NA)),
                                      pretty_axis_args = wt_per_haul_paa,
                                      mtext_args = list(side = 4, "Catch weight (kg)", line = 3.75, cex = 1.25)
),
x = c(-4.8, -4.5), y = c(54, 56))
## Add titles
mtext(side = 1, expression("Longitude (" * degree * ")"), cex = 1.25, line = 2)
mtext(side = 2, expression("Latitude (" * degree * ")"), cex = 1.25, line = -2)
dev.off()

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

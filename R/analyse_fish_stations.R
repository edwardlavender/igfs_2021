################################
################################
#### analyse_fish_stations.R

#### This code:
# 1) Analyses the station data from IGFS leg 1
# ... i.e., the number of trawls, where and when they occurred etc.

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
stations <- readRDS("./data/fish/stations.rds")
ctd_xy   <- readRDS("./data/ctd/meta.rds")
coast    <- readRDS("./data/spatial/coast/coast_s.rds")
isles    <- readRDS("./data/spatial/coast/isles_s.rds")
ices     <- readRDS("./data/spatial/ices/ices.rds")
bathy    <- raster::raster("./data/spatial/bathy/bathy.tif")
ocean    <- raster::raster("./data/spatial/bathy/ocean.tif")


################################
################################
#### Map survey area

## Set up figure
png("./fig/map_study_site.png",
    height = 10, width = 10, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 2))

## Define ocean strata
strata <- c("coastal", "medium", "deep", "slope")
ocean_for_ire   <- raster::crop(ocean, box_ire)
ocean_by_strata <- raster::cut(ocean_for_ire, breaks = c(0, 80, 120, 200, 600))
raster::plot(ocean_by_strata)

## Make map
pretty_map(add_rasters = list(x = ocean_by_strata,
                              col = rev(gray.colors(4)),
                              zlim = c(1, 4),
                              axis.args = list(at = 1:4, labels = strata),
                              smallplot = c(0.8, 0.84, 0.2, 0.8)
),
add_polys = list(x = ices, lwd = 1),
xlim = xlim_ire, ylim = ylim_ire
)

## Label Galway
points(-9.062691, 53.270962, pch = 21, bg = "black", cex = 1.5)
text(-8.5, 53.37, "Galway")

## Addd boundary box (for leg one)
add_boundary_box(boundaries[1:4], border = "darkblue", lwd = 1.5)

## Add ICES labels (manually):
text(x = c(-8.674607, -11.195115, -11.083916, -6.598894, -5.412772),
     y = c(56.17305, 52.75777, 51.62530, 51.60364, 52.4313),
     labels = c("VIa", "VIIb", "VIIj", "VIIg", "VIIa"),
     font = 2
)

## Add scalebar and north arrow
arrows(-11.5, y0 = 56.5, y1 = 57.2, length = 0.1, lwd = 2)
raster::scalebar(d = 200,
                 label = "200 km",
                 xy = c(-8.5, 51.05),
                 lonlat = TRUE)

## Add titles
mtext(side = 1, expression("Longitude (" * degree * ")"), cex = 1.25, line = 1.5)
mtext(side = 2, expression("Latitude (" * degree * ")"), cex = 1.25, line = -3)
mtext(side = 4, "Depth strata", cex = 1.25, line = 0)
dev.off()


################################
################################
#### Map stations

#### Map stations (for leg 1: all off Northern Ireland)
## Set up map
png("./fig/map_stations.png",
    height = 10, width = 10, units = "in", res = 600)
pp <- par(oma = c(2, 2, 2, 4))

## Define stations
add_stations_type <- 2L
if(add_stations_type == 1L){
  add_stations <- list(list(x = stations$fldShotLonDecimalDegrees,
                            y = stations$fldShotLatDecimalDegrees),
                       list(x = stations$fldHaulLonDecimalDegrees,
                            y = stations$fldHaulLatDecimalDegrees)
                       )
} else if(add_stations_type== 2L){
  add_stations <- list(x = stations$mid_lon, y = stations$mid_lat, cex = 1.5)
} else {
  add_stations <- NULL
}

## Define base map
axis_ls <- pretty_map(add_rasters = list(x = bathy,
                                         zlim = bathy_col_param$zlim,
                                         col = bathy_col_param$col),
                      add_polys = list(x = coast, col = "grey"),
                      add_points = add_stations,
                      crop_spatial = TRUE,
                      xlim = boundaries[1:2], ylim = boundaries[3:4]
)

## Add stations
# Use arrows to mark movement from shooting to hauling locations
arrows(x0 = stations$fldShotLonDecimalDegrees, y0 = stations$fldShotLatDecimalDegrees,
       x1 = stations$fldHaulLonDecimalDegrees, y1 = stations$fldHaulLatDecimalDegrees,
       col = "red",
       length = 0.02)
# Add CTD sampling stations
points(ctd_xy$lon, ctd_xy$lat, pch = 18, col = "springgreen4", bg = "springgreen4", cex = 2.25)

## Label sampling stations
text(-10.65, 54.45, "CTD", font = 2)
text(-8.775, 56.525, "Trawl", font = 2)

## Add scalebar and north arrow
arrows(-10.5, y0 = 56, y1 = 56.8, length = 0.1, lwd = 2)
# flapper::dist_btw_clicks(longlat = TRUE)
raster::scalebar(d = 100,
                 label = "100 km",
                 xy = c(-9, 53.1),
                 lonlat = TRUE)

## Add inset with area enclosed
TeachingDemos::subplot({
  px <- par(new = TRUE)
  trans <- 0.5
  xlim_inset <- c(-12, -4)
  ylim_inset <- c(51, 57.5)
  if(!is.null(add_stations)){
    add_stations_mini     <- add_stations
    add_stations_mini$cex <- 0.5
  }
  pretty_map(add_rasters = list(x = ocean,
                                zlim = bathy_col_param$zlim,
                                col = scales::alpha(bathy_col_param$col, trans),
                                plot_method = raster::plot,
                                legend = FALSE),
             add_points = add_stations_mini,
             add_polys = list(x = isles, col = scales::alpha("lightgrey", trans)),
             xlim = xlim_inset, ylim = ylim_inset,
             crop_spatial = TRUE,
             pretty_axis_args = list(control_axis = list(lwd.ticks = 0, labels = FALSE, lwd = 2))
  )
  arrows(x0 = stations$fldShotLonDecimalDegrees, y0 = stations$fldShotLatDecimalDegrees,
         x1 = stations$fldHaulLonDecimalDegrees, y1 = stations$fldHaulLatDecimalDegrees,
         col = "red",
         length = 0.01)
  points(ctd_xy$lon, ctd_xy$lat, pch = 18, col = "springgreen4", bg = "springgreen4", cex = 0.5)
  add_boundary_box(c(xlim_inset, ylim_inset), lwd = 1.5)
  add_boundary_box(boundaries[1:4], border = "darkblue", lwd = 1.5)
  par(px)
}, x = c(-6.65, -5.0), y = c(53, 54.325), pars = list(xaxs = "i", yaxs = "i")
)
# Add back axes
invisible(pretty_axis(axis_ls = axis_ls, add = TRUE))
## Add titles
mtext(side = 1, expression("Longitude (" * degree * ")"), cex = 1.25, line = 2)
mtext(side = 2, expression("Latitude (" * degree * ")"), cex = 1.25, line = -1)
mtext(side = 4, "Depth (m)", cex = 1.25, line = 3)
dev.off()


################################
################################
#### Station summaries

#### Station details
stations
stations$fldCruiseStationNumber
colnames(stations)
station_summary <-
  stations %>%
  dplyr::mutate(fldDateTimeShot = format(fldDateTimeShot, "%d-%b %H:%m"),
                fldShotLatDecimalDegrees = add_lagging_point_zero(fldShotLatDecimalDegrees, 3),
                fldShotLonDecimalDegrees = add_lagging_point_zero(fldShotLonDecimalDegrees, 3),
                fldHaulLatDecimalDegrees = add_lagging_point_zero(fldHaulLatDecimalDegrees, 3),
                fldHaulLonDecimalDegrees = add_lagging_point_zero(fldHaulLonDecimalDegrees, 3),
                ) %>%
  dplyr::select(Station              = fldCruiseStationNumber,
                Division             = area,
                Stratum              = strata,
                `Shoot (lat [o])`    = fldShotLatDecimalDegrees,
                `Shoot (lon [o])`    = fldShotLonDecimalDegrees,
                `Shoot (Depth [m])`  = fldShotDepth,
                `Shoot (Time)`       = fldDateTimeShot,
                `Haul (lat [o])`     = fldHaulLatDecimalDegrees,
                `Haul (lon [o])`     = fldHaulLonDecimalDegrees,
                `Haul (Depth [m])`   = fldHaulDepth,
                Gear                 = Gear_Type,
                `Duration [minutes]` = TowDurationMin,
                Validity             = fldValidityCode)
station_summary$Gear <- plyr::mapvalues(station_summary$Gear,
                                        from = c("Groundgear_D", "Groundgear_A"),
                                        to = c("D", "A"))
write.table(station_summary,
            file = "./fig/station_summary.txt",
            quote = FALSE, sep = ",", row.names = FALSE)

#### Basic statistics
nrow(stations) # 45
table(stations$fldValidityCode) # I - 3, V = 42
range(stations$fldDateTimeShot) #"2021-10-30 12:39:00 UTC" "2021-11-10 13:09:00 UTC"

#### A summary of stations completed by area and gear type
station_counts <-
  stations %>%
  dplyr::group_by(area, strata, Gear_Type, fldValidityCode) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::pivot_wider(names_from = fldValidityCode, values_from = n) %>%
  dplyr::select(Division = area,
                Strata = strata,
                Gear = Gear_Type,
                `Count (V)` = V,
                `Count (I)` = I,
                ) %>%
  dplyr::mutate(Strata = factor(Strata, levels = c("Coast", "Medium", "Deep", "Slope"))) %>%
  dplyr::arrange(Division, Strata)
station_counts$`Count (I)`[is.na(station_counts$`Count (I)`)] <- 0
station_counts$Gear <- plyr::mapvalues(station_counts$Gear,
                                       from = c("Groundgear_D", "Groundgear_A"),
                                       to = c("D", "A"))
write.table(station_counts,
            file = "./fig/station_counts.txt",
            quote = FALSE, sep = ",", row.names = FALSE)


################################
################################
#### Vessel conditions

#### Variables:
# wind speed
# vessel roll
# vessel heave - not collected
# vessel pitch

#### Set up figure
png("./fig/vessel_conditions.png", height = 6, width = 12, units = "in", res = 600)
pp <- par(mfrow = c(1, 3), oma = c(2, 2, 2, 2))
cex.lab  <- 1.4
cex.axis <- 1.4
adj <- -0.1
paa <- list(cex.axis = cex.axis, las = TRUE)

#### Wind speed
pretty_hist(stations$WindSpeed_Kts, xlab = "", ylab = "", control_axis = paa)
mtext(side = 1, "Wind speed (knots)", line = 2)
mtext(side = 2, "Frequency", line = 2)
mtext(side = 3, "A", font = 2, adj = adj)

#### Roll
pretty_hist(stations$Roll_deg, xlab = "", ylab = "", control_axis = paa)
mtext(side = 1, expression(paste("Roll (", degree, ")")), line = 2)
mtext(side = 3, "B", font = 2, adj = adj)

#### Pitch
pretty_hist(stations$Pitch_deg, xlab = "", ylab = "", control_axis = paa)
mtext(side = 1, expression(paste("Pitch (", degree, ")")), line = 2)
mtext(side = 3, "C", font = 2, adj = adj)
par(pp)
dev.off()


################################
################################
#### Swept area

#### Examine swept area (using summary statistics)
utils.add::basic_stats(stations$WingSweptAreaKmSq)
sum(stations$WingSweptAreaKmSq)
pretty_hist(stations$WingSweptAreaKmSq)
pretty_plot(stations$fldCruiseStationNumber, stations$WingSweptAreaKmSq)


################################
################################
#### Trawl deployments (including geometry)

#### Key variables
# tow times in relation to daylight hours
# door spread against depth
# wingspread against headline
# speed against tow distance
# mean tow depth against depth strata

#### Set up plot to save
png("./fig/trawl_properties.png",
    height = 6, width = 8, units = "in", res = 600)
pp <- par(mfrow = c(2, 3), oma = c(2, 2, 2, 2), mar = c(2, 2, 2, 2))
adj = -0.01
line.xlab <- 2.5
line.ylab <- 2.5
cex.lab  <- 1
cex.axis <- 1.4

#### Tow times in relation to daylight hours
# Define times of sunrise/sunsert
stations[, c("sunrise", "sunset")] <-
  suncalc::getSunlightTimes(data = data.frame(date = as.Date(stations$fldDateTimeShot),
                                              lat = stations$fldShotLatDecimalDegrees,
                                              lon = stations$fldShotLonDecimalDegrees))[, c("sunrise", "sunset")]

stations$fldDateTimeHaul <- stations$fldDateTimeShot + stations$TowDurationMin * 60
stations$hour_shot <- Tools4ETS::hour_dbl(stations$fldDateTimeShot)
stations$hour_haul <- Tools4ETS::hour_dbl(stations$fldDateTimeHaul)
# Define blank plot
pretty_plot(stations$fldDateTimeShot, stations$hour_shot,
            cex.axis = cex.axis, xlab = "", ylab = "",
            ylim = c(0, 24),
            type = "n")

# Shade night to sunrise
polygon(c(stations$fldDateTimeShot, rev(stations$fldDateTimeShot)),
        c(Tools4ETS::hour_dbl(stations$sunrise), rep(0, nrow(stations))),
        col = "lightgrey", border = NA)
polygon(c(stations$fldDateTimeShot, rev(stations$fldDateTimeShot)),
        c(Tools4ETS::hour_dbl(stations$sunset), rep(24, nrow(stations))),
        col = "lightgrey", border = NA)
# Add tows
lapply(split(stations, 1:nrow(stations)), function(d){
  lines(c(d$fldDateTimeShot, d$fldDateTimeShot),
        c(d$hour_shot, d$hour_haul),
        col = d$col, lwd = 2)
})
# Add the time of civil sunrise and sunset
# lines(stations$fldDateTimeShot, Tools4ETS::hour_dbl(stations$sunrise), col = "dimgrey", lty = 3)
# lines(stations$fldDateTimeShot, Tools4ETS::hour_dbl(stations$sunset), col = "dimgrey", lty = 3)

# Add titles
mtext(side = 1, "Time (months)", line = line.xlab, cex = cex.lab)
mtext(side = 2, "Time (hours)", line = line.ylab, cex = cex.lab)
mtext(side = 3, "A", font = 2, cex = cex.lab, adj = adj)

# Add legend
legend(min(stations$fldDateTimeShot), 24,
       lty = c(1, 1, NA, NA),
       pch = c(NA, NA, 22, 22),
       col = c("black", "red", "black", "black"),
       pt.bg = c(NA, NA, "white", "black"),
       legend = c("Valid", "Invalid", "Day", "Night"),
       ncol = 2,
       bty = "n")

#### Door spread against depth
pretty_plot(stations$fldShotDepth, stations$DoorSpread,
            cex.axis = cex.axis, xlab = "", ylab = "")
mtext(side = 1, "Depth (m)", line = line.xlab, cex = cex.lab)
mtext(side = 2, "Door spread (m)", line = line.ylab, cex = cex.lab)
mtext(side = 3, "B", font = 2, cex = cex.lab, adj = adj)

#### Wing spread against headline height
pretty_plot(stations$fldHeadlineHeight, stations$WingSpread,
            xlab = "", ylab = "")
mtext(side = 1, "Headline height (m)", line = line.xlab, cex = cex.lab)
mtext(side = 2, "Wing spread (m)", line = line.ylab, cex = cex.lab)
mtext(side = 3, "C", font = 2, cex = cex.lab, adj = adj)

#### Distance against speed
pretty_plot(stations$Dist_Nmi, stations$Dist_Nmi/(stations$TowDurationMin/60),
            cex.axis = cex.axis, xlab = "", ylab = "")
mtext(side = 1, "Distance (Nmi)", line = line.xlab, cex = cex.lab)
mtext(side = 2, "Speed (Nmi/hour)", line = line.ylab, cex = cex.lab)
mtext(side = 3, "D", font = 2, cex = cex.lab, adj = adj)

#### Depth against strata
pretty_plot(stations$strata, stations$depth,
            cex.axis = cex.axis, xlab = "", ylab = "")
mtext(side = 1, "Depth (m)", line = line.xlab, cex = cex.lab)
mtext(side = 2, "Time (hours)", line = line.ylab, cex = cex.lab)
mtext(side = 3, "E", font = 2, cex = cex.lab, adj = adj)

## Close figure
dev.off()


################################
################################
#### Environmental time series



#### End of code.
################################
################################


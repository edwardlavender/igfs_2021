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

#### Essential packages
library(magrittr)
library(prettyGraphics)

#### Load data
stations <- readRDS("./data/fish/stations.rds")


################################
################################
#### Map stations



################################
################################
#### Station summaries

#### Basic statistics
nrow(stations)
table(stations$fldValidityCode)
range(stations$fldDateTimeHaul)

#### A summary of stations completed by area and gear type
station_counts <-
  stations %>%
  dplyr::group_by(area, Gear_Type, fldValidityCode) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::select(Area = area,
                Gear = Gear_Type,
                Validity = fldValidityCode,
                Count = n
                )
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


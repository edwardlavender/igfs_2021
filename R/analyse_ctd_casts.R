################################
################################
#### analyse_ctd_casts

#### This code:
# 1) Analyses CTD casts from IGFS leg 1
# ... Two SBE CTD casts were deployed
# ... For one of those, the mini-CTD was also deployed for comparison.

#### Steps preceding this code
# 1) Processing of raw data via process_data_raw.R


################################
################################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(prettyGraphics)

#### Load data
sbe  <- readRDS("./data/ctd/sbe.rds")
rose <- readRDS("./data/ctd/rose.rds")
meta <- readRDS("./data/ctd/meta.rds")

#### Define up/down indices for rose data
ind_down <- rose$direction == "down"
ind_up   <- rose$direction == "up"


################################
################################
#### Summary table of CTD samples

#### Define a tidy table of CTD deployment timies/locations
meta_tidy <-
  meta %>%
  dplyr::mutate(lat = add_lagging_point_zero(plyr::round_any(lat, 0.00001), 5),
                lon = add_lagging_point_zero(plyr::round_any(lon, 0.00001), 5),
                timestamp = format(timestamp, "%d-%b %H:%M:%S")) %>%
  dplyr::select(ID = id,
                `Lon [o]` = lon,
                `Lat [o]` = lat,
                Time = timestamp
  )

#### Write table to file
write.table(meta_tidy,
            file = "./fig/ctd_meta.txt",
            quote = FALSE, sep = ",", row.names = FALSE)


################################
################################
#### Visualise CTD casts

#### Set up figure
png("./fig/ctd_profiles.png",
    height = 5, width = 8, units = "in", res = 600)
pp <- par(mfrow = c(1, 2), oma = c(1, 3, 2, 1), mar = c(2, 1, 1, 1))

#### Create a plot for each SBE cast
# (For the second SBE cast, we will also plot the comparison 'rose' dataset)
purrr::walk(1:2, function(i){

  #### Extract SBE data
  # i <- 2
  d <- split(sbe, sbe$id)[[i]]

  #### Plot the depth--temperature profile
  ## Define pretty axes
  paa <- list(side = 3:2,
              control_digits = 2)
  # For the second SBE location, pretty axes need to be defined
  # ... so that we can plot the rose data on the same plot
  if(i == 2) paa$x <- list(lim = c(d$temp, rose$temp), y = d$depth_neg)
  ## Plot depth ~ temperature for the SBE CTD
  axis_ls <- pretty_plot(d$temp, d$depth_neg,
                         pretty_axis_args = paa,
                         ylim = c(NA, 0),
                         xlab = "", ylab = "",
                         type = "l", lwd = 2
  )
  points(d$temp, d$depth_neg, pch = 21, bg = "black", cex = 0.5)
  ## Add the depth ~ temperature data for the rose CTD (if applicable)
  if(i == 2){
    lines(rose$temp[ind_down], rose$depth_neg[ind_down],
          col = "dimgrey", lty = 2)
    points(rose$temp[ind_down], rose$depth_neg[ind_down],
           pch = 21, col = "dimgrey", bg = "dimgrey", cex = 0.25)
    lines(rose$temp[ind_up], rose$depth_neg[ind_up],
          col = "grey", lty = 3)
    points(rose$temp[ind_up], rose$depth_neg[ind_up],
           col = "grey", cex = 0.25)
  }
  ## Add axes/plot title
  xlim <- axis_ls[[1]]$lim
  ylim <- axis_ls[[2]]$lim
  axis(side = 1, pos = ylim[1], lwd.ticks = 0, labels = FALSE)
  axis(side = 4, pos = xlim[2], lwd.ticks = 0, labels = FALSE)
  mtext(side = 3, LETTERS[i], font = 2, adj = 0.06, line = -2.2, cex = 1.5)

  #### Add the depth ~ salinity profile
  ## Define graphical parameters
  px <- par(new = TRUE)
  col_sal <- "blue"
  ## Define x limits, axis tick mark positions and labels appropriately
  # (For the second CTD, we will enforce 2 d.p.
  # ... for axis labels to match the first plot)
  xlim <- xat <- xla <- NULL
  if(i == 2) {
    xlim <- c(29, 31)
    xat <- pretty_seq(xlim)$at
    xla <- add_lagging_point_zero(xat, n = 2)
  }
  ## Plot depth ~ salinity for the SBE CTD
  plot(d$sal, d$depth_neg,
       xlim = xlim, ylim = axis_ls[[2]]$lim,
       axes = FALSE,
       xlab = "", ylab = "",
       col = col_sal,
       type = "l", lwd = 2)
  points(d$sal, d$depth_neg,
         pch = 21, col = col_sal, bg = col_sal,
         cex = 0.5)
  ## Add depth ~ salinity for the rose CTD (if applicable)
  if(i == 2){
    lines(rose$sal[ind_down], rose$depth_neg[ind_down],
          col = "royalblue", lty = 2)
    points(rose$sal[ind_down], rose$depth_neg[ind_down],
           pch = 21, col = "royalblue", bg = "royalblue", cex = 0.25)
    lines(rose$sal[ind_up], rose$depth_neg[ind_up],
          col = "skyblue", lty = 3)
    points(rose$sal[ind_up], rose$depth_neg[ind_up],
           col = "skyblue", cex = 0.25)

  }
  ## Add axes
  axis(side = 1, at = xat, labels = xla, pos = ylim[1])

  ## Add legend (to the second plot only for brevity)
  if(i == 2){
    legend(x = 29, y = -12.5,
           lty = c(1, 1, 1,
                   1, 1, 1),
           lwd = c(2, 1, 1,
                   2, 1, 1),
           pch = c(21, 21, 21,
                   21, 21, 21),
           col = c("black", "dimgrey", "grey",
                   "blue", "royalblue", "skyblue"),
           pt.bg = c("black", "dimgrey", NA,
                     "blue", "royalblue",NA),
           legend = c("SBE  [T]", expression("Rose [T"*""%down%""*"]"), expression("Rose [T"*""%up%""*"]"),
                      "SBE  [S]", expression("Rose [S"*""%down%""*"]"), expression("Rose [S"*""%up%""*"]")),
           bty = "n"
    )
  }
  par(px)
})

#### Add global axis labels and save
mtext(side = 1, "Salinity (PSU)", outer = TRUE, line = 0)
mtext(side = 2, "Depth (m)", outer = TRUE, line = 2)
mtext(side = 3, expression("Temperature (" * degree * "C)"), outer = TRUE, line = 0.5)
par(pp)
dev.off()


#### End of code.
################################
################################

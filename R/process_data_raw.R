################################
################################
#### process_data_raw

#### This code:
# 1) Processes raw data required for this project, including
# ... fish data
# ... vessel data
# ... ctd data
# ... spatial data

#### Steps preceding this code
# 1) Acquisition of fish data from IGFS database (via David Stokes)
# 2) Acquisition of spatial data (sourced from GEBCO and GADM)


################################
################################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(prettyGraphics)

#### Essential variables
source("./R/define_global_param.R")


################################
################################
#### Fish data

################################
#### Stations

#### Load stations
stations <- readxl::read_excel("./data-raw/fish/IGFS2021_SummaryData.xlsx",
                               sheet = "StationDataStatic")

#### Update columns
# Gear type
stations$Gear_Type <- factor(stations$Gear_Type)
# Define ICES areas and depth strata
area <- stringr::str_extract_all(stations$fldStratum, "[A-Z][a-z]*")
stations$area  <- sapply(area, function(elm) paste0(elm[1:(length(elm)-1)], collapse = ""))
stations$strata <- sapply(area, function(elm) elm[length(elm)])
# Average haul depths
stations$depth <- NA
for(i in 1:nrow(stations)) stations$depth[i] <- mean(c(stations$fldShotDepth[i], stations$fldHaulDepth))
stations$col <- c("black", "red")[factor(stations$fldValidityCode, levels = c("V", "I"))]
# Average haul locations
stations$mid_lat <- stations$mid_lon <- NA
for(i in 1:nrow(stations)){
  stations$mid_lat[i] <- mean(c(stations$fldShotLatDecimalDegrees[i], stations$fldHaulLatDecimalDegrees[i]))
  stations$mid_lon[i] <- mean(c(stations$fldShotLonDecimalDegrees[i], stations$fldHaulLonDecimalDegrees[i]))
}

#### Save stations
saveRDS(stations, "./data/fish/stations.rds")


################################
#### Species taxonomies

#### Get taxonomies from fishbase
run <- FALSE
if(run){
  fb_taxa <- rfishbase::load_taxa() # 30 s
  saveRDS(fb_taxa, "./data-raw/fish/fb_taxa.rds")
} else fb_taxa <- readRDS("./data-raw/fish/fb_taxa.rds")


################################
#### Species weights

#### Load catch data
catches <- readxl::read_excel("./data-raw/fish/IGFS2021_SummaryData.xlsx",
                          sheet = "CatchData")

#### Save catch data
saveRDS(catches, "./data/fish/catches.rds")


################################
#### Length-frequency data

#### Load biometrics
biometrics <- readxl::read_excel("./data-raw/fish/IGFS2021_SummaryData.xlsx",
                              sheet = "LngtFreq")

#### Focus on fish with length-frequency data
biometrics <- biometrics[biometrics$LngtCm > 0, ]

#### Process common names
biometrics$CommName[biometrics$CommName == "NULL"] <- NA
biometrics$CommName <- stringr::str_to_sentence(biometrics$CommName)
unique(sort(biometrics$CommName))
biometrics$CommName[biometrics$CommName == "American plaice (lr dab)"] <- "American plaice"
biometrics$CommName[biometrics$CommName ==  "Flounder (european)"] <- "European flounder"
biometrics$CommName[biometrics$CommName ==  "Hollow nosed rattail/saddled grenadier"] <- "Saddled grenadier"
biometrics$CommName[biometrics$CommName ==  "Blue skate cf d batis"] <- "Blue skate"
biometrics$CommName[biometrics$CommName == "Edible crab unsexed"] <- "Edible crab"

#### Assign taxonomic hierarchy
index <- match(biometrics$SciName, fb_taxa$Species)
biometrics$class  <- fb_taxa$Class[index]
biometrics$order  <- fb_taxa$Order[index]
biometrics$family <- fb_taxa$Family[index]
# Check that taxonomic levels have been successfully queried
# ... Not all species have been, but for subsets of species that are of interest
# ... we can manually assign these as required.
table(is.na(biometrics$class))
table(is.na(biometrics$order))
table(is.na(biometrics$family))

#### Save biometrics
saveRDS(biometrics, "./data/fish/biometrics.rds")


################################
################################
#### Vessel data



################################
################################
#### CTD data

################################
#### CTD data summary

#### Metadata_CE21013.csv
# This contains the dates/times/locations for the two deployments of the
# ... SBE CTD (termed CTD1 and CTD2).

#### cruise_data_uncal_1mbinned.csv
# This contains data from two CTD deployments
# ... listed in Metadata_CE21013.csv:
# ... ... one on 30/10/2021 off North East Ireland (in the Ocean)
# ... ... ... [named CTD1]
# ... ... one on 07/11/2021 in Loch Swilly (while sheltering from a storm)
# ... ... ... [named CTD2]
# For each deployment, only data on from the down-cast is included.

#### CTD ROSE.txt
# This contains data from one deployment of the mini CTD on 11-07-2021
# This CTD was deployed alongside the SBE CTD in Loch Swilly for
# ... comparison/validation.
# This contains data from both the down-cast and the up-cast.


################################
#### (SBE) metadata

#### Load data
meta <- readr::read_csv("./data-raw/ctd/Metadata_CE21013.csv")

#### Tidy columns
nms <- c("timestamp_deck_checks", "cruise", "id", "lat", "lon", "timestamp", "linear_time")
colnames(meta) <- nms
meta <- meta[, c("id", "lat", "lon", "timestamp")]

#### Save data
saveRDS(meta, "./data/ctd/meta.rds")


################################
#### SBE data

#### Read data
sbe <- readr::read_csv("./data-raw/ctd/cruise_data_uncal_1mbinned.csv")

#### Pull out relevant columns (CTD ID, depth, temperature, salinity)
sbe$id        <- sbe$`CTD number`
sbe$depth     <- sbe$depSM
sbe$depth_neg <- abs(sbe$depth) * -1
sbe$temp      <- sbe$t090C
sbe$sal       <- sbe$sal00

#### Select cols
sbe <- sbe[, c("id", "depth", "depth_neg", "temp", "sal")]

#### Save processed data
saveRDS(sbe, "./data/ctd/sbe.rds")


################################
#### Rose data

#### Load data
rose <- read.table("./data-raw/ctd/CTD ROSE.txt", skip = 61)
head(rose)

#### Select columns
rose <- rose[, c(1, 3, 8, 10, 11)]
colnames(rose) <- c("time", "pressure", "temp", "sal", "density")

#### Calculate depth from pressure
## depth = (pressure [dbar] * 10000)/(density [kg/m^3] * gravitational acceleration [9.80665 m/s^2])
# Convert pressure from dbar to Pa
rose$pressure <- rose$pressure * 10000
# Calculate depth
calc_depth <-
  function(pressure, density, gravity = 9.80665){
    pressure/(density * gravity)
  }
calc_depth(rose$pressure[1], 1023.26)
rose$depth     <- calc_depth(rose$pressure, rose$density)
rose$depth_neg <- abs(rose$depth) * -1

#### Focus on the depth data that corresponds to the SBE data
# (There is a lot of noise near the surface which we'll exclude)
rose <- rose[rose$depth >= 0, ]
rose <- rose[rose$depth >= min(sbe$depth), ]

#### Define a factor distinguishing the downcast and upcast
bottom <- which.max(rose$depth)
rose$direction <- factor("down", levels = c("down", "up"))
rose$direction[bottom:nrow(rose)] <- "up"

#### Visualise check for processed depth/temperature data
pretty_plot(rose$temp, rose$depth_neg,
            pretty_axis_args = list(side = 3:2),
            type = "n")
ind_down <- rose$direction == "down"
ind_up   <- rose$direction == "up"
lines(rose$temp[ind_down], rose$depth_neg[ind_down])
lines(rose$temp[ind_up], rose$depth_neg[ind_up], col = "red")

#### Save processed data
saveRDS(rose, "./data/ctd/rose.rds")


################################
################################
#### Spatial data

#### Load GADM coastline data
# This is available by country (Ireland + UK)
list.files("./data-raw/spatial/coast/")
coast_ire <- readRDS("./data-raw/spatial/coast/gadm36_IRL_0_sp.rds")
coast_gb  <- readRDS("./data-raw/spatial/coast/gadm36_GBR_0_sp.rds")

#### Visualise GADM data [SLOW!]
## Define a function to get an appropriate extent
extent_max <- function(...){
  dots <- list(...)
  extent_by_dot <- lapply(dots, function(x) raster::coordinates(raster::extent(x)))
  extents <- do.call(rbind, extent_by_dot)
  xlim <- range(extents[, 1])
  ylim <- range(extents[, 2])
  return(raster::extent(c(xlim, ylim)))
}
## Make plot
plot <- FALSE
if(plot){
  png("./fig/spatial-raw.png",
      height = 10, width = 10, units = "in", res = 300)
  prettyGraphics::pretty_map(add_polys = list(list(x = coast_ire),
                                              list(x = coast_gb)),
                             xlim = extent_max(coast_ire, coast_gb)[1:2],
                             ylim = extent_max(coast_ire, coast_gb)[3:4])
  dev.off()
}

#### Process coastline data
# Join coastline data for Ireland and UK
isles <- raster::bind(coast_ire, coast_gb)
# Crop to boundaries (for speed)
coast <- raster::crop(isles, boundaries)
# Simplify coastline data using Douglas-Peuker algorithm for plotting speed
isles_s <- rgeos::gSimplify(isles, tol = 0.005, topologyPreserve = TRUE)
raster::plot(isles_s)
coast_s <- rgeos::gSimplify(coast, tol = 0.005, topologyPreserve = TRUE)
raster::plot(coast_s)

#### Save coastline data
saveRDS(isles_s, "./data/spatial/coast/isles_s.rds")
saveRDS(coast_s, "./data/spatial/coast/coast_s.rds")
saveRDS(coast, "./data/spatial/coast/coast.rds")
rgdal::writeOGR(coast, "./data/spatial/coast/", layer = "coast",
                driver = "ESRI Shapefile", overwrite_layer = TRUE)

#### Load GEBCO bathymetry data
ocean <- raster::raster("./data-raw/spatial/bathy/GEBCO_30_Nov_2021_f5cf0a35a58c/gebco_2021_n62.30346679687501_s45.62622070312501_w-15.754394531250004_e4.8999023437499964.tif")
raster::coordinates(raster::extent(ocean))

#### Process and save bathymetry data
ocean[ocean >= 0] <- NA
ocean <- abs(ocean)
bathy <- raster::crop(ocean, boundaries)
ocean[ocean > max(bathy_col_param$zlim)] <- max(bathy_col_param$zlim)
raster::plot(bathy)
raster::writeRaster(bathy, "./data/spatial/bathy/bathy.tif", overwrite = TRUE)
raster::writeRaster(ocean, "./data/spatial/bathy/ocean.tif", overwrite = TRUE)


#### End of code.
################################
################################

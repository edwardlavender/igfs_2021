################################
################################
#### define_global_param

#### This code:
# 1) Defines global parameters required by multiple scripts.

#### Steps preceding this code
# 1) NA


################################
################################
#### Selected fish groups

#### Load raw catches data for species codes
catches <- readxl::read_excel("./data-raw/fish/IGFS2021_SummaryData.xlsx",
                              sheet = "CatchData")

#### Define selected fish groups based on Stokes et al. (2014)
commerical      <- c("HAD", "WHG", "HKE", "COD")
elasmobranchs   <- c("SDR", "THR", "CUR", "BLR", "PTR", "SHR", "SAR", "UNR", "LSD", "DGS", "SKT")
pelagics        <- c("WHB", "SPR", "HOM", "BOF", "HER", "GSS", "ARG", "MAC")
non_commercial  <- c("NOP", "POD", "GUG", "DAB", "JOD", "ESB")
commerical_flat <- c("PLE", "POK", "LBI", "MON", "SOL")

#### Focus on species caught during the survey
commerical      <- commerical[(commerical %in% catches$Species)]
elasmobranchs   <- elasmobranchs[(elasmobranchs %in% catches$Species)]
pelagics        <- pelagics[(pelagics %in% catches$Species)]
non_commercial  <- non_commercial[(non_commercial %in% catches$Species)]
commerical_flat <- commerical_flat[(commerical_flat %in% catches$Species)]

#### Examine species
commerical
elasmobranchs
pelagics
commerical_flat
non_commercial


################################
################################
#### Spatial parameters

#### area boundaries
boundaries <- raster::extent(c(-11, -5, 53, 57))

#### bathy graphical parameters
bathy_zlim <- raster::cellStats(raster::raster("./data/spatial/bathy/bathy.tif"), "range")
bathy_col_param <- pretty_cols_brewer(bathy_zlim,
                                      scheme = "Blues",
                                      n_breaks = max(bathy_zlim))


#### End of code.
################################
################################

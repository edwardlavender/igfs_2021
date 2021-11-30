################################
################################
#### define_global_param

#### This code:
# 1) Defines global parameters required by multiple scripts.

#### Steps preceding this code
# 1) NA


################################
################################
#### Define global parameters

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

# extract job
#load data
library(sp)
library(raster)
library(sf)
sp28 <- readRDS("~/projects/ml_forest-health/data/sp28.RDS")
brick_15 <- raster::brick("~/projects/ml_forest-health/data/raster/stack/files_15.tif")
sp28_xy <- sf::st_zm(sp28)
#poly_coords <- sp::coordinates(sf::as_Spatial(sp28_xy))

# extract values from raster
library(doParallel)  
no_cores <- 30
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
#getting all ndvi for all layers
ndvi_ex_15 <- foreach(i=1:nlayers(brick_15)) %dopar% raster::extract(brick_15[[i]],sp28_xy)
stopCluster(cl)
gc()

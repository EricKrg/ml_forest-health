# extract job
#load data
library(sp)
library(raster)
library(sf)
#sp28 <- readRDS("~/projects/ml_forest-health/data/sp28.RDS")
#sp28_xy <- sf::st_zm(sp28) # drop z dimension
#
sp25 <- sf::st_read("~/projects/ml_forest-health/data/sp25_gipuzkoa_v2b_datos_clase_2018_02_19.gpkg")
sp25_xy <- sf::st_zm(sp25) # drop z dimension

brick_16 <- raster::brick("~/projects/ml_forest-health/data/raster/stack/ndvi_16.tif")

poly_coords <- sp::coordinates(sf::as_Spatial(sp25_xy)) #lowcost var.

# extract values from raster

library(doParallel) 
parallel_extract <- function(raster_brick, in_geometry){
  no_cores <- 20
  cl <- makeCluster(no_cores, type="FORK")
  registerDoParallel(cl)
  #getting all ndvi for all layers
  if(class(in_geometry)[1] == "matrix"){
    ndvi<- foreach(i=1:raster::nlayers(raster_brick)) %dopar% 
      raster::extract(raster_brick[[i]],in_geometry)
  } else {
    in_geometry <- sf::st_zm(in_geometry) # drop z dimension
    ndvi<- foreach(i=1:raster::nlayers(raster_brick)) %dopar%
      raster::extract(raster_brick[[i]],in_geometry) # this takes forever
  }
  stopCluster(cl)
  gc()
  return(ndvi)
}
ndvi_16_sp25 <- parallel_extract(brick_16, poly_coords)
saveRDS(object = ndvi_16_sp25,"~/projects/ml_forest-health/data/ndvi_extract16_sp25.RDS")

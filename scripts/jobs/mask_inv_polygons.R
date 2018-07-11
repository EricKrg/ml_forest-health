# rasterize polygons on predictor-rasters

library(foreach)
library(sp)
library(raster)
library(spatial.tools)

# load inventory polygons
inv <- readRDS("data/inv_forest16.RDS")
inv_xy <- sf::st_zm(inv) # drop z dimension
inv_spatial <- sf::as_Spatial(inv_xy[, 1:4])
raster::projection(inv_spatial) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# function to mask data with polygon - in parts
parallel_mask <- function(no_cores, in_raster, in_geometry, parts){
  # Initiate cluster
  cl <- parallel::makeCluster(no_cores, type="FORK")
  doParallel::registerDoParallel(cl)
  
  # Parallelize mask function
  masked_raster <- foreach::foreach(i = 1:length(parts)) %dopar% {
    # create smaller raster for faster processing time
    temp = raster::crop(in_raster, raster::extent(in_geometry[parts[[i]], ]))
    # mask raster with part of geometry
    raster::mask(temp, in_geometry[parts[[i]], ]) 
  }
  
  # Finish
  parallel::stopCluster(cl)
  
  # Merge all raster parts
  rMerge <- do.call(merge, masked_raster)
  
  gc()
  return(rMerge)
}


# Split features into parts with 1000 polygons each, bigger numbers use to much RAM
features <- 1:length(inv_spatial)
parts <- split(features, cut(features, length(inv_spatial)/1000)) 
# set number of cores
no_cores <- 15

# create NDVI files list
predictor_rasters <- c("ndvi_max16.tif",  "ndvi_mean16.tif",  "ndvi_min16.tif", "ndvi_sum16.tif",
                       "ndvi_summer16.tif", "ndvi_winter16.tif")

# load raster and lower resolution.
# much faster to take raster with lower resolution, and use this as mask,
# instead of using high-resolution raster and polygons as mask
mask_raster <- raster::raster(paste0("data/prediction_data/", predictor_rasters[1]))
mask_raster <- raster::aggregate(mask_raster, 100)

# create masked raster
mask_raster <- parallel_mask(no_cores, mask_raster, inv_spatial, parts)

# disaggregate to input resolution
mask_raster <- raster::disaggregate(mask_raster, 100)

## adjust extent, which changes after aggregation
# remove cols
mask_raster_adj <- spatial.tools::modify_raster_margins(mask_raster, c(0, 0, -39, 0))
# get raster for extent
extent_raster <- raster::raster(paste0("data/prediction_data/", predictor_rasters[1]))
# extend the extent of mask_raster to prediction-rasters
mask_raster = raster::extend(mask_raster_adj, raster::extent(extent_raster))

# loop trough all files and mask in_raster to mask_raster
for(file in predictor_rasters){
  # get path of file
  path = paste0("data/prediction_data/", file)
  # load raster
  in_raster <- raster::raster(path)
  # mask raster
  new_raster <- raster::mask(in_raster, mask_raster)
  # write raster to disk
  writeRaster(new_raster,
              filename = paste0("data/prediction_data/masked_", file),
              overwrite = TRUE)
}
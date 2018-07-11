# Filename: 05.0.1 raster_prep.R

# TO DO:  create a layerstack to predict on

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION

#**********************************************************


# Prediction data prep.

#prepare raster
# see colnames(data_pred)
library(raster)
library(ForestHealth)
layers <- raster::brick("data/raster/stack/ndvi_16.tif")

layers <- ForestHealth::fh_raster_transform(layers, parallel = F, transform_eq = "(x - 10000) / 10000")  
raster::writeRaster(layers, "data/raster/stack/ndvi_16_transform.tif")

# load raster
#layers <- raster::brick("data/raster/stack/ndvi_16_transform.tif")

# get names 
files_16 <- list.files("data/raster/",pattern = ".*S2._2016.*")
names2016 <- stringr::str_extract(files_16, "20.{6}")
names(layers) <- paste0("ndvi_", names2016)

ndvi_sum <- sum(layers, na.rm = TRUE)
ndvi_mean <- raster::mean(layers, na.rm = TRUE)
ndvi_max <- max(layers, na.rm = TRUE)
ndvi_min <- min(layers, na.rm = TRUE)

# summer
summer <- stringr::str_which(names2016, "2016(05|06|07|08|09|10).*")
summer_layers <- layers[[summer]]
ndvi_summer <- raster::mean(summer_layers, na.rm = TRUE)

# winter
winter <- stringr::str_which(names2016, "2016(01|02|03|04|11|12).*")
winter_layers <-  layers[[winter]]
ndvi_winter <- raster::mean(winter_layers, na.rm = TRUE)

# write raster so far
raster::writeRaster(ndvi_sum, "data/prediction_data/ndvi_sum16.tif", overwrite = TRUE)
raster::writeRaster(ndvi_mean, "data/prediction_data/ndvi_mean16.tif", overwrite = TRUE)
raster::writeRaster(ndvi_summer, "data/prediction_data/ndvi_summer16.tif", overwrite = TRUE)
raster::writeRaster(ndvi_winter, "data/prediction_data/ndvi_winter16.tif", overwrite = TRUE)
raster::writeRaster(ndvi_min, "data/prediction_data/ndvi_min16.tif", overwrite = TRUE)
raster::writeRaster(ndvi_max, "data/prediction_data/ndvi_max16.tif", overwrite = TRUE)

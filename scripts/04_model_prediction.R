
# Filename: 04_model_prediction.R

# TO DO: prediction based on multiclass and binary model

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 Prediction
# 2.1 Prediction multiclass
# 2.2 Predictions binary
# 3 Visualization
#**********************************************************

pacman::p_load(mlr, mapview,sp,sf,rgdal,ggplot2)
#******************************************
# 2 Model Prediction------------------------
#******************************************
ndvi_mean <- raster::raster("data/prediction_data/masked_ndvi_mean16.tif")
ndvi_sum <- raster::raster("data/prediction_data/masked_ndvi_sum16.tif")
ndvi_summer <- raster::raster("data/prediction_data/masked_ndvi_summer16.tif")
ndvi_winter <- raster::raster("data/prediction_data/masked_ndvi_winter16.tif")
ndvi_max <- raster::raster("data/prediction_data/masked_ndvi_max16.tif")
ndvi_min <- raster::raster("data/prediction_data/masked_ndvi_min16.tif") 
#check
#mapview::mapview(ndvi_mean) 

# Data Frame out of the layers raster
layers_nd <- raster::brick(ndvi_sum,ndvi_mean,ndvi_summer,ndvi_winter, ndvi_max, ndvi_min) # layers new data - prediction base

# NewData and coords can beused for prediction of multiclass and binary
NewData <- readRDS("data/prediction_data/04_NewData.RDS")
coords_raster <- readRDS("data/prediction_data/04_coords_raster.RDS")
#NewData <- raster::as.data.frame(layers_nd)
# colnames(NewData) <- colnames(data_pred)[2:length(data_pred)]
# head(NewData)
# saveRDS(NewData, "data/prediction_data/04_NewData.RDS")
# 
# coords_raster <- data.table::as.data.table(sp::coordinates(ndvi_mean))
# saveRDS(coords_raster, "data/prediction_data/04_coords_raster.RDS")


#*******************************************************************************
#  2.1 Prediction Multi class --------------------------------------------------
#*******************************************************************************

# read in model data
rf.tuned <- readRDS("data/prediction_data/03_1_rf_tuned.RDS")
task_spatial <- readRDS("data/training_data/task_spatial.RDS")

# creates the final Model
model_multiclass <- mlr::train(rf.tuned, task_spatial)


#Predicts the values for every Pixel
pred_multiclass <- predict(model_multiclass, newdata=NewData)
saveRDS(pred_multiclass,"data/prediction_data/pred_multiclass.RDS")

# writes the result into a data frame
#pred_multiclass <- readRDS("data/prediction_data/pred_multiclass.RDS")

result_multiclass <- data.table::as.data.table(pred_multiclass$data)
result_multiclass <- readRDS("data/prediction_data/dt.result_multiclass.RDS")
saveRDS(result_multiclass,"data/prediction_data/dt.result_multiclass.RDS")

# adds the result to the dataframe
coords_raster_mc <- coords_raster

coords_raster_mc$results <-result_multiclass$response
coords_raster_mc$results[coords_raster_mc$results == "<NA>"] <- NA
# spatial info from te raster with predictions into spatial object
coordinates(coords_raster_mc) <- ~ x + y

# creates grid
gridded(coords_raster_mc) <- TRUE # sub
# coerce to raster
rasterDF <- raster::raster(coords_raster_mc)

# data sf as references crs
data_sf.xy <- st_zm(data_sf)
raster::crs(rasterDF) <- raster::crs(as(data_sf.xy, "Spatial")) #+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs 
mapview(rasterDF) + mapview(data_sf.xy, zcol = "affected")
# write the result
raster::writeRaster(rasterDF,"data/prediction_data/04_pred_mc.tif")


# visualization
map_multiclass <- raster::raster("data/prediction_data/04_pred_map_mc.tif")
ia_map <- mapview::mapview(map_multiclass) + mapview(data_sf) 

mapview::addExtent(ia_map,data = data_sf)

sf::st_bbox(data_sf)



#*******************************************************************************
# 2.2  Prediction Binary ------------------------------------------------------
#*******************************************************************************

b.NewData <- NewData[,-c(5, 6)] # remove seasons
# read in model data
b.rf.tuned <- readRDS("data/prediction_data/03_2_b_rf_tuned.RDS")
b.task_spatial_no_season <- readRDS("data/training_data/b_task_spatial_no_season.RDS")

# creates the final Model
model_bin <- mlr::train(b.rf.tuned, b.task_spatial_no_season)

#Predicts the values for every Pixel
pred_bin <- predict(model_bin, newdata=b.NewData)
saveRDS(pred_bin,"data/prediction_data/04_pred_bin.RDS")


result_bin <- data.table::as.data.table(pred_bin$data)
result_bin <- readRDS("data/prediction_data/dt.result_multiclass.RDS")
saveRDS(result_bin,"data/prediction_data/04_result_bin.RDS")

# adds the result to the dataframe
coords_raster_bin <- coords_raster

coords_raster_bin$results <-result_bin$response
coords_raster_bin$results[coords_raster_bin$results == "<NA>"] <- NA
# spatial info from te raster with predictions into spatial object
coordinates(coords_raster_bin) <- ~ x + y

# creates grid
gridded(coords_raster_bin) <- TRUE # sub
# coerce to raster
b.rasterDF <- raster::raster(coords_raster_bin)

# data sf as references crs
data_sf.xy <- st_zm(data_sf)
raster::crs(b.rasterDF) <- raster::crs(as(data_sf.xy, "Spatial")) #+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs 
mapview(b.rasterDF) + mapview(data_sf.xy, zcol = "affected")
# write the result
raster::writeRaster(b.rasterDF,"data/prediction_data/04_pred_bin.tif")



#*******************************************************************************
# 3 Visualization ------------------------------------------------------
#*******************************************************************************
map_bin <- raster::raster("data/prediction_data/04_pred_map_bin.tif")
map_mc <- raster::raster("data/prediction_data/04_pred_map_mc.tif")
ia_map <- mapview::mapview(map_bin) + mapview(data_sf) 
mapview::addExtent(ia_map,data = data_sf)

bbox_sp25 <- sf::st_as_sfc(sf::st_bbox(data_sf))


map_bin_extend <- mapview(map_bin ,na.color = "#ffffff00",legend = TRUE, map.types = c("Esri.WorldImagery"), 
        col.regions = viridisLite::viridis(2,direction = -1), layer.name = 'Classes') +
  mapview(bbox_sp25,alpha.regions = 0, alpha = 1, color = "red")
map_bin_extend
mapview::mapshot(x = map_bin_extend,file = "map.png",remove_url = T,
                 remove_controls = c("zoomControl", "layersControl", "homeButton"))


# multiclass prediction map

map_mc_extend <- mapview(map_mc, na.color = "#ffffff00", legend = TRUE, map.types = c("Esri.WorldImagery"), 
                          col.regions = viridisLite::viridis(3,alpha = 0.5,direction = -1), layer.name = 'Classes') +
  mapview(bbox_sp25,alpha.regions = 0, alpha = 1, color = "red")
map_mc_extend
mapview::mapshot(x = map_mc_extend,file = "map_mc.png",remove_url = T,
                 remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Filename: 01_data_explo.R

# TO DO: tidy data, ndvi selection

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 TIDY PTS
# 3 NDVI VARIATIONS
# 4 PLOTS
# 5 SELECTION TO RASTER
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(raster, sp, sf, mapview, rgdal, gganimate)


#load data
sp28 <- readRDS("data/sp28.RDS")
brick_15 <- raster::brick("data/raster/stack/files_15.tif")
sp28_xy <- sf::st_zm(sp28)
poly_coords <- coordinates(as_Spatial(sp28_xy))

# extract values from raster
# see extract.R in script/jobs 
# --> result

#mean of the poly values

mean_extract <-function(extract_results){
  l = 1
  j = 1
  poly_list = list()
  layer_list = list()
  
  for(k in 1:length(extract_results)){ 
    for(i in extract_results[[k]]){
      tmp <- mean(i)
      poly_list[[j]] <- tmp
      j = j + 1
    }
    layer_list[[l]] <- poly_list
    j = 1
    poly_list = list()
    l = l + 1 
  }
  return(layer_list)
}
layer_list <- mean_extract(ndvi_extract)

cov_id_15 <- gsub("TELEDETEKZIOA__S2A_", "",x = cov_id_15)
names_15 <- gsub("T.*", "",x = cov_id_15)

# function to return a formatted ndvi df
# inlist is the output of mean_extract()
# layernames is a vector with the columnnames (date stamps) of the new ndvi df

NDVI_DF <- function(inlist, layernames){
  ndvi_df <- data.frame(PID = 1:length(inlist[[1]]))
  w = 1
  for(w in 1:length(inlist)){
    ndvi_df[,paste0(layernames[w])] <- unlist(inlist[[w]])
  }
  return(ndvi_df)
}

test <-NDVI_DF(layer_list,names_15)

#PID represents corresponding polygon in spXY

#****************************************
# 4 PLOTS--------------------------------
#****************************************

p <- airquality %>%
  ggplot(aes(Solar.R,Temp, frame = Month)) + geom_jitter()
  
gganimate(p)

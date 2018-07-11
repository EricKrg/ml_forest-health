# this script holds function needed for 01_data explo



#**********************************************************
# Mask raster with polygons. limited polygons at a time to save memory----
# Input number of no_core = cores, in_raster = raster, 
# in_geometry = polyon geometry (SPDF), parts = grouping vector for parallel processing
#**********************************************************

library(foreach)
library(sp)
library(raster)
library(spatial.tools)

# load inventory polygons
inv <- readRDS("data/inv_forest16.RDS")
inv_xy <- sf::st_zm(inv) # drop z dimension
inv_spatial <- sf::as_Spatial(inv_xy[, 1:4])
raster::projection(inv_spatial) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# function to mask data with polygons - in parts
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




#**********************************************************
#
#**********************************************************

write_rowwise <- function(layer, path, name){
  # create empty brick
  b <- raster::brick(layer, values=FALSE)  
  b <- raster::writeStart(b,
                          filename= paste0(path, "/", name, ".gri"),
                          format="raster",
                          overwrite=TRUE)
  tr <- raster::blockSize(b)
  for (i in 1:tr$n) {
    v <- raster::getValuesBlock(layer , row=tr$row[i], nrows=tr$nrows[i])
    b <- raster::writeValues(b, v, tr$row[i])
  }
  b <- raster::writeStop(b)
  rm(v, b)
  gc()
}




#**********************************************************
# Extract mean NDVI value for each polygon ----
# take raster brick and polygon geometry (as SPDF) and calculate mean value
# of raster-pixels in polygon. Return data.frame
#**********************************************************

library(foreach)
library(doParallel)
# load inventory polygons
sp25 <- sf::st_read("data/sp25_gipuzkoa_v2b_datos_clase_2018_02_19.gpkg")
sp25_xy <- sf::st_zm(sp25) # drop z dimension
sp25_spatial <- sf::as_Spatial(sp25_xy)
raster::projection(sp25_spatial) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
in_geometry <- sp25_spatial
poly_coords <- sp::coordinates(sf::as_Spatial(sp25_xy)) #lowcost var.

# load ndvi-raster brick
brick_ndvi <- raster::brick("data/raster/stack/ndvi_16.tif") # from 00_get_data

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
    ndvi <- foreach::foreach(i = 1:raster::nlayers(raster_brick)) %dopar% 
      raster::extract(raster_brick[[i]], in_geometry, fun = mean, na.rm = TRUE)  # this takes forever
  }
  stopCluster(cl)
  gc()
  return(ndvi)
}

#ndvi_mean_extract <- parallel_extract(brick_ndvi, poly_coords) # 2016 took 30 minutes

## set names of layers in 2016
# load files
files_16 <- list.files("data/raster/",pattern = ".*S2._2016.*")
date <- stringr::str_extract(files_16, "20.{6}")
year <- substr(date, 1,4)
month <- substr(date, 5,6)
day <- substr(date, 7, nchar(date))
# create new name as year_month_day
new_names <- paste("ndvi", year, month, day, sep ="_")
names(ndvi_mean_extract) <- new_names
saveRDS(object = ndvi_mean_extract, "data/ndvi_mean_extract16_sp25_all_xy.RDS")



#**********************************************************
# mean of the poly values ---
#**********************************************************

#this is not needed if the poly centroids where used in the extraction
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
#layer_list <- mean_extract(ndvi_extract)


#**********************************************************
# function to return a formatted ndvi df ----
# inlist is the output of mean_extract() or result of the extract job (if poly centroids were used)
# layernames is a vector with the columnnames (date stamps) of the new ndvi dt
# this also works with extraction by poly centroids
# returns a formatted data table
#**********************************************************

NDVI_DF <- function(inlist, layernames, transform_ndvi){
  ndvi_df <- data.table::data.table(PID = 1:length(inlist[[1]]))
  w = 1
  for(w in 1:length(inlist)){
    if(transform_ndvi){
    ndvi_tmp <- (unlist(inlist[[w]])-10000)/10000 
    } else {
      ndvi_tmp <- unlist(inlist[[w]])
    }
    ndvi_df[,paste0(layernames[w])] <- ndvi_tmp
  }
  return(ndvi_df)
}


#**********************************************************
# this functions returns a vector with the colnames of all time stamps of a df
# so you could subset your df based on that vector and 
# cut out all columns which are date stamps
#**********************************************************

select_date_cols <- function(df, year){
  all_cols = NULL
  for(cols in colnames(df)){
    if(!is.na(stringr::str_match(cols, paste0(year,".*"))[1])){
      all_cols <- c(all_cols,cols) # all cols mit datum
    }
  }
  cols = NULL
  return(all_cols)
}


#**********************************************************
# Time series----
# function which creates a timeseries data table 
# returns a list with 1 - dt affected & 2 - dt unaffected
# input df needs colnames like: YYYYMMDD
# input is NDVI_DF output
#**********************************************************

create_timeseries <- function(ndvi_df){
  ndvi_aff <- ndvi_df %>%
    dplyr::filter(affected > 0)
  ndvi_no <- ndvi_df %>%
    dplyr::filter(affected == 0)
  time_df <-NULL
  i = NULL
  j = 1
  l = 1
  value = NULL
  end = length(ndvi_df)-1
  for(k in list(ndvi_aff,ndvi_no)){
    print(k) 
    for(i in k[,2:end]) { 
      print(j)
      tmp <- data.frame(value = i, affected = k$affected)
      time <- as.numeric(colnames(k)[j+1])
      year <- as.numeric(substr(time, 1,4))
      month <- as.numeric(substr(time, 5,6))
      day <- as.numeric(substr(time, 7,nchar(time)))
      tmp$time_raw <- as.numeric(colnames(k)[j+1])
      tmp$year <- year
      tmp$month <- month
      tmp$day <- day
      value[[j]] <- tmp
      j = j + 1
      tmp = NULL
    }
    time_tmp <- data.table::as.data.table(do.call(rbind, value))
    l =  l +1
    j = 1
  }
  #time_df <- data.table::as.data.table(do.call(rbind, value))
  return(time_df)
}


#**********************************************************
# !!! parallel mode not working yet !!!
# takes a layer stack/brick an transforms all layers-pixel values 
# provide a transformation eq like: transform_eq = "(x-10000) / 10000
# example: raster_transform(r_stack,transform_eq = "(x + 10) * 2")  
# it is necessary to use x to represent a raster layer in transform_eq 
# r_stack2 <- raster_transform(r_stack,parallel = T,no_cores = 4,transform_eq = "(x + 10) * 2" )
#**********************************************************

raster_transform <- function(in_layerStack, parallel, no_cores, transform_eq){
  print(paste0("using: ", transform_eq, " for transformation"))
  library(foreach)
  if(missing(parallel)){
    for(layer in 1:raster::nlayers(in_layerStack)){
      print(layer)
      x <- in_layerStack[[layer]]
      trans_tmp <-eval(parse(text=transform_eq)) # apply trans_eq
      trans_tmp[trans_tmp < 0] <- NA
      in_layerStack[[layer]] <- trans_tmp
      trans_tmp = x = NULL
    }
  } else {
    if(missing(no_cores)) no_cores = 3
    cl <- parallel::makeCluster(no_cores, type="FORK")
    doParallel::registerDoParallel(cl)
    out<- foreach::foreach(i=1:raster::nlayers(in_layerStack)) %dopar% 
      x <- in_layerStack[[i]]
    trans_tmp <- eval(parse(text=transform_eq))
    trans_tmp[trans_tmp < 0] <- NA
    in_layerStack[[i]] <- trans_tmp
    trans_tmp = x = NULL
  }
  gc()
  return(in_layerStack)
}






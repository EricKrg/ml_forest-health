# job script to stack all ndvi layers

pacman::p_load(raster, sf, rgdal)

files_15 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2015.*")
files_16 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2016.*")
files_17 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2017.*")

files_15 <- list.files("data/raster/",pattern = ".*S2._2015.*")
files_16 <- list.files("data/raster/",pattern = ".*S2._2016.*")
files_17 <- list.files("data/raster/",pattern = ".*S2._2017.*")

# Inventory geopackage for BoundingBox of study area
#inv <- readRDS("~/projects/ml_forest-health/data/inv_forest16.RDS")
inv <- readRDS("data/inv_forest16.RDS")

# set path for functions
path = "~/projects/ml_forest-health/data/raster/"
path = "data/raster/"

# Crop NDVI Rasters and return list of rasters
cutNstack <- function(filevector, extent, path){
  results <- list()
  j = 1
  # Loop over all files in filevector
  for(file in filevector) {
    print(file)
    # load rasterfile
    temp_raster <-raster::raster(paste0(path, file))
    raster::crs(temp_raster) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    # crop raster file to extent
    temp_raster_cropped <- raster::crop(temp_raster, extent)
    results[[j]] <- temp_raster_cropped 
    # reset and continue loop
    temp_raster <- NULL
    temp_raster_cropped <- NULL
    #gc()
    j = j + 1
  }
  return(results)
}

## Loop over 3 lists of filenames and run cutNstack

# bounding box of studyarea
study_extent = extent(inv)
# list for output-names
stack_list <- c("ndvi_15", "ndvi_16","ndvi_17")
u = 1
for(file_list in list(files_15, files_16, files_17)){
  print(file_list)
  # Run cutNstack
  tmp_list <- cutNstack(file_list, study_extent, path)
  # Create raster-stack from tmp_list
  tmp <- raster::stack(tmp_list)
  
  # set the date as names of layer
  # extract date from filename
  date = stringr::str_extract(file_list, "20.{6}")
  year <- substr(date, 1,4)
  month <- substr(date, 5,6)
  day <- substr(date, 7, nchar(date))
  # create new name as year_month_day
  new_names = paste("ndvi", year, month, day, sep ="_")
  names(tmp) = new_names
  
  print(tmp)
  # Write raster-stack to disk
  ## Takes to long to write Raster. Save raster as rds file instead
  #writeRaster(tmp,
  #            filename=paste0(path, "stack/", stack_list[[u]], ".tif"),
  #            options="INTERLEAVE=BAND", overwrite=TRUE)
  saveRDS(tmp, file = paste0(path, "stack/", stack_list[[u]], ".rds"))
  
  # reset and continue loop
  tmp_list <- NULL
  tmp <- NULL
  u = u + 1
}
# Filename: 00_getdata.R

# TO DO:
# What is the difference between GEOEUSKADI and TELEDETEKZIOA

#**********************************************************
# CONTENTS-------------------------------------------------
# PACKAGES AND DATA PREPERATION
# DOWNLOAD NDVI RASTERS 
# CREATE AND CROP NDVI RASTER STACKS PER YEAR
# Create aggregated rasterfiles for faster processing time in tests
# Plot Rasters
#**********************************************************

#****************************************
# PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(raster, XML, stringr, sf, rgdal, dyplr, ggplot2)

#geopckg
#sp28_sf <- sf::st_read("data/SP28_GIPUZKOA_V9b_DATOS_clase_2018_02_19.gpkg")
#forest_inv <- sf::st_read("data/INV_FORESTAL_2016_10000_ETRS89.gpkg")
#uploaded as rds 
sp28 <- readRDS("data/sp28.RDS")
inv <- readRDS("data/inv_forest16.RDS")
names(sp28)
names(inv)

#****************************************
## DOWNLOAD NDVI RASTERS-------
#****************************************

## Get all Sentinell NDVI file names
# WCS Server-URL GetCapabilities query
wcs_cap = "http://geo.hazi.eus/ows?service=WCS&version=2.0.1&request=GetCapabilities"
# Parse XML Tree
wcs_cap_xml = xmlTreeParse(wcs_cap, useInternalNodes = TRUE)
# Create XML Root 
wcs_xml_nodes = xmlRoot(wcs_cap_xml)
# Get all CoverageIds in WCS Server
wcs_coverageID <- XML::xpathSApply(wcs_xml_nodes, "//wcs:CoverageId", xmlValue)

## Get all elements with "S2", (2015|2016|2017) and "NDVI" in CoverageID 
# GEOEUSKADI
cov_id_geoeu = stringr::str_subset(wcs_coverageID, "GEOEUSKADI__S2.*(2015|2016|2017).*NDVI")
# TELEDETEKZIOA
cov_id_teledetek = stringr::str_subset(wcs_coverageID, "TELEDETEKZIOA__S2.*(2015|2016|2017).*NDVI")

# all S2 names
all_names <- c(cov_id_geoeu,cov_id_teledetek)

# creates a vector with all layer names fpr 2015-17
for(i in list("2015","2016","2017")){
  tmp <- all_names %>% 
    stringr::str_extract(pattern = paste0(".*",i,".*")) %>% 
    na.exclude()
  tmp <- gsub("TELEDETEKZIOA__S2A_", "",x = tmp)
  tmp <- gsub("GEOEUSKADI__S2A_", "",x = tmp)
  tmp <- gsub("T.*", "",x = tmp)
  tmp <- sort(tmp) # datum sortieren
  assign(paste0("names",i),tmp) # ins env
  saveRDS(tmp,paste0("data/names",i, ".RDS")) # save
}
# Server URL with CRS4326
wcs_url <- "http://geo.hazi.eus/ows?service=WCS&version=2.0.1&request=GetCoverage&crs=4326&CoverageId="


for(filename in cov_id_geoeu){
  print(filename)
  # If file has not been downloaded allready
  if(!file.exists(paste0("data/raster/", filename, ".tif"))){
    download.file(paste0(wcs_url, filename), paste0("data/raster/",filename,".tif"))
  }
}

for(filename in cov_id_teledetek){
  print(filename)
  # If file has not been downloaded allready
  if(!file.exists(paste0("data/raster/", filename, ".tif"))){
    download.file(paste0(wcs_url, filename), paste0("data/raster/",filename,".tif"))
  }
}

#****************************************
## CREATE AND CROP NDVI RASTER STACKS PER YEAR-------
#****************************************

#!!!
# this is processed in a background job
# --> see jobs/cutnstack_job.R
#!!!


# Get NDVI files list for every year
files_15 <- list.files("data/raster/",pattern = ".*S2._2015.*")
files_16 <- list.files("data/raster/",pattern = ".*S2._2016.*")
files_17 <- list.files("data/raster/",pattern = ".*S2._2017.*")

## Remove file
# Extents do not overlap for this file
remove_file = c("TELEDETEKZIOA__S2B_20170716T111603451Z_NDVI.tif",
                "TELEDETEKZIOA__S2B_20170713T110523761Z_NDVI.tif")

files_17 = files_17[-which(files_17 %in% remove_file)]

# Inventory geopackage for BoundingBox of study area
inv <- readRDS("data/inv_forest16.RDS")
names(inv)

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
# Path with raster files
path = "data/raster/"
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



#****************************************
##  Create aggregated rasterfiles for faster processing time in tests-------
#****************************************

ndvi_17 = readRDS("data/raster/stack/ndvi_17.rds")
a = Sys.time()
ndvi_dis_10 = raster::aggregate(ndvi_17, 10, filename = "data/raster/stack/ndvi_17_agg10.rds")
ndvi_dis_100 = raster::aggregate(ndvi_17, 100, filename = "data/raster/stack/ndvi_17_agg100.rds")
Sys.time() - a

ndvi_16 = readRDS("data/raster/stack/ndvi_16.rds")
a = Sys.time()
ndvi_dis_10 = raster::aggregate(ndvi_16, 10, filename = "data/raster/stack/ndvi_16_agg10.rds")
ndvi_dis_100 = raster::aggregate(ndvi_16, 100, filename = "data/raster/stack/ndvi_16_agg100.rds")
Sys.time() - a

ndvi_15 = readRDS("data/raster/stack/ndvi_15.rds")
a = Sys.time()
ndvi_dis_10 = raster::aggregate(ndvi_15, 10, filename = "data/raster/stack/ndvi_15_agg10.rds")
ndvi_dis_100 = raster::aggregate(ndvi_15, 100, filename = "data/raster/stack/ndvi_15_agg100.rds")
Sys.time() - a


#****************************************
##  Plot Rasters-------
#****************************************

raster_files <- list.files("data/raster/",pattern = ".*S2._*")

## Remove files
# Extents of studyarea do not overlap for this files
remove_file = c("TELEDETEKZIOA__S2B_20170716T111603451Z_NDVI.tif",
                "TELEDETEKZIOA__S2B_20170713T110523761Z_NDVI.tif")

raster_files = raster_files[-which(raster_files %in% remove_file)]

## Dates of rasters
date = stringr::str_extract(raster_files, "20.{6}")
year <- substr(date, 1,4)
month <- substr(date, 5,6)
day <- substr(date, 7, nchar(date))
# create new name as year_month_day
dates = paste("ndvi", year, month, day, sep ="_")
sort(dates)

## Overview of raster-dates per year
ggplot()+
  geom_point(aes(y = as.numeric(day),
                x= as.numeric(month),
                colour = as.factor(year)),
                size = 3,
                position = position_jitter(w = 0, h = 0.2)) +
  scale_x_continuous(breaks = 1:12) + scale_y_continuous(breaks = 1:31)


## Create PNG for each raster
for(file in raster_files){
  # Load raster
  raster = raster(paste0("data/raster/", file))
  # Create filename
  date = stringr::str_extract(file, "20.{6}")
  year <- substr(date, 1,4)
  month <- substr(date, 5,6)
  day <- substr(date, 7, nchar(date))
  # create new name as year_month_day
  new_name = paste("ndvi", year, month, day, sep ="_")
  # create new png file
  png(filename = paste0("data/raster/png/", new_name, ".png"), width = 833, height = 640)
  plot(raster, main = new_name)
  dev.off()
}





# job script to stack all ndvi layers

files_15 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2015.*")
files_16 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2016.*")
files_17 <- list.files("~/projects/ml_forest-health/data/raster/",pattern = ".*S2._2017.*")

inv <- readRDS("~/projects/ml_forest-health/data/inv_forest16.RDS")
#inv_wgs <- sf::st_transform(inv, 4326)

cutNstack <- function(filevector, bbox, path){
  results <- list()
  j = 1
  for(o in 1:length(filevector)) {
    print(o)
    r <-raster::raster(paste0(path, filevector[o])) 
    raster::crs(r) <- "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
    rc <- raster::crop(r, inv)
    results[[j]] <- rc 
    j = j + 1
    r <- NULL
    rc <- NULL
    gc()
  }
  return(results)
}
stack_list <- c("files_15", "files_16","files_17")
u = 1
for(k in list(files_15, files_16,files_17)){
  print(k)
  tmp_list <- cutNstack(k,inv, "~/projects/ml_forest-health/data/raster/")
  tmp <- raster::stack(tmp_list,quick = T)
  print(tmp)
  writeRaster(tmp, 
              filename=paste0("~/projects/ml_forest-health/data/raster/stack/", stack_list[[u]], ".tif"),
              options="INTERLEAVE=BAND", overwrite=TRUE) #save
  u = u + 1
}

#writeRaster(data_stack, filename="data/raster/stack/stack_15.tif", options="INTERLEAVE=BAND", overwrite=TRUE) #save 2015
#stack("<stack.tif>") to reload

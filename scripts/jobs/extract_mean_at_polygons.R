# extract job
library(foreach)
library(doParallel)
library(ForestHealth)
# load inventory polygons
sp25 <- sf::st_read("data/sp25_gipuzkoa_v2b_datos_clase_2018_02_19.gpkg")
sp25_xy <- sf::st_zm(sp25) # drop z dimension
sp25_spatial <- sf::as_Spatial(sp25_xy)
raster::projection(sp25_spatial) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
in_geometry <- sp25_spatial
poly_coords <- sp::coordinates(sf::as_Spatial(sp25_xy)) #lowcost var.

# load ndvi-raster brick
brick_ndvi <- raster::brick("data/raster/stack/ndvi_16.tif") # from 00_get_data


ndvi_mean_extract <- ForestHealth::fh_parallel_extract(brick_ndvi, poly_coords,
                                                       no_cores = 20) # 2016 took 30 minutes

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






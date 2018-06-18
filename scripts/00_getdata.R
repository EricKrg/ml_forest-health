# Filename: 00_getdata.R

# TO DO: get data

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(raster, sp, sf, mapview, rgdal, gdalUtils)

#geopckg
sp28_sf <- sf::st_read("data/SP28_GIPUZKOA_V9b_DATOS_clase_2018_02_19.gpkg")

forest_inv <- sf::st_read("data/INV_FORESTAL_2016_10000_ETRS89.gpkg")

#raster
dsn = "http://geo.hazi.eus/ows?service=WCS&version=2.0.1&request=GetCapabilities"

rgdal::ogrListLayers(dsn)
gdalUtils::ogrinfo(dsn, so=TRUE)


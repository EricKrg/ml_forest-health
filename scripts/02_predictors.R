# Filename: 02_extract_predictors.R

# TO DO: Extracting all usable predictors 

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 Raster Stack
# 3 CREATE SAMPLES
# 4 FIGURES
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(raster, sp, sf, mapview, rgdal)

### Locate our workspace (Where our rasters are stored)
setwd("./data/")

# Our variables will be:
# NDVI variations -> to be decided 
# 
pts <- # geopck smth.

#ndvi variations

ndvi_xy <- raster("./data/raster/")
  
#****************************************
# 2 Raster Stack------------------------
#****************************************

# stack:
layers <- stack()

crs(layers) <- crs(pts)

#check
mapview(layers)



#**********************************************************
# 3. Create Samples------------------------------
#**********************************************************

### prepare pts data
# name if needed
colnames(pts)[XXXX] <- "x"
colnames(pts)[XXXX] <- "y"

# make three samples 1 - sick, 2 - a bit sick, 3 - healthy 
#samples as df
smp.ill <- subset(pts, )
smp.half_ill <- subset(pts,)
smp.healthy <- subset(pts,)

smp.list <- list(smp.ill, smp.half_ill,smp.healthy)
class <- c("ill","half_ill", "healthy")
### Sample from grids (layers)
### each class extracts sample from layer stack --> check if coords are matching
j <- 1
df.list <- list()
for(i in smp.list){
  tmp <- as.data.frame(raster::extract(layers, i, sp = TRUE))
  tmp$class <- class[j]
  df.list[[j]] <- assign(paste0("df", i),tmp)
  j = j + 1
}
df.all <- do.call(rbind, df.list)
# Add xy values
### this has to be check before impl.
df.all$x <- pts$x
df.all$y <- pts$y

#check
mapview::mapview(df.all)

#check data types 

#save 
saveRDS(df.all,file = "~/data/XXXXXX")

#***************************************
# 4 Figures-------
#***************************************
# create some spineplots & histograms
par(mfrow=c(2,5))
with(chile, {
  spineplot(landslide ~slope_c, ylevels = c("TRUE", "FALSE"), xlab ="Slope angle",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~curvature_c, ylevels = c("TRUE", "FALSE"), xlab ="Curvature",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~road_dis, ylevels = c("TRUE", "FALSE"), xlab ="Distance to road",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~fallas_dis, ylevels = c("TRUE", "FALSE"), xlab ="Distance to fallas",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~eastwest, ylevels = c("TRUE", "FALSE"), xlab ="eastwest",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~northsouth, ylevels = c("TRUE", "FALSE"), xlab ="northsouth",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~tri, ylevels = c("TRUE", "FALSE"), xlab ="TRI",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~twi_c, ylevels = c("TRUE", "FALSE"), xlab ="TWI",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~catch_area, ylevels = c("TRUE", "FALSE"), xlab ="Catchment Area",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~dem_c, ylevels = c("TRUE", "FALSE"), xlab ="DEM",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
})
par(mfrow=c(1,2))
with(chile, {
  spineplot(landslide ~geo_ambiente, ylevels = c("TRUE", "FALSE"), xlab ="AMBIENTE",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~geo_unidad, ylevels = c("TRUE", "FALSE"), xlab ="UNIDAD",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
})

par(mfrow=c(1,2))
with(chile, {
  spineplot(landslide ~catch_area, ylevels = c("TRUE", "FALSE"), xlab ="Catchment Area",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
  spineplot(landslide ~catch_slope, ylevels = c("TRUE", "FALSE"), xlab ="Catchment Slope",
            ylab ="", yaxlabels = c("Landsli.","No landsli."))
})


crs(layers)
crs(as(chile.geo, "Spatial"))
mapview(chile.geo, zcol ="landslide", layer.name = "landslide", burst = T,
        col.regions = c("black","red")) + mapview(layers)

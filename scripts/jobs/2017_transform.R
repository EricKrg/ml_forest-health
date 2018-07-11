# transform ndvi data to 0-1 range

layers <- raster::brick("data/raster/stack/ndvi_17.gri")

# transform values
for(i in 1:raster::nlayers(layers)){
  print(i)
  temp_vals = (raster::values(layers[[i]])-10000) / 10000
  raster::values(layers[[i]]) <- temp_vals
}
rm(temp_vals)

# write to disk rowwise
# create empty brick
b <- raster::brick(layers, values=FALSE)  
b <- raster::writeStart(b,
                        filename= "data/raster/stack/ndvi_17_transform.gri",
                        format="raster",
                        overwrite=TRUE)
tr <- raster::blockSize(b)
for (i in 1:tr$n) {
  v <- raster::getValuesBlock(layers , row=tr$row[i], nrows=tr$nrows[i])
  b <- raster::writeValues(b, v, tr$row[i])
}
b <- raster::writeStop(b)
rm(v, b)
gc()

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









#****************************************
# 4 PLOTS--------------------------------
#****************************************

p <- airquality %>%
  ggplot(aes(Solar.R,Temp, frame = Month)) + geom_jitter()
  
gganimate(p)

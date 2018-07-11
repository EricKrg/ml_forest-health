# Filename: 01_2_data_explo_sp25.R

# TO DO: tidy data, ndvi selection --> only data prep.

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 TIDY Data
# 3 NDVI VARIATIONS
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(tidyverse, raster, sp, sf, mapview, rgdal)

devtools::install_github("EricKrg/ForestHealth")
library(ForestHealth) # noice 

#load data
sp25 = sf::st_read("data/sp25_gipuzkoa_v2b_datos_clase_2018_02_19.gpkg")
### 

# extract values from raster
# see extract.R in script/jobs 
# --> result

#ndvi_16_sp25 = readRDS("data/ndvi_mean_extract16_sp25.RDS") # new data, with mean polygon ndvi
ndvi_16_sp25 = readRDS("data/ndvi_mean_extract16_sp25_all_xy.RDS") # new data, with xy ndvi
names2016 = stringr::str_replace_all(names(ndvi_16_sp25), "[^0-9]", "") # we need the names as colnames in the following fun

# create ndvi_df
# imported fun 
ndvi_16.DF = ForestHealth::fh_NDVI_DF(ndvi_16_sp25, names2016, transform_ndvi = TRUE)
## Here we have to cheat a bit, because the mean_extract has only the 1410 datapoints
# get subset of sp25 from training data
# ndvi_16_sp25_train = readRDS("data/training_data/ndvi_16_sp25.RDS")
# sp25 = sp25[ndvi_16_sp25_train$PID, ]


#add sp columns-----------------------------------------------------------------
ndvi_16.DF$affected = sp25$Nivel_infe # add affected state
ndvi_16.DF$E1= sp25$E1 # add 
ndvi_16.DF$altitude = sp25$M_ALTITUD # ad alltitude
ndvi_16.DF$tesela = sp25$sup_tesela # add tesela
ndvi_16.DF$geom = sp25$geom # ad geom
#!! ndvi_16.df holds the raw ndvi data  (2016) for each polygon, not cleaned or filtered!


#*******************************************************************************
# 2 Tidy data ------------ -----------------------------------------------------
#*******************************************************************************

## CLEAN NDVI 2016--------------------------------------------------------------
# delete 20161230 identical with 20161213 - sp25
ndvi_16.DF$`20161230` = NULL

ndvi_16.DF$affected_factor[ndvi_16.DF$affected > "0"] = TRUE
ndvi_16.DF$affected_factor[ndvi_16.DF$affected == "0"] = FALSE

## count affected - unaffected--------------------------------------------------

# sp25
# affected
nrow(subset(ndvi_16.DF, affected_factor == T)) # 705 
# detail
nrow(subset(ndvi_16.DF, affected == 1)) # 495
nrow(subset(ndvi_16.DF, affected == 2)) # 210
# unaffected
nrow(subset(ndvi_16.DF, affected_factor == F)) # 9000

# why selection-----------------------------------------------------------------
# For the best results, it is probably best to have relatively equal numbers of examples for each class.
# 
# However, with a lot of problems, this sometimes isn't something thats easily attainable.
# Fraud detection is a prime example. Most credit card transactions are going to be valid 
# (you'll have a lot of positive examples) but there might be only a couple of fraudulent 
# transactions (negative examples).
# 
# Ways to deal with this could include dataset augmentation or synthetic sampling 
# to get more negative examples or maybe reducing the number of positive examples.


#old filter NA and - 1 filter --------------------------------------------------
# filter out col. with -1
# filter out rows with -1 
# ndvi_16.DF = as.data.frame(ndvi_16.DF)
# for(cols in all_cols){
#   del_row = which(is.na(ndvi_16.DF[,cols]))
#   if(length(del_row) > 6000){
#     ndvi_16.DF = ndvi_16.DF[,-grep(cols,colnames(ndvi_16.DF))] 
#   } 
#   else if(!is_empty(del_row)){
#     ndvi_16.DF = ndvi_16.DF[-c(del_row),]
#   }
# }
# all_cols = select_date_cols(ndvi_16.DF, "2016")
# cols = NULL
# 
# for(cols in all_cols){
#   del_row = which(ndvi_16.DF[,cols] == -1)
#   if(length(del_row) > 2000){
#     ndvi_16.DF = ndvi_16.DF[,-grep(cols,colnames(ndvi_16.DF))] 
#   } 
#   else if(!is_empty(del_row)){
#     ndvi_16.DF = ndvi_16.DF[-c(del_row),]
#    
#   }
# }

# new method - set NA and ignore na for stats. parameters-----------------------
ndvi_16.DF = as.data.frame(ndvi_16.DF)
for(cols in all_cols){
  ndvi_16.DF[,cols][ndvi_16.DF[,cols] < 0] = NA
}
ndvi_16.DF = data.table::as.data.table(ndvi_16.DF)

#*******************************************************************************
# 3 NDVI Stats. parameters -----------------------------------------------------
#*******************************************************************************
#
all_cols = ForestHealth::fh_date_cols(ndvi_16.DF, "2016")
print(all_cols)
#
ndvi_16.DF$ndvi_sum = rowSums(as.data.frame(ndvi_16.DF)[all_cols], na.rm = TRUE)
ndvi_16.DF$ndvi_mean = rowMeans(as.data.frame(ndvi_16.DF)[all_cols], na.rm = TRUE)
ndvi_16.DF$ndvi_min = ndvi_16.DF %>% 
  as.data.frame() %>%
  dplyr::select(all_cols) %>%
  apply(1, FUN=min,na.rm = T)

ndvi_16.DF$ndvi_max =ndvi_16.DF %>% 
  as.data.frame() %>%
  dplyr::select(all_cols) %>%
  apply(1, FUN=max,na.rm = T)
# summer
summer = stringr::str_which(colnames(ndvi_16.DF),"2016(05|06|07|08|09|10).*")
ndvi_16.DF$ndvi_summer = rowMeans(as.data.frame(ndvi_16.DF)[, summer], na.rm = TRUE)

# winter
winter = stringr::str_which(colnames(ndvi_16.DF),"2016(01|02|03|04|11|12).*")
ndvi_16.DF$ndvi_winter = rowMeans(as.data.frame(ndvi_16.DF)[, winter], na.rm = TRUE)

# selection --------------------------------------------------------------------

ndvi.af = subset(ndvi_16.DF, affected_factor == T) # 705 
ndvi.nonaf = subset(ndvi_16.DF, affected_factor == F) # 9000

set.seed(1234)
ndvi.smp = ndvi.nonaf[sample(nrow(ndvi.nonaf), nrow(ndvi.af)), ] # make a random sample

# bind it to new df

ndvi_16.DF = rbind(ndvi.af, ndvi.smp)

#check
ndvi_16.DF %>%
  dplyr::filter(ndvi_summer < 0)

# save NDVI --------------------------------------------------------------------
#save NDVI df
# !!! BACKUP before overwirte !!! #
saveRDS(ndvi_16.DF, "data/training_data/ndvi_16_sp25_xy.RDS") #this is the trainging data for models built on sp25






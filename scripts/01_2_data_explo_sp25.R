# Filename: 01_2_data_explo_sp25.R

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
pacman::p_load(tidyverse, raster, sp, sf, mapview, rgdal, gganimate)
source("scripts/modules/01_data_explo_functions.R") # import own data explo funs

#load data
sp25 <- sf::st_read("data/sp25_gipuzkoa_v2b_datos_clase_2018_02_19.gpkg")
### 

# extract values from raster
# see extract.R in script/jobs 
# --> result

# names from 00_getdata

ndvi_16_sp25 <- readRDS("data/ndvi_extract16_sp25.RDS")
names2016 <- readRDS("data/names2016.RDS")

# create ndvi_df
# imported fun 
ndvi_16.DF<-NDVI_DF(ndvi_16_sp25,names2016) # for sp25


#add sp columns-----------------------------------------------------------------
ndvi_16.DF$affected <- sp25$Nivel_infe # add affected state
ndvi_16.DF$E1<- sp25$E1 # add 
ndvi_16.DF$altitude <- sp25$M_ALTITUD # ad affected state
ndvi_16.DF$tesela <- sp25$sup_tesela # ad affected state
ndvi_16.DF$geom <- sp25$geom # ad affected state
#!! ndvi_16.df holds the raw ndvi data  (2016) for each polygon, not cleaned or filtered!

## CLEAN NDVI 2016--------------------------------------------------------------
# delete 20161230 identical with 20161213 - sp25
ndvi_16.DF$`20161230` = NULL

ndvi_16.DF$affected_factor[ndvi_16.DF$affected > "0"] <- TRUE
ndvi_16.DF$affected_factor[ndvi_16.DF$affected == "0"] <- FALSE

## count affected - unaffected--------------------------------------------------

# sp25
# affected
nrow(subset(ndvi_16.DF, affected_factor == T)) # 705 
# detail
nrow(subset(ndvi_16.DF, affected == 1)) # 495
nrow(subset(ndvi_16.DF, affected == 2)) # 210
# unaffected
nrow(subset(ndvi_16.DF, affected_factor == F)) # 9000

# For the best results, it is probably best to have relatively equal numbers of examples for each class.
# 
# However, with a lot of problems, this sometimes isn't something thats easily attainable.
# Fraud detection is a prime example. Most credit card transactions are going to be valid 
# (you'll have a lot of positive examples) but there might be only a couple of fraudulent 
# transactions (negative examples).
# 
# Ways to deal with this could include dataset augmentation or synthetic sampling 
# to get more negative examples or maybe reducing the number of positive examples.


#-------------------------------------------------------------------------------

# imported fun
all_cols <- select_date_cols(ndvi_16.DF, "2016")

# filter out col. with -1
# filter out rows with -1 
ndvi_16.DF <- as.data.frame(ndvi_16.DF)
for(cols in all_cols){
  del_row <- which(is.na(ndvi_16.DF[,cols]))
  if(length(del_row) > 6000){
    ndvi_16.DF <- ndvi_16.DF[,-grep(cols,colnames(ndvi_16.DF))] 
  } 
  else if(!is_empty(del_row)){
    ndvi_16.DF <- ndvi_16.DF[-c(del_row),]
  }
}
all_cols <- select_date_cols(ndvi_16.DF, "2016")
cols = NULL

for(cols in all_cols){
  del_row <- which(ndvi_16.DF[,cols] == -1)
  if(length(del_row) > 6000){
    ndvi_16.DF <- ndvi_16.DF[,-grep(cols,colnames(ndvi_16.DF))] 
  } 
  else if(!is_empty(del_row)){
   ndvi_16.DF <- ndvi_16.DF[-c(del_row),]
  }
}

ndvi_16.DF <- data.table::as.data.table(ndvi_16.DF)

# imported fun
all_cols <- select_date_cols(ndvi_16.DF, "2016") # again because zero cols were removed

ndvi_16.DF$ndvi_sum <- rowSums(as.data.frame(ndvi_16.DF)[all_cols])
ndvi_16.DF$ndvi_mean <- rowMeans(as.data.frame(ndvi_16.DF)[all_cols])
# summer
summer <- na.omit(stringr::str_extract(colnames(ndvi_16.DF),"2016(05|06|07|08).*"))
ndvi_16.DF$ndvi_summer <- ndvi_16.DF %>%
  dplyr::select(summer)  %>%
  rowMeans()

# winter
winter <- na.omit(stringr::str_extract(colnames(ndvi_16.DF),"2016(01|02|03|04|09|10|11|12).*"))
ndvi_16.DF$ndvi_winter <- ndvi_16.DF %>%
  dplyr::select(winter)  %>%
  rowMeans()

#quantiles

temp_select <- ndvi_16.DF %>%
  dplyr::select(all_cols) 
# .25 0.5 0.75
value = c()
quan_list = list()
for(quan in c(0.25, 0.5, 0.75)){
  for(row in 1:nrow(temp_select)){
    tmp <- as.numeric(quantile(temp_select[row,], probs = quan))
    value[row] <- tmp
  }
  ndvi_16.DF[,paste0("ndvi",quan)] <- value
  value = NULL
}

# selection --------------------------------------------------------------------

ndvi.af <- subset(ndvi_16.DF, affected_factor == T) # 705 
ndvi.nonaf <- subset(ndvi_16.DF, affected_factor == F) # 9000

set.seed(1234)
ndvi.smp <- ndvi.nonaf[sample(nrow(ndvi.nonaf), nrow(ndvi.af)), ] # make a random sample

# bind it to new df

ndvi_16.DF <- rbind(ndvi.af, ndvi.smp)

ndvi_16.DF %>%
  dplyr::filter(ndvi_summer < 0)

# imported fun
#ndvi_16.time <- create_timeseries(ndvi_16.DF)

#PID represents corresponding polygon in spXY

#****************************************
# 4 PLOTS--------------------------------
#****************************************

library(ggplot2)
library(gganimate)
library(gapminder)

#### 2016 TEST-----------------------------------------------------------------
# time
p <- ggplot(na.omit(ndvi_16.time[[1]]), aes(value, affected_factor)) +
  geom_boxplot(alpha = 0.7) +
  transition_time(month) +  
  labs(title = 'month: {frame_time}', x = 'NDVI', y = 'affected') 
animate(p, 100, 10)

ndvi_16.time[[1]] %>%
  ggplot(aes(y = value, x = affected_factor)) + geom_boxplot()


# static
# histogram---------------------------------------------------------------------

hist_mean <- ggplot(ndvi_16.DF) + geom_histogram(aes(ndvi_mean, fill = affected_factor),position="dodge")
hist_sum <- ggplot(ndvi_16.DF) + geom_histogram(aes(ndvi_sum, fill = affected_factor),position="dodge")
hist_summer <- ggplot(ndvi_16.DF) + geom_histogram(aes(ndvi_summer, fill = affected_factor),position="dodge")
hist_winter <- ggplot(ndvi_16.DF) + geom_histogram(aes(ndvi_winter, fill = affected_factor),position="dodge")
hist_mean + hist_sum + hist_summer + hist_mean

# freq plot---------------------------------------------------------------------


mean_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_mean, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + theme(legend.position = "none") + ylim(0,400) + xlab("")


sum_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_sum, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + theme(legend.position = "none") + ylab("") + xlab("")


summer_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_summer, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + theme(legend.position = "none") + ylab("")+ xlab("") + ylim(0,400)+
  xlim(0,1.0)


winter_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_winter, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + ylab("")  + xlab("")+ ylim(0,400)

q25_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi0.25, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + ylab("")  + xlab("")+ ylim(0,400)

q5_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi0.5, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + ylab("")  + xlab("")+ ylim(0,400)

q75_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi0.75, colour = as.factor(affected))) +
  geom_freqpoly(binwidth = 0.1) + ylab("")  + xlab("")+ ylim(0,400)

q25_freq + q5_freq + q75_freq

#tiles -------------------------------------------------------------------------
mean_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_mean, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + 
  xlab("NDVI mean") + ylab("affected class") + xlim(0,1)

sum_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_sum, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) +
  xlab("NDVI sum") + ylab("")

summer_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_summer, y = affected))+ 
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") +
  xlab("NDVI summer") +  theme(axis.title.y =element_blank()) +  xlim(0,1)

winter_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_winter, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") + 
  xlab("NDVI winter") + theme(axis.title.y =element_blank()) + xlim(0,1)

#combine freq and tiles

(mean_freq  | summer_freq | winter_freq | sum_freq)/
  (mean_tile | summer_tile | winter_tile | sum_tile) + 
  patchwork::plot_layout(heights = c(2, 1)) &  theme_minimal() & theme(legend.position = "none")
  

# correlation-------------------------------------------------------------------
library(corrplot)
# look only at affected
# ndvi_16.af <- ndvi_16.DF %>%
#   dplyr::filter(affected > 0)

ndvi_16.cor <- ndvi_16.DF %>%
  dplyr::select(altitude,E1,tesela,all_cols,ndvi_mean,ndvi_sum,
                ndvi_summer,ndvi_winter,affected,affected_factor)

correlations <- cor(ndvi_16.cor)
corrplot::corrplot(correlations, method="square")


# clustering -------------------------------------------------------------------
ndvi_16.clus <- ndvi_16.DF %>%
  dplyr::select(all_cols,ndvi_mean,ndvi_sum,
                ndvi_summer,ndvi_winter)

clus <- kmeans(ndvi_16.clus, 3)
ndvi_16.DF$cluster <- clus$cluster


ggplot(ndvi_16.DF) + geom_jitter(aes(ndvi_mean, affected, color = as.factor(cluster))) # crap
# distribution------------------------------------------------------------------
#devtools::install_github("thomasp85/patchwork")
library(patchwork)

mean_p <- ggplot(data = ndvi_16.DF) +
  geom_bin2d(mapping = aes(x = ndvi_mean, y = affected)) 

sum_p <- ggplot(data = ndvi_16.DF) +
  geom_bin2d(mapping = aes(x = ndvi_sum, y = affected))

summer_p <- ggplot(data = ndvi_16.DF) +
  geom_bin2d(mapping = aes(x = ndvi_summer, y = affected))

winter_p <- ggplot(data = ndvi_16.DF) +
  geom_bin2d(mapping = aes(x = ndvi_winter, y = affected))

(mean_p | summer_p | winter_p) /
  sum_p

# scatter
j = 1
scatters = list()
for(var_y in c("E1", "altitude","tesela")){
  for(var_x in c("ndvi_mean", "ndvi_sum", "ndvi_summer", "ndvi_winter")){
    scatters[[j]] <- ggplot(data = ndvi_16.DF, aes_string(x=var_x, y=var_y)) +
      geom_jitter(aes(color = affected_factor), alpha = 0.7) + xlab(var_x) + ylab(var_y)
    j = j + 1
  }
}


scatters[[1]] + scatters[[2]] + scatters[[3]]+ scatters[[4]]+ scatters[[5]]+ scatters[[6]] +
  scatters[[7]]+ scatters[[8]]+ scatters[[9]]+ scatters[[10]]+ scatters[[11]]+ scatters[[12]]




#box_plots------------------------------------------------------------------

mean_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_mean)) + geom_boxplot() 
sum_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_sum)) + geom_boxplot() 
summer_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_summer)) + geom_boxplot()  
winter_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_winter)) + geom_boxplot() 

#patch together
(mean_box | summer_box | winter_box ) /
  sum_box 


#spine_plots------------------------------------------------------------------

par(mfrow=c(2,2))
with(ndvi_16.DF, {
  spineplot(as.factor(affected_factor) ~ ndvi_mean, ylevels = c("TRUE", "FALSE"), xlab ="NDVI mean",
            ylab ="", yaxlabels = c("Affected","Not affected"))
  spineplot(as.factor(affected_factor) ~ndvi_sum, ylevels = c("TRUE", "FALSE"), xlab ="NDVI sum",
            ylab ="", yaxlabels = c("Affected","Not affected"))
  spineplot(as.factor(affected_factor) ~ndvi_summer, ylevels = c("TRUE", "FALSE"), xlab ="NDVI summer",
            ylab ="", yaxlabels = c("Affected","Not affected"))
  spineplot(as.factor(affected_factor) ~ndvi_winter, ylevels = c("TRUE", "FALSE"), xlab ="NDVI winter",
            ylab ="", yaxlabels = c("Affected","Not affected"))
})
#map ---------------------------------------------------------------------------
library(sf)
library(mapview)
ndvi_16.sf <- st_as_sf(ndvi_16.DF) # activate geom again

ndvi_16.sf <- sf::st_simplify(ndvi_16.sf)

mapview(ndvi_16.sf, zcol = "affected", alpha = 0)






ndvi_17 <- readRDS("data/raster/stack/ndvi_17.rds")




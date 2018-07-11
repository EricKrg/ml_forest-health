# Filename: 01_3_data_vis_sp25.R

# TO DO: visualisation for eda

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 VISUALIZATION
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
pacman::p_load(sf, tidyverse, mapview, gridExtra)
#****************************************
# 2 VISUALIZATION------------------------
#****************************************
ndvi_16.DF <- readRDS("data/training_data/ndvi_16_sp25_xy.RDS")
nrow(ndvi_16.DF) == 1410 # check

library(ggplot2)
library(gganimate)
devtools::install_github("thomasp85/patchwork")
library(patchwork)

# pre-processing
all_cols <- select_date_cols(ndvi_16.DF, "2016")
ndvi_16.DF <- as.data.frame(ndvi_16.DF)




# ndvi over time----------------------------------------------------------------
ndvi_sel <- as.data.frame(ndvi_16.DF)[,all_cols]
ndvi_16.DF[, all_cols]
year_stat <- as.data.frame(colMeans(ndvi_sel,na.rm = T))
min_v = max_v = c()
for(cols in all_cols){
  tmp_min <- min(ndvi_sel[,cols], na.rm = T)
  tmp_max <- max(ndvi_sel[,cols], na.rm = T)
  min_v <- c(min_v,tmp_min)
  max_v <- c(max_v, tmp_max)
}
year_stat$min <- min_v
year_stat$max <- max_v
year_stat$date <- row.names(year_stat)

year_stat <- year_stat[-c(which(year_stat$`colMeans(ndvi_sel, na.rm = T)` == "NaN")),]
year_stat <- year_stat[-c(which(year_stat$date == "20160713")),]
colnames(year_stat)[1] <- "mean"

year_stat <- year_stat[order(year_stat$date),]

old_names <- year_stat$date
year_stat$date <- paste0(substr(old_names, 5, 6), "_", substr(old_names, 7, 8))

plot_year_stat <- year_stat %>%
  ggplot() +
  geom_point(aes(date,min), color = "lightgreen") +
  geom_line(aes(date,min, group = 2), color = "lightgreen")+
  geom_area(aes(date,min, group = 2), fill = "lightgreen", alpha = 0.4)+
  geom_point(aes(date,mean), color = "indianred") +
  geom_line(aes(date,mean, group = 1), color = "indianred")+
  geom_area(aes(date,mean, group = 1), fill = "indianred", alpha= 0.2)+
  geom_point(aes(date,max), color = "lightblue") +
  geom_line(aes(date,max, group = 3), color = "lightblue")+
  geom_area(aes(date,max, group = 3), fill = "lightblue", alpha = 0.2) +
  ylab("NDVI") + xlab("Datum") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("plor_year_stat.png", path = "figures/bericht", plot = plot_year_stat, 
       height = 10, width = 16, units = "cm", dpi = 100)




# class membership -------------------------------------------------------------
ndvi_16.DF %>%
  ggplot() + geom_bar(aes(as.factor(affected), fill = affected_factor)) +
  xlab("Affected class") 
table(ndvi_16.DF$affected)







#missingness -------------------------------------------------------------------
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  #temp_df <- temp_df[,order(colSums(temp_df))]
  temp_df <- temp_df[ ,order(names(temp_df), decreasing = TRUE)]
  # set new names as month/day
  old_names <- names(temp_df)
  new_names <- paste0(substr(old_names, 5, 6), "_", substr(old_names, 7, 8))
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) +
    scale_fill_manual(values=c("lightgrey", "black"), name="NDVI\nFehlend", labels = c("Ja", "Nein")) +
    scale_y_discrete(labels = new_names) + 
    theme_light() + ylab("Datum") + xlab("Waldbestand") + ggtitle(title)
}

plot_missing <- plot_Missing(ndvi_16.DF[ ,all_cols])

ggsave("plot_missing.png", path = "figures/bericht", plot = plot_missing, 
       height = 10, width = 16, units = "cm", dpi = 100)




#*********************************************************
# binary ----
#*********************************************************

# histogram---------------------------------------------------------------------

hist_mean <- ggplot(ndvi_16.DF) +
  geom_histogram(aes(ndvi_mean, fill = affected_factor),position="dodge") + xlab("NDVI_Mittel") +
  theme(legend.position = "none") + ylim(0,250) + xlim(0,1) + ylab("Anzahl")

hist_min <- ggplot(ndvi_16.DF) +
  geom_histogram(aes(ndvi_min, fill = affected_factor),position="dodge") + ylab("") + xlab("NDVI_Min") +
  theme(legend.position = "none") + ylim(0,250)+ xlim(0,1)

hist_max <- ggplot(ndvi_16.DF) +
  geom_histogram(aes(ndvi_max, fill = affected_factor),position="dodge") + ylab("") + xlab("NDVI_Max") +
  theme(legend.position = "none") + ylim(0,250)+ xlim(0,1)

hist_sum <- ggplot(ndvi_16.DF) +
  geom_histogram(aes(ndvi_sum, fill = affected_factor) ,position="dodge") + ylab("") + xlab("NDVI_Sum") +
  ylim(0,250) + theme(legend.position = c(0.70, 0.9)) + 
  scale_fill_discrete(name = "Betroffen", breaks = c(FALSE, TRUE), labels = c("Nein", "Ja"))

hist_summer <- ggplot(ndvi_16.DF) + 
  geom_histogram(aes(ndvi_summer, fill = affected_factor),position="dodge") + ylab("") + xlab("NDVI_Sommer") +
  theme(legend.position = "none") + ylim(0,250)+ xlim(0,1)

hist_winter <- ggplot(ndvi_16.DF) +
  geom_histogram(aes(ndvi_winter, fill = affected_factor),position="dodge")  + ylab("")+ xlab("NDVI_Winter") +
  theme(legend.position = "none")  + ylim(0,250) + xlim(0,1)

# # density-----------------------------------------------------------------------
# density_mean <- ggplot(ndvi_16.DF) +
#   geom_density(alpha = 0.2, aes(ndvi_mean, color = affected_factor) ) + 
#   theme(legend.position = "none") + ylim(0,10) +  xlim(0,1) +  xlab("")
# 
# density_min <- ggplot(ndvi_16.DF) +
#   geom_density(alpha = 0.2, aes(ndvi_min, color = affected_factor)) + ylab("") +xlab("") +
#   theme(legend.position = "none") + xlim(0,1) + ylim(0,10)
# 
# density_max <- ggplot(ndvi_16.DF) +
#   geom_density(alpha = 0.2, aes(ndvi_max, color = affected_factor)) + ylab("") +
#   theme(legend.position = "none") +  xlim(0,1)  + ylim(0,10) + xlab("")
# 
# density_sum <- ggplot(ndvi_16.DF) +
#   geom_density(alpha = 0.2, aes(ndvi_sum, color = affected_factor)) + ylab("")  +
#   ylim(0,10) + xlab("")
# 
# density_summer <- ggplot(ndvi_16.DF) + 
#   geom_density(alpha = 0.2, aes(ndvi_summer, color = affected_factor)) + ylab("") +
#   theme(legend.position = "none") + xlim(0,1)  + ylim(0,10) + xlab("")
# 
# density_winter <- ggplot(ndvi_16.DF) +
#   geom_density(alpha = 0.2, aes(ndvi_winter, color = affected_factor)) + ylab("") +
#   theme(legend.position = "none")  + xlim(0,1)  + ylim(0,10) + xlab("")

#box_plots------------------------------------------------------------------

mean_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_mean)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("NDVI") +  xlab("") + ylim(0,1.0)
min_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_min)) + 
  geom_boxplot() + 
  scale_x_discrete(labels =c("Nein", "Ja")) +
  ylab("") +  xlab("") + ylim(0,1.0)
max_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_max)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("") +  xlab("") + ylim(0,1.0)
sum_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_sum)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("") +  xlab("") + ylim(0,10)
summer_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_summer)) + 
  geom_boxplot()  + 
  scale_x_discrete(labels =c("Nein", "Ja")) +
  ylab("") +  xlab("") + ylim(0,1.0)
winter_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_winter)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("") +  xlab("") + ylim(0,1.0)

hist_box_bina <- grid.arrange(mean_box, min_box, max_box, summer_box, winter_box, sum_box,
             hist_mean, hist_min, hist_max, hist_summer, hist_winter, hist_sum,
             ncol = 6, nrow = 2, heights = c(1,2))

ggsave("hist_box_bina.png", path = "figures/bericht", plot = hist_box_bina, 
       height = 20, width = 30, units = "cm", dpi = 100)





#box_plots------------------------------------------------------------------

mean_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_mean)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("NDVI_Mittel") +  xlab("Betroffen") + ylim(0,1.0)

min_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_min)) + 
  geom_boxplot() + 
  scale_x_discrete(labels =c("Nein", "Ja")) +
  ylab("NDVI_Min") +  xlab("Betroffen") + ylim(0,1.0)

max_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_max)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("NDVI_Max") +  xlab("Betroffen") + ylim(0,1.0)

sum_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_sum)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("NDVI_Sum") +  xlab("Betroffen") + ylim(0,10)

summer_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_summer)) + 
  geom_boxplot()  + 
  scale_x_discrete(labels =c("Nein", "Ja")) +
  ylab("NDVI_Sommer") +  xlab("Betroffen") + ylim(0,1.0)

winter_box <- ggplot(ndvi_16.DF, aes(affected_factor, ndvi_winter)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nein", "Ja")) +
  ylab("NDVI_Winter") +  xlab("Betroffen") + ylim(0,1.0)


box_bina <- grid.arrange(mean_box, min_box, max_box, summer_box, winter_box, sum_box,
             ncol = 6, nrow = 1)
ggsave("box_bina.png", path = "figures/bericht", plot = box_bina, 
       height = 10, width = 30, units = "cm", dpi = 100)



#*********************************************************
# multiclass ----
#*********************************************************

# freq plot---------------------------------------------------------------------

mean_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_mean, fill = as.factor(affected))) +
  geom_histogram(position="dodge")+ ylim(0,200) + theme(legend.position = "none") + xlab("") + xlim(0,1.0) + 
  ylab("Anzahl")

min_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_min, fill = as.factor(affected))) +
  geom_histogram(position="dodge")+ ylim(0,200) + theme(legend.position = "none") + ylab("") +
  xlab("") + xlim(0,1.0)

max_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_max, fill = as.factor(affected))) +
  geom_histogram(position="dodge")+ ylim(0,200) + theme(legend.position = "none") +  ylab("") + 
  xlab("") + xlim(0,1.0) 

sum_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_sum, fill = as.factor(affected))) +
  geom_histogram(position="dodge") + scale_fill_discrete(name = "Schadklasse") + 
  ylim(0,200)+ ylab("") + xlab("")  + theme(legend.position = "none")


summer_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_summer, fill = as.factor(affected))) +
  geom_histogram(position="dodge")+ ylim(0,200) + ylab("")+ xlab("") +
  xlim(0,1.0) + theme(legend.position = "none")


winter_freq <- ggplot(data = ndvi_16.DF, mapping = aes(x = ndvi_winter, fill = as.factor(affected))) +
  geom_histogram(position="dodge")+ ylim(0,200) + ylab("")  + xlab("")+ 
  xlim(0,1.0) + theme(legend.position = "none")


#tiles -------------------------------------------------------------------------
mean_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_mean, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + 
  xlab("NDVI_Mittel") + ylab("Schadklasse") + xlim(0,1) + theme(legend.position = "none")

min_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_min, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") +
  xlab("NDVI_Min") +   xlim(0, 1.0) + theme(legend.position = "none")

max_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_max, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") +
  xlab("NDVI_Max") +  xlim(0, 1.0) + theme(legend.position = "none")

summer_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_summer, y = affected))+ 
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") +
  xlab("NDVI_Sommer") +    xlim(0,1) + theme(legend.position = "none")

winter_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_winter, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) + ylab("") + 
  xlab("NDVI_Winter") +  xlim(0,1) + theme(legend.position = "none")

sum_tile <- ggplot(ndvi_16.DF, aes(x = ndvi_sum, y = affected)) +
  geom_tile(aes(color = as.factor(affected)), alpha = 0.5) +
  xlab("NDVI_Sum") + ylab("") + theme(legend.position = "none")

#combine freq and tiles
hist_freq_mult <- grid.arrange(mean_freq, min_freq, max_freq, summer_freq, winter_freq, sum_freq,
             mean_tile, min_tile, max_tile, summer_tile, winter_tile, sum_tile, nrow = 2, ncol = 6)

ggsave("hist_freq_mult.png", path = "figures/bericht", plot = hist_freq_mult, 
       height = 20, width = 30, units = "cm", dpi = 100)




#box_plots------------------------------------------------------------------

mean_box <- ggplot(ndvi_16.DF, aes(y = ndvi_mean, x = as.factor(affected))) + 
  geom_boxplot() + 
  ylab("NDVI_Mittel") +  xlab("Schadstufe") + ylim(0,1.0)

min_box <- ggplot(ndvi_16.DF, aes(as.factor(affected), ndvi_min)) + 
  geom_boxplot() + 
  ylab("NDVI_Min") +  xlab("Schadstufe") + ylim(0,1.0)

max_box <- ggplot(ndvi_16.DF, aes(as.factor(affected), ndvi_max)) + 
  geom_boxplot() + 
  ylab("NDVI_Max") +  xlab("Schadstufe") + ylim(0,1.0)

sum_box <- ggplot(ndvi_16.DF, aes(as.factor(affected), ndvi_sum)) + 
  geom_boxplot() + 
  ylab("NDVI_Sum") +  xlab("Schadstufe") + ylim(0,10)

summer_box <- ggplot(ndvi_16.DF, aes(as.factor(affected), ndvi_summer)) + 
  geom_boxplot()  + 
  ylab("NDVI_Sommer") +  xlab("Schadstufe") + ylim(0,1.0)

winter_box <- ggplot(ndvi_16.DF, aes(as.factor(affected), ndvi_winter)) + 
  geom_boxplot() + 
  ylab("NDVI_Winter") +  xlab("Schadstufe") + ylim(0,1.0)

box_mult <- grid.arrange(mean_box, min_box, max_box, summer_box, winter_box, sum_box, nrow = 1, ncol = 6)

ggsave("box_mult.png", path = "figures/bericht", plot = box_mult, 
       height = 10, width = 30, units = "cm", dpi = 100)






# correlation-------------------------------------------------------------------
library(corrplot)
# look only at affected
# ndvi_16.af <- ndvi_16.DF %>%
#   dplyr::filter(affected > 0)

ndvi_16.cor <- ndvi_16.DF %>%
  dplyr::select(NDVI_Mittel = ndvi_mean, NDVI_Sum = ndvi_sum,
                NDVI_Sommer = ndvi_summer, NDVI_Winter = ndvi_winter,
                NDVI_Min = ndvi_min, NDVI_Max = ndvi_max,
                Schadklasse = affected, Betroffen = affected_factor)

ndvi_16.cor <- ndvi_16.cor[-c(which(ndvi_16.cor$NDVI_Winter == "NaN")),]
correlations <- cor(ndvi_16.cor, method = "spearman")

png('figures/bericht/corrplot.png', height = 600, width = 600)
corrplot::corrplot(correlations, method="square", na.label = "square", type = "upper", tl.col = "black")
dev.off()




# clustering -------------------------------------------------------------------
# ndvi_16.clus <- ndvi_16.DF %>%
#   dplyr::select(all_cols,ndvi_mean,ndvi_sum,
#                 ndvi_summer,ndvi_winter)
# 
# clus <- kmeans(ndvi_16.clus, 3)
# ndvi_16.DF$cluster <- clus$cluster
# 
# 
# ggplot(ndvi_16.DF) + geom_jitter(aes(ndvi_mean, affected, color = as.factor(cluster))) # crap
# 



# distribution------------------------------------------------------------------
# scatter
# j = 1
# scatters = list()
# for(var_y in c("E1", "altitude","tesela")){
#   for(var_x in c("ndvi_mean", "ndvi_sum", "ndvi_summer", "ndvi_winter")){
#     scatters[[j]] <- ggplot(data = ndvi_16.DF, aes_string(x=var_x, y=var_y)) +
#       geom_jitter(aes(color = affected_factor), alpha = 0.7) + xlab(var_x) + ylab(var_y)
#     j = j + 1
#   }
# }
# 
# 
# scatters[[1]] + scatters[[2]] + scatters[[3]]+ scatters[[4]]+ scatters[[5]]+ scatters[[6]] +
#   scatters[[7]]+ scatters[[8]]+ scatters[[9]]+ scatters[[10]]+ scatters[[11]]+ scatters[[12]]
# 



#spine_plots------------------------------------------------------------------

png('figures/bericht/spineplot.png', height = 600, width = 900)

par(mfrow=c(2,3), cex = 1)
with(ndvi_16.DF, {
  spineplot(as.factor(affected_factor) ~ ndvi_mean, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Mittel",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  spineplot(as.factor(affected_factor) ~ndvi_min, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Min",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  spineplot(as.factor(affected_factor) ~ndvi_max, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Max",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  spineplot(as.factor(affected_factor) ~ndvi_summer, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Sommer",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  spineplot(as.factor(affected_factor) ~ndvi_winter, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Winter",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  spineplot(as.factor(affected_factor) ~ndvi_sum, ylevels = c("TRUE", "FALSE"), xlab ="NDVI_Sum",
            ylab ="", yaxlabels = c("Betroffen","Nicht Betroffen"))
  
})

dev.off()


# 
# 
# #map ---------------------------------------------------------------------------
# library(sf)
# library(mapview)
# ndvi_16.sf <- st_as_sf(ndvi_16.DF) # activate geom again
# 
# ndvi_16_xy <-st_centroid(ndvi_16.sf)
# ndvi_16_xy$circle_size <- abs(log10(ndvi_16.DF$ndvi_mean))
# 
# mapview(ndvi_16_xy, zcol = "affected", cex = "circle_size",alpha = 0,
#                     legend = T)
# 

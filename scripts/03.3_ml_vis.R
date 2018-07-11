# Filename: 03.3_ml_vis

# visualisation of the ml validation (both models)

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES 
# 2 Visualisation Multiclass model
# 3 Visualisation binary model
#**********************************************************

pacman::p_load(mlr, mapview, sp, sf, rgdal, gridExtra, tidyverse, randomForest)
set.seed(1234)

#*******************************************************************************
# 2 Visualisation Multiclass model -----------------------------------------------
#*******************************************************************************
cv_rf <- readRDS("data/training_data/cv_rf.RDS")
spcv_rf <- readRDS("data/training_data/spcv_rf.RDS")

## UNTUNED Plot validation results----

# combine the results:
mmce_nospatial <- as.data.frame(cv_rf$measures.test$mmce)
auc_nonspatial <- as.data.frame(cv_rf$measures.test$multiclass.au1u)
# all the auroc values into one dataframe
mmce_df <- as.data.frame(spcv_rf$measures.test$mmce)
auc_df <- as.data.frame(spcv_rf$measures.test$multiclass.au1u)

performance_mmce <- data.frame(non_spatial = mmce_nospatial$`cv_rf$measures.test$mmce`,
                               spatial = mmce_df$`spcv_rf$measures.test$mmce`)
performance_auc <- data.frame(non_spatial = auc_nonspatial$`cv_rf$measures.test$multiclass.au1u`,
                              spatial = auc_df$`spcv_rf$measures.test$multiclass.au1u`)


# visualization

plot_mmce <- performance_mmce %>% gather() %>%
  ggplot(., aes(x = key, y = value))+
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nicht-Räumlich", "Räumlich")) + 
  theme_bw()+ ylim(0,1) + ylab("Mean Missclassification Error") + xlab("")

ggsave("plot_mmce.png", path = "figures/bericht", plot = plot_mmce, 
       height = 8, width = 8, units = "cm", dpi = 100)

plot_auc <- performance_auc %>% gather() %>%
  ggplot(., aes(x = key, y = value))+
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nicht-Räumlich", "Räumlich")) + 
  theme_bw()+ ylim(0,1) + ylab("AUROC") + xlab("")

ggsave("plot_auc.png", path = "figures/bericht", plot = plot_auc, 
       height = 8, width = 8, units = "cm", dpi = 100)




## TUNNNING Plot validation results----

cv_rf_tuned <- readRDS("data/training_data/cv_rf_tuned.RDS")
spcv_rf_tuned <- readRDS("data/training_data/spcv_rf_tuned.RDS")
# combine the results:
t.mmce_nospatial <- as.data.frame(cv_rf_tuned$measures.test$mmce)
t.auc_nonspatial <- as.data.frame(cv_rf_tuned$measures.test$multiclass.au1u)
# all the auroc values into one dataframe
t.mmce_df <- as.data.frame(spcv_rf_tuned$measures.test$mmce)
t.auc_df <- as.data.frame(spcv_rf_tuned$measures.test$multiclass.au1u)

t.performance_mmce <- data.frame(non_spatial = t.mmce_nospatial$`cv_rf_tuned$measures.test$mmce`,
                                 spatial = t.mmce_df$`spcv_rf_tuned$measures.test$mmce`)
t.performance_auc <- data.frame(non_spatial = t.auc_nonspatial$`cv_rf_tuned$measures.test$multiclass.au1u`,
                                spatial = t.auc_df$`spcv_rf_tuned$measures.test$multiclass.au1u`)



plot_Tmmce <- t.performance_mmce %>% gather() %>%
  ggplot(., aes(x = key, y = value))+
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nicht-Räumlich", "Räumlich")) + 
  theme_bw()+ ylim(0,1) + ylab("Mean Missclassification Error") + xlab("")

ggsave("plot_Tmmce.png", path = "figures/bericht", plot = plot_Tmmce, 
       height = 8, width = 8, units = "cm", dpi = 100)

plot_Tauc <- t.performance_auc %>% gather() %>%
  ggplot(., aes(x = key, y = value))+
  geom_boxplot() + 
  scale_x_discrete(labels = c("Nicht-Räumlich", "Räumlich")) + 
  theme_bw()+ ylim(0,1) + ylab("AUROC") + xlab("")

ggsave("plot_Tauc.png", path = "figures/bericht", plot = plot_Tauc, 
       height = 8, width = 8, units = "cm", dpi = 100)





# Variable importance-----------------------------------------------------------
rf.tuned <- readRDS("data/prediction_data/03_1_rf_tuned.RDS")
task_spatial <- readRDS("data/training_data/task_spatial.RDS")

model_multiclass <- mlr::train(rf.tuned, task_spatial)

var_imp = mlr::getFeatureImportance(model_multiclass, type = 1)  #mean decrease in accuracy
var_imp_res <- gather(var_imp[["res"]])

plot_var_imp <-
  var_imp_res %>% 
  ggplot(., aes(fct_reorder(key, value), value)) + 
  geom_bar(stat = "identity", fill = "indianred") +
  ylab("Mittlere Abnahme der Genauigkeit") + xlab("") +  
  scale_x_discrete(labels = rev(c("NDVI_Min", "NDVI_Sum", "NDVI_Winter", "NDVI_Sommer", "NDVI_Max", "NDVI_Mittel"))) +
  coord_flip()
#The loss of accuracy is the Mean Decrease in Accuracy.

ggsave("plot_var_imp.png", path = "figures/bericht", plot = plot_var_imp, 
       height = 8, width = 16, units = "cm", dpi = 100)

# lets do it oldschool
randomForest::varImpPlot(model_multiclass$learner.model)




#*******************************************************************************
# 3 Visualisation binary model -----------------------------------------------
#*******************************************************************************
b.spcv_rf <- readRDS("data/training_data/03.2_b.spcv.RDS")
## UNTUNED Plot validation results

# combine the results:
# all the auroc values into one dataframe
b.f1_df <- as.data.frame(b.spcv_rf$measures.test$f1)
b.auc_df <- as.data.frame(b.spcv_rf$measures.test$auc)

b.performance_f1 <- data.frame(spatial = f1_df$`b.spcv_rf$measures.test$f1`)
b.performance_auc <- data.frame(spatial = auc_df$`b.spcv_rf$measures.test$auc`)



# visualization
b.spatial_f1 <- ggplot(b.performance_f1, aes(x = "spatial" , y = spatial))+
  geom_boxplot()+
  theme_bw()+ ylim(0,1) + ylab("F1") + xlab("") + 
  scale_x_discrete(labels = "Räumlich") 

b.spatial_auc <- ggplot(b.performance_auc, aes(x = "spatial" , y = spatial))+
  geom_boxplot()+
  theme_bw()+ ylim(0,1) + ylab("AUROC") + xlab("")+ 
  scale_x_discrete(labels = "Räumlich") 

binary_plots <- grid.arrange(b.spatial_f1, b.spatial_auc, ncol = 2)


ggsave("binary_plots.png", path = "figures/bericht", plot = binary_plots, 
       height = 8, width = 8, units = "cm", dpi = 100)



## TUNNNING Plot validation results
b.spcv_rf_tuned <- readRDS("data/training_data/03.2_b.spcv_rf_tuned.RDS")
# combine the results:
# all the auroc values into one dataframe
bt.f1_df <- as.data.frame(b.spcv_rf_tuned$measures.test$f1)
bt.f1_df$`b.spcv_rf_tuned$measures.test$f1`[bt.f1_df$`b.spcv_rf_tuned$measures.test$f1` == 0] <- NA
bt.auc_df <- as.data.frame(b.spcv_rf_tuned$measures.test$auc)

bt.performance_f1 <- data.frame(spatial = bt.f1_df$`b.spcv_rf_tuned$measures.test$f1`)
bt.performance_auc <- data.frame(spatial = bt.auc_df$`b.spcv_rf_tuned$measures.test$auc`)

# visualization
bt.spatial_f1 <- ggplot(bt.performance_f1, aes(x = "spatial" , y = spatial))+
  geom_boxplot()+
  theme_bw()+ ylim(0,1) + ylab("F1") + xlab("")+ 
  scale_x_discrete(labels = "Räumlich") 

bt.spatial_auc <- ggplot(bt.performance_auc, aes(x = "spatial" , y = spatial))+
  geom_boxplot()+
  theme_bw()+ ylim(0,1) + ylab("AUROC") + xlab("")+ 
  scale_x_discrete(labels = "Räumlich") 

tuned_binary_plots <- grid.arrange(bt.spatial_f1, bt.spatial_auc, ncol = 2)


ggsave("tuned_binary_plots.png", path = "figures/bericht", plot = tuned_binary_plots, 
       height = 8, width = 8, units = "cm", dpi = 100)






# Variable importance-----------------------------------------------------------
b.rf.tuned <- readRDS("data/prediction_data/03_2_b_rf_tuned.RDS")
b.task_spatial_no_season <- readRDS("data/training_data/b_task_spatial_no_season.RDS")

model_bin <- mlr::train(b.rf.tuned, b.task_spatial_no_season)

b.var_imp = mlr::getFeatureImportance(model_bin, type = 1)  #mean decrease in accuracy
b.var_imp_res <- gather(b.var_imp[["res"]])

plot_b_var_imp <-
  b.var_imp_res %>% 
  ggplot(., aes(fct_reorder(key, value), value)) + 
  geom_bar(stat = "identity", fill = "indianred") +
  ylab("Mittlere Abnahme der Genauigkeit") +
  scale_x_discrete(labels = c("NDVI_Max", "NDVI_Mittel", "NDVI_Min", "NDVI_Sum")) +
  xlab("") +  
  coord_flip()

ggsave("plot_b_var_imp.png", path = "figures/bericht", plot = plot_b_var_imp, 
       height = 8, width = 16, units = "cm", dpi = 100)


# Filename: 05modelling_with_validation.R

# TO DO:  model creation, training, validation, prediction

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 Model definition
# 3 Crossvalidation
# 4 Model Prediction
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
setwd("./")
#load package
#devtools::install_github("pat-s/mlr@all-mods") # working with GAM
# we have to see which mlr version we are using 

pacman::p_load(mlr, mapview,sp,sf,rgdal,ggplot2)

set.seed(27041992)

#load data
model_data <- readRDS(".RDS")

# transform the data
#sf::st_geometry(model_data) <- NULL

#******************************************
#2 GAM defenition---------------------------
#******************************************
## Define the task:
task_spatial <- makeClassifTask(id = "tree_health_rf", data = chile,
                                target = "class", positive = "TRUE",
                                spatial =TRUE
                                #coordinates = chile[,c("x","y")] # depends on mlr version
)  #coords with the new mlr vers.

# define the learner
lrn_rf <- makeLearner("classif.rf",
                       #family = "binomial",
                       #binomial.link = "logit",
                       predict.type = "class",
                       formula = "class~...+...")


#******************************************
# 3 Crossvalidation -----------------------
#******************************************
# resampling spatial:
# resampling definition
resampling_spatial <- makeResampleDesc("SpRepCV", folds = 5, reps = 10)

spcv_rf <- mlr::resample(lrn_rf, task_spatial, resampling_spatial,
                          measures = list(auc),
                          show.info = TRUE, models = TRUE)

# resampling non spatial
# resampling definition
resampling_non_spatial <- makeResampleDesc("RepCV",  folds = 5, reps = 10)

cv_rf <- mlr::resample(lrn_gam, task_spatial, resampling_non_spatial,
                        measures = list(auc), #auroc as measure
                        show.info = TRUE, models = TRUE)

# combine the results:
auroc_nospatial <- as.data.frame(cv_rf$measures.test$auc)
# all the auroc values into one dataframe
auroc <- as.data.frame(spcv_rf$measures.test$auc)
boxplot(auroc)
awerte <- cbind(auroc,auroc_nospatial )
colnames(awerte) <- c("räuml. Kreuzvalidierung","nicht räuml. Kreuzvalidierung")
rvttm <- reshape::melt(awerte)

# visualization
ggplot(rvttm, aes(x=variable, y=value))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous("AUROC")

#******************************************
# 4 Model Prediction------------------------
#******************************************
# Data Frame out of the layers raster
NewData <- data.frame(rasterToPoints(layers))
NewData <- as.data.frame(layers,na.rm=TRUE,xy=TRUE)

# creates the final Model
model1 <- train(lrn_rf, task_spatial)
plot(model1)
#Predicts the values for every Pixel
pred <- predict(model1, newdata=NewData)

# writes the result into a data frame
result <- as.data.frame(pred$data)

# adds the result to the dataframe
NewData$probTRUE <- result$prob.TRUE

# spatial info from te raster with predictions into spatial object
spg <- NewData[,c(1,2,13)]
#str(NewData)
coordinates(spg) <- ~ x + y

# creates grid
gridded(spg) <- TRUE
# coerce to raster
rasterDF <- raster(spg)
crs(rasterDF) <- crs(area)

# write the result
writeRaster(rasterDF,"./data/raster/...")

# visualization
save.image(file = "./data/")
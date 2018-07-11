# Filename: 03.2 modelling_binary.R

# TO DO:  model creation, tuning, model preformance vis

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 Model definition
# 3 Crossvalidation
# 4 Tunning
# 5 Crossvalidation with tunning
# 6 Visualization
#**********************************************************

#****************************************
# 1 PACKAGES AND DATA PREPERATION-------
#****************************************
#load package
#devtools::install_github("pat-s/mlr@all-mods") # working with GAM
# we have to see which mlr version we are using 

pacman::p_load(mlr, mapview,sp,sf,rgdal,ggplot2)
set.seed(1234)

#load data
#data <- readRDS("data/training_data/ndvi_16_sp25.RDS")
data <- readRDS("data/training_data/ndvi_16_sp25_xy.RDS")
# remove missing
data$ndvi_summer[data$ndvi_summer == "NaN"] <- NA # somehow there are NaN in the data - investigate
data$ndvi_summer[data$ndvi_winter == "NaN"] <- NA # somehow there are NaN in the data - investigate
# make sure response is logical
data$affected_factor <- as.factor(data$affected_factor)
class(data$affected_factor)
# only predictors
#colnames(data) # check
b.data_pred <- as.data.frame(data)[,c(29:36)] 

data_sf <- st_as_sf(data)
data_sf <- data_sf[,c(25,29,30:33)] # 36 for quan.

b.data_pred <- na.omit(b.data_pred) # removed 39 obs.
# reduce false to 666 for 1:1 sample
b.data_pred <- b.data_pred[-c(sample(which(b.data_pred$affected_factor == F), 39)),]
# add coords
coords <- b.data_pred %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_coordinates() %>%
  data.frame()
# drop geom
b.data_pred$geom <- NULL
head(b.data_pred)
#******************************************
#2 RF defenition---------------------------
#******************************************
## Define the task:
b.task_spatial <- makeClassifTask(id = "tree_health_rf", data = b.data_pred,
                                target = "affected_factor", positive = "TRUE",
                                coordinates =  coords )
b.task_spatial_no_season <- makeClassifTask(id = "tree_health_rf", data = b.data_pred[,1:5],
                                  target = "affected_factor", positive = "TRUE",
                                  coordinates =  coords )
saveRDS(b.task_spatial_no_season, "data/training_data/b_task_spatial_no_season.RDS") # with seasons
# define the learner
b.rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob",  fix.factors.prediction = TRUE,
                  par.vals = list(ntree = 300, mtry = 3)) # lets start with 300 trees
b.rf$par.vals <- list(importance = TRUE) # access var importance

#*******************************************************************************
# 3 Crossvalidation with out tuning --------------------------------------------
#*******************************************************************************

# stay with 5 folds
# for testing stay with 10 reps 

folds = 5
reps = 10

listMeasures("classif")
# test measures will be  f1, auc------------------------------------------------
# In statistical analysis of binary classification, the F1 score (also F-score or F-measure) 
# is a measure of a test's accuracy. It considers both the precision p and 
# the recall r of the test to compute the score: p is the number of correct positive 
# results divided by the number of all positive results returned by the classifier,
# and r is the number of correct positive results divided by the number of all relevant
# samples (all samples that should have been identified as positive). 
# The F1 score is the harmonic average of the precision and recall, where an F1 
# score reaches its best value at 1 (perfect precision and recall) and worst at 0.
#-------------------------------------------------------------------------------
# resampling spatial:
# resampling definition
resampling_spatial <- makeResampleDesc("SpRepCV", folds = folds, reps = reps)

b.spcv_rf <- mlr::resample(b.rf, b.task_spatial, resampling_spatial,
                         measures = list(f1, auc),
                         show.info = TRUE, models = TRUE)
#mean(b.spcv_rf$measures.test[,3], na.rm = T) mean auc = 0.549 mean f1 = 0.43
b.spcv_rf_no_season <- mlr::resample(b.rf, b.task_spatial_no_season, resampling_spatial,
                           measures = list(f1, auc),
                           show.info = TRUE, models = TRUE)
#mean(b.spcv_rf_no_season$measures.test[,3], na.rm = T) # f1.test.mean=0.4192392 auc mean = 0.61 --> leave seasons out
saveRDS(b.spcv_rf_no_season, "data/training_data/03.2_b.spcv.RDS")

# resampling non spatial
# resampling definition
resampling_non_spatial <- makeResampleDesc("RepCV",  folds = folds, reps = reps)

b.cv_rf <- mlr::resample(b.rf, b.task_spatial_no_season, resampling_non_spatial,
                       measures = list(f1,auc), 
                       show.info = TRUE, models = TRUE)


#*******************************************************************************
# 4 Tuning ---------------------------------------------------------------------
#*******************************************************************************

#set tunable parameters
#grid search to find hyperparameters

# hyperparameters ---------------------------------------------------------------

# parameters: mtry --> varibales test per node, stops exhaustive search with all variables per node

# The standard recursive partitioning algorithm would start with all the data and 
# do an exhaustive search over all variables and possible split points to find the
# one that best "explained" the entire data - reduced the node impurity the most. 
# The data are split according to the best split point and the process repeated in
# the left and right leaves in turn, recursively, until some stopping rules are met.
# The key thing here is that each time the recursive partitioning algorithm looks 
# for a split all the variables are included in the search.
# 
# Where RF models differ is that when forming each split in a tree, the algorithm 
# randomly selects mtry variables from the set of predictors available. Hence when
# forming each split a different random set of variables is selected within which 
# the best split point is chosen.
# 
# Hence for large trees, which is what RFs use, it is at least conceivable that 
# all variables might be used at some point when searching for split points whilst 
# growing the tree.

# parameter: ntree 

# In short there may be some overfitting due to the use of fully grown trees
# (the "average" that they talk about in the second statement), which may be showing 
# up as you add more trees to the forest.
# 
# Hastie et al (2009) suggest that this "overfitting" does not often cost much in 
# terms of prediction error, and if you don't tune that parameter then tuning is simplified.

# parameter: nodesize --> min. obs. per node to decide on

# A decision tree works by recursive partition of the training set. Every node t
# of a decision tree is associated with a set of nt data points from the training se

#-------------------------------------------------------------------------------


rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 6),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)


b.rf_tune <- tuneParams(learner = b.rf, resampling = resampling_spatial, task = b.task_spatial_no_season,
                      par.set = rf_param, control = rancontrol, measures = list(f1, multiclass.au1u))

b.rf_tune$y
# f1.test.mean multiclass.au1u.test.mean 
# 0.4650591                 0.6124026 
b.rf_tune$x
# 
# $ntree
# [1] 231
# 
# $mtry
# [1] 3
# 
# $nodesize
# [1] 48

#using hyperparameters for modeling
b.rf.tuned <- setHyperPars(b.rf, par.vals = b.rf_tune$x)

saveRDS(b.rf.tuned, "data/prediction_data/03_2_b_rf_tuned.RDS")

#*******************************************************************************
# 5 Crossvalidation with tuning --------------------------------------------
#*******************************************************************************
b.rf.tuned
# stay with 5 folds
# for testing stay with 10 reps 

folds = 5
reps = 10

# test measures will be  mmce 0-best, acc 1-best, because acc is not enough - see accuracy paradox

# resampling spatial:
# resampling definition
resampling_spatial <- makeResampleDesc("SpRepCV", folds = folds, reps = reps)

b.spcv_rf_tuned <- mlr::resample(b.rf.tuned, b.task_spatial_no_season, resampling_spatial,
                               measures = list(f1,auc),
                               show.info = TRUE, models = TRUE)
# mean(spcv_rf_tuned$measures.test[,2], na.rm = T) #f1  0.436
# mean(spcv_rf_tuned$measures.test[,3], na.rm = T) #auc 0.6007762
saveRDS(b.spcv_rf_tuned, "data/training_data/03.2_b.spcv_rf_tuned.RDS")
# resampling non spatial
# resampling definition
resampling_non_spatial <- makeResampleDesc("RepCV",  folds = folds, reps = reps)

cv_rf_tuned <- mlr::resample(rf.tuned, b.task_spatial_no_season, resampling_non_spatial,
                             measures = list(f1,auc), 
                             show.info = TRUE, models = TRUE)
#f1.test.mean=0.7269357,auc.test.mean=0.7777948


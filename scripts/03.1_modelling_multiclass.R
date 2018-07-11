# Filename: 03.1_modelling_multiclass.R

# TO DO:  model creation, tuning, model preformance vis

#**********************************************************
# CONTENTS-------------------------------------------------
# 1 PACKAGES AND DATA PREPERATION
# 2 Model definition
# 3 Crossvalidation
# 4 Tuning
# 5 Crossvalidation with tuning
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
data <- readRDS("data/training_data/ndvi_16_sp25_xy.RDS")
# remove missing
data$ndvi_summer[data$ndvi_summer == "NaN"] <- NA 
data$ndvi_summer[data$ndvi_winter == "NaN"] <- NA 
# make sure response is factor
data$affected <- as.factor(data$affected)
class(data$affected)
# only predictors
colnames(data) # check
data_pred <- as.data.frame(data)[,c(25,29,31:36)] 

data_sf <- st_as_sf(data)
data_sf <- data_sf[,c(25,29,30:33)] # 36 for quan.

data_pred <- na.omit(data_pred) # removed 39 obs.

# add coords
coords <- data_pred %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_coordinates() %>%
  data.frame()
# drop geom
data_pred$geom <- NULL
head(data_pred)
#******************************************
#2 RF defenition---------------------------
#******************************************
## Define the task:
task_spatial <- makeClassifTask(id = "tree_health_rf", data = data_pred,
                                target = "affected", 
                                coordinates =  coords )

task_spatial_no_seasons <- makeClassifTask(id = "tree_health_rf", data = data_pred[,1:5],
                                           target = "affected", 
                                           coordinates =  coords )


saveRDS(task_spatial, "data/training_data/task_spatial.RDS") # with seasons

# define the learner
rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob", #Classification: “response” (= labels) or
                                        #“prob” (= probabilities and labels by selecting 
                                        #the ones with maximal probability). 
                  par.vals = list(ntree = 300, mtry = 3)) # lets start with 300 trees
rf$par.vals <- list(importance = TRUE) # access var importance

#*******************************************************************************
# 3 Crossvalidation with out tuning --------------------------------------------
#*******************************************************************************

# stay with 5 folds
# for testing stay with 10 reps 

folds = 5
reps = 10
listMeasures("classif", properties = "classif.multi")
# test measures will be  au1u, mmce - auc variants are interchangable 
#See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.

# resampling spatial:
# resampling definition
resampling_spatial <- makeResampleDesc("SpRepCV", folds = folds, reps = reps)

spcv_rf <- mlr::resample(rf, task_spatial, resampling_spatial,
                          measures = list(multiclass.au1u, mmce),
                          show.info = TRUE, models = TRUE)
# test with seasons --> multiclass.au1u.test.mean=0.5875917,mmce.test.mean=0.5391991
spcv_rf_no_season <- mlr::resample(rf, task_spatial_no_seasons, resampling_spatial,
                         measures = list(multiclass.au1u, mmce),
                         show.info = TRUE, models = TRUE)
# test without winter summer ndvi -->  multiclass.au1u.test.mean=0.5819213,mmce.test.mean=0.5248575
# not better with out seasons


# resampling non spatial
# resampling definition
resampling_non_spatial <- makeResampleDesc("RepCV",  folds = folds, reps = reps)

cv_rf <- mlr::resample(rf, task_spatial, resampling_non_spatial,
                        measures = list(multiclass.au1u, mmce), 
                        show.info = TRUE, models = TRUE)

saveRDS(spcv_rf, "data/training_data/spcv_rf.RDS")
saveRDS(cv_rf, "data/training_data/cv_rf.RDS")
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


rf_tune <- tuneParams(learner = rf, resampling = resampling_spatial, task = task_spatial,
                      par.set = rf_param, control = rancontrol,
                      measures = list(multiclass.au1u, mmce))

rf_tune$y
# multiclass.au1u.test.mean            mmce.test.mean 
# 0.6004642                            0.48
rf_tune$x
# 
# $ntree
# [1] 445 # less tree less chance of overfitting
# 
# $mtry
# [1] 3
# 
# $nodesize
# [1] 48

#using hyperparameters for modeling
rf.tuned <- setHyperPars(rf, par.vals = rf_tune$x)

saveRDS(rf.tuned, "data/prediction_data/03_1_rf_tuned.RDS")

#*******************************************************************************
# 5 Crossvalidation with tuning ------------------------------------------------
#*******************************************************************************

# stay with 5 folds
# for testing stay with 10 reps 

folds = 5
reps = 10

# test measures will be  mmce 0-best, acc 1-best, because acc is not enough - see accuracy paradox

# resampling spatial:
# resampling definition
resampling_spatial <- makeResampleDesc("SpRepCV", folds = folds, reps = reps)

spcv_rf_tuned <- mlr::resample(rf.tuned, task_spatial, resampling_spatial,
                         measures = list(multiclass.au1u,mmce),
                         show.info = TRUE, models = TRUE)
#multiclass.au1u.test.mean=0.5982179,mmce.test.mean=0.5078481


# resampling non spatial
# resampling definition
resampling_non_spatial <- makeResampleDesc("RepCV",  folds = folds, reps = reps)

cv_rf_tuned <- mlr::resample(rf.tuned, task_spatial, resampling_non_spatial,
                       measures = list(multiclass.au1u,mmce), 
                       show.info = TRUE, models = TRUE)
#multiclass.au1u.test.mean=0.7755594,mmce.test.mean=0.3632740

saveRDS(spcv_rf_tuned, "data/training_data/spcv_rf_tuned.RDS")
saveRDS(cv_rf_tuned, "data/training_data/cv_rf_tuned.RDS")

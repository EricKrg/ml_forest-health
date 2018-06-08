############################
#tuning with mlr and kernlab
###########################
library(kernlab)
task.svm <- makeClassifTask(data = df,
                            target = "croptype",
                            coordinates = df[c("utmx","utmy")])

#learner
lrn.svm <- makeLearner("classif.ksvm",
                       kernel = "rbfdot",
                       predict.type = "response")

ctrl <- makeTuneControlRandom(maxit = 10)

num_ps = makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x)
)

#inner
inner <- makeResampleDesc("SpCV", iters = 5)
tune.wrapper <- makeTuneWrapper(learner = lrn.svm , resampling = inner,
                                par.set = num_ps, measures = list(mmce) , control = ctrl,
                                show.info = TRUE)


#outer
outer <- makeResampleDesc("SpCV", iters = 5)
configureMlr(on.par.without.desc = "quiet")

tune_res <- resample(tune.wrapper, task.svm, resampling =outer,
                     extract = getTuneResult,
                     measures = list(mmce),
                     show.info =  TRUE )
tune_res$extract
# Tune result:
#   Op. pars: C=1.56; sigma=0.00132
# mmce.test.mean=0.1054

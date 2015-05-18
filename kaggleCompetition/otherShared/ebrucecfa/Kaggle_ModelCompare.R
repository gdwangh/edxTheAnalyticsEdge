#-----------------------------------------
library(doParallel)
# SET UP THE PARAMETER SPACE SEARCH GRID
ctrl <- trainControl(method="repeatedcv",          # use repeated 10fold cross validation
                     repeats=5,	                        # do 5 repititions of 10-fold cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)
# Note that the default search grid selects 3 values of each tuning parameter
#
grid <- expand.grid(.interaction.depth = seq(1,7,by=2), # look at tree depths from 1 to 7
                    .n.trees=seq(10,100,by=5),	        # let iterations go from 10 to 100
                    .shrinkage=c(0.01,0.1))		# Try 2 values of the learning rate parameter

# BOOSTED TREE MODEL										
set.seed(123)
registerDoParallel(4)		                        # Registrer a parallel backend for train
getDoParWorkers()

system.time(gbm.tune <- train(pargs3,
                              data = Train,
                              method = "gbm",
                              metric = "ROC",
                              trControl = ctrl,
                              tuneGrid=grid,
                              verbose=FALSE))

#---------------------------------
# SUPPORT VECTOR MACHINE MODEL
#
set.seed(123)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
  svm.tune <- train(pargs3,
                    data = Train,
                    method = "svmRadial",
                    tuneLength = 9,		# 9 values of the cost function
                    preProc = c("center","scale"),
                    metric="ROC",
                    trControl=ctrl)	        # same as for gbm above
)	

#---------------------------------
# RANDOM FOREST MODEL
#
set.seed(123)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
  rf.tune <- train(pargs3,
                    data = Train,
                    method = "rf",
                    nodesize = 5,  	
                    ntree=500,
                    preProc = c("center","scale"),
                    metric="ROC",
                    trControl=ctrl)	        # same as for gbm above
)	

#---------------------------------
# PCR MODEL
#
set.seed(123)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
  pcr.tune <- train(pargs3,
                    data = Train,
                    method = "pcr",
                    tuneLength=20,
                    preProc = c("pca"),
                    metric="ROC",
                    trControl=ctrl)          # same as for gbm above
)	


#---------------------------------
# glm MODEL
#
#
set.seed(123)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
glm.tune = train(pargs3, 
                data=Train, 
                method="glm", 
                family="binomial", 
                preProcess = c('center', 'scale'), 
                metric="ROC", 
                trControl=ctrl)
)





#-----------------------------------
# COMPARE MODELS USING RESAPMLING
# Having set the seed to 1 before running gbm.tune and svm.tune we have generated paired samplesfor comparing models using resampling.
#
# The resamples function in caret collates the resampling results from the two models
rValues <- resamples(list(svm=svm.tune,gbm=gbm.tune, rf=rf.tune, glm=glm.tune, knn=knn.tune))
rValues$values
#---------------------------------------------
# BOXPLOTS COMPARING RESULTS
bwplot(rValues,metric="ROC")		# boxplot

# Predict
gbm.tune.pred = predict(gbm.tune, newdata=Test, type="prob")
gbm.tune.pred.prob = gbm.tune.pred[,2]

rf.tune.pred = predict(rf.tune, newdata=Test, type="prob")
rf.tune.pred.prob = rf.tune.pred[,2]

glm.tune.pred = predict.train(glm.tune, newdata=Test, type="prob")
glm.tune.pred.prob = glm.tune.pred[,2]

svm.tune.pred = predict.train(svm.tune, newdata=Test, type="prob")
svm.tune.pred.prob = svm.tune.pred[,2]



# Score  .93178 xRF = .29 yGLM = .12 zGBM = .30 WSVM=.29 Pre-Procpargs2 #BEST ENTRY OVERALL
# Score  .93169 xRF = .30 yGLM = .08 zGBM = .32 WSVM=.30 Pre-Proc pargs2
# Score  .92992 xRF = .25 yGLM = .15 zGBM = .35 WSVM=.25 Pre-Proc pargs2

xGBM = .32
xRF = .29
xGLM = .12
xSVM = .27
xKNN = .0

Probability1 = (gbm.tune.pred.prob*xGBM+
                  rf.tune.pred.prob*xRF+
                  glm.tune.pred.prob*xGLM+
                  svm.tune.pred.prob*xSVM+
                  knn.tune.pred.prob*xKNN
)

MySubmission.Tuned = data.frame(UniqueID = NewsTest$UniqueID, Probability1)
write.csv(MySubmission.Tuned, "SubmissionTuned.csv", row.names=FALSE)

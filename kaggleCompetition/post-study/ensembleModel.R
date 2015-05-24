# setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

newsTrain$logWordCount = log(1+newsTrain$WordCount)
newsTest$logWordCount = log(1+newsTest$WordCount)

# get info from pubdate
newsTrain$PubDate = strptime(newsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
newsTest$PubDate = strptime(newsTest$PubDate, "%Y-%m-%d %H:%M:%S")

newsTrain$Weekday = newsTrain$PubDate$wday
newsTest$Weekday = newsTest$PubDate$wday

newsTrain$Hour = newsTrain$PubDate$hour
newsTest$Hour = newsTest$PubDate$hour

# trans to factor
newsTrain$PopularFactor = as.factor(ifelse(newsTrain$Popular,"Yes", "No"))

newsTrain$NewsDesk = as.factor(newsTrain$NewsDesk)
newsTest$NewsDesk = factor(newsTest$NewsDesk, levels = levels(newsTrain$NewsDesk))

newsTrain$SectionName = as.factor(newsTrain$SectionName)
newsTest$SectionName = factor(newsTest$SectionName, levels = levels(newsTrain$SectionName))


newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))

newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))


## ENSEMBLE

library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

modelLookup("glm")$parameter # no parameter to tune
modelLookup("rf")$parameter  # mtry

library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)


rfGrid<- expand.grid(mtry=c(1:20))
set.seed(1000)
rfFit = train(PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")
# rfFit: The final value used for the model was mtry = 12
rfGrid<- expand.grid(mtry=c(12))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.8914681

stopCluster(cl)

# glmnet
modelLookup("glmnet")$parameter  # alpha  lambda


cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

library(glmnet)
# set.seed(1000)
# x = newsTrain[,c("NewsDesk","SectionName","SubsectionName","logWordCount","Weekday","Hour")]
# gla1 <- cv.glmnet(data.matrix(x), y=newsTrain$PopularFactor, nfolds = 10, alpha=1, type.measure="auc", family="binomial")
# gla1$lambda.1se   # 0.02832375
# gla1$lambda.min   # 0.0008256524
# 
# pred1 = predict(gla1$glmnet.fit, newx=data.matrix(x), s = gla1$lambda.min)
# 
# gla0 <- cv.glmnet(data.matrix(x), y=newsTrain$PopularFactor, nfolds = 10, alpha=0, type.measure="auc", family="binomial")
# gla0$lambda.1se  # 0.05560068
# gla0$lambda.min  # 0.01041855
# pred0 = predict(gla0$glmnet.fit, newx=data.matrix(x), s = gla0$lambda.min)
# 
# library("ROCR")
# ROCR.Pred1 = prediction(pred1, newsTrain$PopularFactor)
# auc1 = as.numeric(performance(ROCR.Pred1, "auc")@y.values)
# auc1  # 0.8208463
# 
# ROCR.Pred0 = prediction(pred0, newsTrain$PopularFactor)
# auc0 = as.numeric(performance(ROCR.Pred0, "auc")@y.values)
# auc0  # 0.8198742

# 0.0008256524
# glmGrid<- expand.grid(alpha=seq(0, 1, by=0.1), lambda=c(1:110)*0.0001)
# 
# set.seed(1000)
# glmnetFit = train(PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
#                   data = newsTrain,
#                   method="glmnet", 
#                   trControl=ensCtrl,
#                   tuneGrid=glmGrid,
#                   metric="ROC")
# 
# # lambda=c(1:100)*0.01, result :  alpha = 0.2 and lambda = 0.01
# #  lambda=c(1:110)*0.0001, result: alpha = 1 and lambda = 8e-04

glmGrid<- expand.grid(alpha=c(1), lambda=8e-04)

stopCluster(cl)


# gbm
modelLookup("gbm")

# library(doParallel)
# cl<- makeCluster(detectCores()-1)  
# registerDoParallel(cl)

# gbmGrid<- expand.grid(n.trees=c(1:50)*100, interaction.depth=c(1:50), shrinkage=c(0.001),n.minobsinnode = c(10))
gbmGrid<- expand.grid(n.trees=c(4900), interaction.depth=c(26), shrinkage=c(.001),n.minobsinnode = 10)

# ptm <- proc.time();ptm
# 
# set.seed(1000)
# gbmFit = train(PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
#                data = newsTrain,
#                method="gbm", 
#                trControl=ensCtrl,
#                tuneGrid=gbmGrid,
#                metric="ROC")
# 
# proc.time()-ptm
# 用户     系统     流逝 
# 366.17    14.36 69955.69 --- 19hours


# n.trees=c(1:10)*100, interaction.depth=c(1:10), shrinkage=c(0.001),n.minobsinnode = c(10)
# n.trees = 4900, interaction.depth = 26, shrinkage = 0.001, n.minobsinnode = 10.


# stopCluster(cl)
# 
# 
# pred.gbm = predict(gbmFit, type="prob")
# 
# library("ROCR")
# ROCR.Pred.gbm = prediction( newsTrain$Popular, pred.gbm[,2]>0.5)
# auc1 = as.numeric(performance(ROCR.Pred.gbm, "auc")@y.values)
# auc1  # 0.5235575


# svm
modelLookup("svmRadial")

library(e1071)
library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

# svmGrid<- expand.grid(.sigma=c(.0007),.C=c(16,32))
svmGrid<- expand.grid(.sigma=c(1:10)*.0001,.C=c(1:100)*0.1)

set.seed(1000)
svmFit = train(PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
               data = newsTrain,
                method="svmRadial", 
                trControl=ensCtrl,
                tuneGrid=svmGrid,
                 metric="ROC")


library(caretEnsemble)
model_list<- caretList(
  PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
  data=newsTrain,
  trControl=ensCtrl,
  metric="ROC",
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=rfGrid, nodesize=1, ntree=3000),
    glmnet=caretModelSpec(method="glmnet",tuneGrid=glmGrid, preProcess=c("center","scale")),
    gbm=caretModelSpec(method="gbm", tuneGrid=gbmGrid),
    svm=caretModelSpec(method="svmRadial",tuneGrid=svmGrid, preProcess=c("center","scale"))
  )
)

stopCluster(cl)

greedy_ensemble<- caretEnsemble(model_list)

library('caTools')
model_preds<- lapply(model_list, predict, newdata=newsTrain, type='prob')
model_preds<- lapply(model_preds, function(x) x[,'Yes'])
model_preds<- data.frame(model_preds)

ens_preds<- predict(greedy_ensemble, newdata=newsTrain)
model_preds$ensemble<- ens_preds

library("ROCR")
ROCR.Pred = prediction( newsTrain$Popular, ens_preds>0.5)
auc = as.numeric(performance(ROCR.Pred, "auc")@y.values)
auc  # 0.8949889

# other method
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

gbm_stack<- caretStack(
  model_list,
  method='gbm',
  verbose=FALSE,
  # tuneGrid=expand.grid(n.trees=c(2500), interaction.depth=c(24), shrinkage=c(.001),n.minobsinnode=c(10)),
  metric='ROC',
  trControl=trainControl(
    method='cv',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    allowParallel=TRUE,
    summaryFunction=twoClassSummary
  )
)


model_preds2 <- model_preds
model_preds2$ensemble <- predict(gbm_stack, newdata=newsTrain, type='prob')$Yes

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, model_preds2$ensemble>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9036781

stopCluster(cl)

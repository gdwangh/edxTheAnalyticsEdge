# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Hour = NewsTrain$PubDate$hour

NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$Hour = NewsTest$PubDate$hour

NewsTrain$Popular = as.factor(NewsTrain$Popular)

NewsDeskFactor = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
NewsTrain$NewsDesk = NewsDeskFactor[1:(nrow(NewsTrain))]
NewsTest$NewsDesk = NewsDeskFactor[(nrow(NewsTrain)+1):length(NewsDeskFactor)]

SectionNameFactor = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
NewsTrain$SectionName = SectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SectionName = SectionNameFactor[(nrow(NewsTrain)+1):length(SectionNameFactor)]

SubsectionNameFactor = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
NewsTrain$SubsectionName = SubsectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SubsectionName = SubsectionNameFactor[(nrow(NewsTrain)+1):length(SubsectionNameFactor)]

NewsTrain$logWordCount = log(NewsTrain$WordCount+1)
NewsTest$logWordCount = log(NewsTest$WordCount+1)

# split train data to training set and valid set
library(caTools)
set.seed(123)
spl = sample.split(NewsTrain$Popular, 0.7)
Train = subset(NewsTrain, spl==TRUE)
Valid = subset(NewsTrain, spl==FALSE)

# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train)
Pred = predict(SimpleRF, newdata=Valid, type="prob")
table(Valid$Popular, Pred[,2]>0.5)  
(1571+220)/nrow(Valid)  # 0.9137755


# auc
library("ROCR")
SimpleRF.pred = predict(SimpleRF, type = "prob")
ROCR.SimpleLog.pred = prediction(SimpleRF.pred[,2], Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9366087

library(caret)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1)

set.seed(825)
gbmFit3 <- train(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, 
                 data = Train,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")

pred = predict(gbmFit3, newdata=Valid, type = "prob")
table(pred, Valid$Popular)
(1571+212)/nrow(Valid)

# gbm
# library(gbm)
# # gbmFit <- gbm(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train,shrinkage=0.01, distribution='bernoulli',cv.folds=5,n.trees=3000,verbose=F)
# gbmFit <- gbm(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train,
#               distribution = "bernoulli",n.trees=1000,cv.folds = 3)
# best.iter <- gbm.perf(gbmFit,method="test")
# 
# # 用交叉检验确定最佳迭代次数
# best.iter <- gbm.perf(gbmFit,method='cv')
# 
# Pred = predict(gbmFit, newdata=Valid, n.trees=100)
# table(Valid$Popular, Pred[,2]>0.5)  


# xgboost: eXtreme Gradient Boosting
# install.packages('xgboost')
# devtools::install_github('dmlc/xgboost',subdir='R-package')
# library(xgboost)
# library(methods)
# label <- Train$Popular
# data <- as.matrix(Train[,1:3])
# xgmat <- xgb.DMatrix(data, label = label)

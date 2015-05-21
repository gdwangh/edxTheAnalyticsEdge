setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
#setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

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

# glm
set.seed(12345)
SimpleLog = glm(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train, family="binomial")
SimpleLog.Pred = predict(SimpleLog, newdata=Valid, type="response")

# rpart
library(rpart)
library(e1071)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid <- expand.grid(.cp= seq(0.00001, 0.01, 0.0001))

# Perform the cross validation
train(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
simpleCART = rpart(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, 
                   data = Train, method="class", cp=0.00381 )
simpleCART.pred = predict(simpleCART, newdata=Valid)
table(Valid$Popular, simpleCART.pred[,2]>0.5)  
(1568+212)/nrow(Valid)  # 0.9081633

# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train)
SimpleRF.Pred = predict(SimpleRF, newdata=Valid, type="prob")


# combine glm + rf
pred1 = (SimpleLog.Pred + SimpleRF.Pred[,2])/2
table(Valid$Popular, pred1>0.5)  
(1563+224)/nrow(Valid)  # 0.9117347

pred2 = (simpleCART.pred[,2] + SimpleRF.Pred[,2])/2
table(Valid$Popular, pred2>0.5)  
(1570+213)/nrow(Valid)  # 0.9096939

# train
library(caret)
fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
tr = train(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train, method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)

pred = predict(tr$finalModel,newdata=Valid,type="prob")
table(Valid$Popular, pred>0.5)  


# auc
library("ROCR")

ROCR.pred1 = prediction(pred1, Valid$Popular)
auc = as.numeric(performance(ROCR.pred1, "auc")@y.values)
auc  # 0.9338814

ROCR.pred2 = prediction(pred2, Valid$Popular)
auc = as.numeric(performance(ROCR.pred2, "auc")@y.values)
auc  # 0.932343

# test: glm+rf
SimpleLog.Pred = predict(SimpleLog, newdata=NewsTest, type="response")
SimpleRF.Pred = predict(SimpleRF, newdata=NewsTest, type="prob")
PredTest = (SimpleLog.Pred + SimpleRF.Pred[,2])/2

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionLogRF.csv", row.names=FALSE)

# test: rpart + rf
SimpleCART.Pred = predict(simpleCART, newdata=NewsTest)
SimpleRF.Pred = predict(SimpleRF, newdata=NewsTest, type="prob")
PredTest = (SimpleCART.Pred[,2] + SimpleRF.Pred[,2])/2

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionCARTRF.csv", row.names=FALSE)


setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
# setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

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

# glm:accuracy
SimpleLog = glm(Popular ~ WordCount+Weekday+Hour+NewsDesk, data=Train, family="binomial")
Pred = predict(SimpleLog, newdata=Valid, type="response")
table(Valid$Popular, Pred>0.5)  
(1553+166)/nrow(Valid)  # 0.8770408
(1549+210)/nrow(Valid)  # 0.897449

# auc
library("ROCR")
SimpleLog.pred = predict(SimpleLog, type = "response")
ROCR.SimpleLog.pred = prediction(SimpleLog.pred, Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9155476

# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ logWordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train)
Pred = predict(SimpleRF, newdata=Valid, type="prob")
table(Valid$Popular, Pred[,2]>0.5)  
(1570+219)/nrow(Valid)  # WordCount, 0.9127551
(1573+218)/nrow(Valid)  # logWordCount, 0.9137755


# auc
library("ROCR")
SimpleRF.pred = predict(SimpleRF, type = "prob")
ROCR.SimpleLog.pred = prediction(SimpleRF.pred[,2], Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9362988, 0.9375389

# adjust parameter
# change ntree and nodesize: 
# SimpleRF = randomForest(Popular~., data=Train, ntree=1000,nodesize=5)

for (ntr in seq(100, 2000, 100)) {
  set.seed(12345)
  SimpleRF = randomForest(Popular~logWordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train, ntree=ntr)
  Pred = predict(SimpleRF, newdata=Valid, type="prob")
  t = table(Valid$Popular,Pred[,2]>0.5)
  acc = (t[1,1] + t[2,2])/nrow(Valid)
  
  ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
  auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
  
  print(paste("ntree = ",ntr,": acc=", acc, ", auc=", auc))
}

for (nz in seq(1, 50, 1)) {
  set.seed(12345)
  SimpleRF = randomForest(Popular~logWordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train, ntree=600, nodesize=nz)
  Pred = predict(SimpleRF, newdata=Valid, type="prob")
  t = table(Valid$Popular,Pred[,2]>0.5)
  acc = (t[1,1] + t[2,2])/nrow(Valid)
  
  ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
  auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
  
  print(paste("nodesize = ",nz,": acc=", acc, ", auc=", auc))
}

# train result for mtry could not used in Random
# library(caret)
# library(pROC)
# set.seed(12345)
# fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
# tr = train(Popular~logWordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, Train, method="rf", nodesize=17, ntree=600, metric="ROC", trControl=fitControl)
# tr

library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ logWordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train,ntree=600, nodesize=17)
Pred = predict(SimpleRF, newdata=Valid, type="prob")
table(Valid$Popular, Pred[,2]>0.5)  
(1571+223)/nrow(Valid)  # 0.9153061

# auc
library("ROCR")
SimpleRF.pred = predict(SimpleRF, type = "prob")
ROCR.SimpleLog.pred = prediction(SimpleRF.pred[,2], Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9364746, 0.9362988, 0.9378572


PredTest = predict(SimpleRF, newdata=NewsTest, type="prob")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(MySubmission, "SubmissionSimplestRF.csv", row.names=FALSE)

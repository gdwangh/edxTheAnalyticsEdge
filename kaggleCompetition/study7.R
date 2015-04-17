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
NewsTest$SectionName = SectionNameFactor[(nrow(NewsTrain)+1):length(NewsDeskFactor)]

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
SimpleRF = randomForest(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName, data=Train)
Pred = predict(SimpleRF, newdata=Valid, type="prob")
table(Valid$Popular, Pred[,2]>0.5)  
(1566+224)/nrow(Valid)  # 0.9132653

# auc
library("ROCR")
SimpleRF.pred = predict(SimpleRF, type = "prob")
ROCR.SimpleLog.pred = prediction(SimpleRF.pred[,2], Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9364746

PredTest = predict(SimpleRF, newdata=NewsTest, type="prob")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(MySubmission, "SubmissionSimplestRF.csv", row.names=FALSE)

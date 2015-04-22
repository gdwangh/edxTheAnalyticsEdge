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
                
# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ WordCount+NewsDesk+Hour+Weekday+SectionName+SubsectionName, data=Train)
SimpleRF.Pred = predict(SimpleRF, newdata=Valid, type="prob")

pred = (SimpleLog.Pred + SimpleRF.Pred[,2])/2
table(Valid$Popular, pred>0.5)  
(1567+220)/nrow(Valid)  # 0.9117347



# auc
library("ROCR")
pred1 = predict(SimpleLog, type = "response")
Pred2 = predict(SimpleRF, type = "prob")
Pred = (pred1 + Pred2[,2])/2
ROCR.SimpleLog.pred = prediction(Pred, Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9420884


# test
SimpleLog.Pred = predict(SimpleLog, newdata=NewsTest, type="response")
SimpleRF.Pred = predict(SimpleRF, newdata=NewsTest, type="prob")
PredTest = (SimpleLog.Pred + SimpleRF.Pred[,2])/2

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionLogRF.csv", row.names=FALSE)

setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
# setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)


# impute NA
# check NewsDesk == NA
tmp1 = subset(NewsTrain, NewsDesk == "")  
table(tmp1$NewsDesk, tmp1$SectionName)

# NewsDesk == NA, SectionName != NA
NewsTrain[NewsTrain$NewsDesk=="" & NewsTrain$SectionName!="",]$NewsDesk = NewsTrain[NewsTrain$NewsDesk=="" & NewsTrain$SectionName!="",]$SectionName
NewsTest[NewsTest$NewsDesk=="" & NewsTest$SectionName!="",]$NewsDesk = NewsTest[NewsTest$NewsDesk=="" & NewsTest$SectionName!="",]$SectionName

# NewsDesk == NA, SectionName == NA, try SubsectionName: SubsectionName is NA too
tmp2 = subset(NewsTrain, NewsDesk == "" & SectionName=="")
table(tmp2$NewsDesk, tmp2$SubsectionName)

tmp2 = subset(NewsTest, NewsDesk == "" & SectionName=="")
table(tmp2$NewsDesk, tmp2$SubsectionName)

# SectionName == NA

# NewsDesk != NA, SectionName == NA
tmp3 = subset(NewsTrain, NewsDesk != "" & SectionName=="")
table(tmp3$NewsDesk, tmp3$SectionName)

NewsTrain[NewsTrain$NewsDesk!="" & NewsTrain$SectionName=="",]$SectionName = NewsTrain[NewsTrain$NewsDesk!="" & NewsTrain$SectionName=="",]$NewsDesk
NewsTest[NewsTest$NewsDesk!="" & NewsTest$SectionName=="",]$SectionName = NewsTest[NewsTest$NewsDesk!="" & NewsTest$SectionName=="",]$NewsDesk

# check SubsectionName == NA, SectionName != NA
tmp5 = subset(NewsTrain, SectionName != "" & SubsectionName=="")
table(tmp5$SectionName, tmp5$SubsectionName)

NewsTrain[NewsTrain$SectionName!="" & NewsTrain$SubsectionName=="",]$SubsectionName = NewsTrain[NewsTrain$SectionName!="" & NewsTrain$SubsectionName=="",]$SectionName
NewsTest[NewsTest$SectionName!="" & NewsTest$SubsectionName=="",]$SubsectionName = NewsTest[NewsTest$SectionName!="" & NewsTest$SubsectionName=="",]$SectionName

# check SubsectionName == NA, NewsDesk != NA
tmp4 = subset(NewsTrain, NewsDesk != "" & SubsectionName=="")

table(tmp4$NewsDesk, tmp4$SubsectionName)

NewsTrain[NewsTrain$NewsDesk!="" & NewsTrain$SubsectionName=="",]$SubsectionName = NewsTrain[NewsTrain$NewsDesk!="" & NewsTrain$SubsectionName=="",]$NewsDesk
NewsTest[NewsTest$NewsDesk!="" & NewsTest$SubsectionName=="",]$SubsectionName = NewsTest[NewsTest$NewsDesk!="" & NewsTest$SubsectionName=="",]$NewsDesk

# NewsTrain[NewsTrain$NewsDesk=="",]$NewsDesk = "empty"
# NewsTest[NewsTest$NewsDesk=="",]$NewsDesk = "empty"

# translate 
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Hour = NewsTrain$PubDate$hour

NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$Hour = NewsTest$PubDate$hour

NewsTrain$Popular = as.factor(NewsTrain$Popular)

NewsDeskFactor = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
NewsTrain$NewsDeskFactor = NewsDeskFactor[1:(nrow(NewsTrain))]
NewsTest$NewsDeskFactor = NewsDeskFactor[(nrow(NewsTrain)+1):length(NewsDeskFactor)]

SectionNameFactor = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
NewsTrain$SectionNameFactor = SectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SectionNameFactor = SectionNameFactor[(nrow(NewsTrain)+1):length(SectionNameFactor)]

SubsectionNameFactor = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
NewsTrain$SubsectionNameFactor = SubsectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SubsectionNameFactor = SubsectionNameFactor[(nrow(NewsTrain)+1):length(SubsectionNameFactor)]

NewsTrain$logWordCount = log(NewsTrain$WordCount + 1)
NewsTest$logWordCount = log(NewsTest$WordCount + 1)

NewsTrain$NewsDeskInt = as.integer(NewsTrain$NewsDeskFactor)
NewsTrain$SectionNameInt = as.integer(NewsTrain$SectionNameFactor)
NewsTrain$SubsectionNameInt = as.integer(NewsTrain$SubsectionNameFactor)

NewsTest$NewsDeskInt = as.integer(NewsTest$NewsDeskFactor)
NewsTest$SectionNameInt = as.integer(NewsTest$SectionNameFactor)
NewsTest$SubsectionNameInt = as.integer(NewsTest$SubsectionNameFactor)

# split train data to training set and valid set
library(caTools)
set.seed(333)
spl = sample.split(NewsTrain$Popular, 0.7)
Train = subset(NewsTrain, spl==TRUE)
Valid = subset(NewsTrain, spl==FALSE)

# glm has error
# # glm
# SimpleLog = glm(Popular ~ WordCount+Hour+Weekday+NewsDesk+SectionName+SubsectionName, data=Train, family="binomial")
# SimpleLog.Pred = predict(SimpleLog, newdata=Valid, type="response")

library(glmnet)

cols = c("logWordCount", "Hour", "Weekday", "NewsDeskInt", "SectionNameInt","SubsectionNameInt")

set.seed(12345)
fit = cv.glmnet(x=as.matrix(Train[, cols]), y=Train$Popular, alpha=1, type.measure='auc', family = "binomial")
plot(fit)

fit$lambda.1se
fit.best <- fit$glmnet.fit

fit.coef <- coef(fit$glmnet.fit, s = fit$lambda.1se) 
fit.coef[which(fit.coef != 0)] 

# glmnet.pred = predict(fit, s='lambda.min', newx=as.matrix(Valid[, cols]), type="response")
glmnet.pred = predict(fit, s='lambda.1se', newx=as.matrix(Valid[, cols]), type="response")

table(Valid$Popular, glmnet.pred>0.5)  
# (1599+16)/nrow(Valid)  # 0.8239796
(1620+2)/nrow(Valid)    # 0.827551

# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular ~ WordCount+Hour+Weekday+NewsDeskFactor+SectionNameFactor+SubsectionNameFactor, data=Train)
SimpleRF.Pred = predict(SimpleRF, newdata=Valid, type="prob")

table(Valid$Popular, SimpleRF.Pred[,2]>0.5)  
(1574+227)/nrow(Valid)  # 0.9188776

pred = SimpleRF.Pred[,2] * 0.7 + glmnet.pred * 0.3
table(Valid$Popular, pred>0.5)  

(1584+210)/nrow(Valid)  # 0.9153061


# auc
library("ROCR")

ROCR.SimpleRF.Pred = prediction(SimpleRF.Pred[,2], Valid$Popular)
auc = as.numeric(performance(ROCR.SimpleRF.Pred, "auc")@y.values)
auc  # 0.9398286

ROCR.pred = prediction(pred, Valid$Popular)
auc = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc  # 0.9425832

# test
# SimpleLog.Pred = predict(SimpleLog, newdata=NewsTest, type="response")

Glmnet.Pred = predict(fit, s='lambda.min', newx=as.matrix(NewsTest[, cols]), type="response")

SimpleRF.Pred = predict(SimpleRF, newdata=NewsTest, type="prob")
PredTest = Glmnet.Pred * 0.3 + SimpleRF.Pred[,2]*0.7

# PredTest = predict(SimpleRF, newdata=NewsTest, type="prob")
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionImputeGlmnetRF.csv", row.names=FALSE)


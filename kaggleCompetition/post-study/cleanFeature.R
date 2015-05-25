setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
#setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

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

# newsTrain$NewsDesk = as.factor(newsTrain$NewsDesk)
# newsTest$NewsDesk = factor(newsTest$NewsDesk, levels = levels(newsTrain$NewsDesk))
# 
# newsTrain$SectionName = as.factor(newsTrain$SectionName)
# newsTest$SectionName = factor(newsTest$SectionName, levels = levels(newsTrain$SectionName))
# 
# newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
# newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))
# 
# newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
# newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))
# 

allCat = rbind(newsTrain[,c("NewsDesk", "SectionName","SubsectionName")], newsTest[,c("NewsDesk", "SectionName","SubsectionName"), ])

table(allCat$SectionName,allCat$NewsDesk)

allCat[allCat$NewsDesk == "Business" & allCat$SectionName=="",]$SectionName = "Business Day"
allCat[allCat$NewsDesk == "Culture" & allCat$SectionName=="",]$SectionName = "Arts"
allCat[allCat$NewsDesk == "Foreign" & allCat$SectionName=="",]$SectionName = "World"
allCat[allCat$NewsDesk == "National" & allCat$SectionName=="",]$SectionName = "U.S."
allCat[allCat$NewsDesk == "OpEd" & allCat$SectionName=="",]$SectionName = "Opinion"
allCat[allCat$NewsDesk == "Science" & allCat$SectionName=="",]$SectionName = "Health"
allCat[allCat$NewsDesk == "Sports" & allCat$SectionName=="",]$SectionName = "Sports"
allCat[allCat$NewsDesk == "Styles" & allCat$SectionName=="",]$SectionName = "U.S."
allCat[allCat$NewsDesk == "TStyle" & allCat$SectionName=="",]$SectionName = "TStyle"

# TStyle all empty

allCat[allCat$NewsDesk == "" & allCat$SectionName=="Arts",]$NewsDesk = "Culture"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Business Day",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Crosswords/Games",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Health",]$NewsDesk = "Science"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="N.Y. / Region",]$NewsDesk = "Metro"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Open",]$NewsDesk = "Open"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Opinion",]$NewsDesk = "OpEd"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Travel",]$NewsDesk = "Travel"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Technology",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="U.S.",]$NewsDesk = "Styles"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="World",]$NewsDesk = "Foreign"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Multimedia",]$NewsDesk = "Multimedia"


allCat$NewsDesk = as.factor(allCat$NewsDesk)
allCat$SectionName = as.factor(allCat$SectionName)
allCat$SubsectionName = as.factor(allCat$SubsectionName)

newsTrain$NewsDeskFactor = head(allCat$NewsDesk, nrow(newsTrain))
newsTest$NewsDeskFactor = tail(allCat$NewsDesk, nrow(newsTest))


newsTrain$SectionNameFactor = head(allCat$SectionName, nrow(newsTrain))
newsTest$SectionNameFactor = tail(allCat$SectionName, nrow(newsTest))


newsTrain$SubsectionNameFactor = head(allCat$SubsectionName, nrow(newsTrain))
newsTest$SubsectionNameFactor = tail(allCat$SubsectionName, nrow(newsTest))

# rf
library(randomForest)
set.seed(1000)
rfFit = randomForest(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour, data=newsTrain)
pred = predict(rfFit, type="prob")

# auc
library("ROCR")
ROCR.RF.Pred = prediction(pred[,2], newsTrain$PopularFactor)
auc = as.numeric(performance(ROCR.RF.Pred, "auc")@y.values)
auc  # 0.929621

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/cleanFeatureRF.csv", row.names=FALSE)

# add cv
library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)

library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

rfGrid<- expand.grid(mtry=c(1:20))
set.seed(1000)
train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")   # mtry = 16. 

rfGrid<- expand.grid(mtry=c(16))
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")
pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9126134

stopCluster(cl)

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/cleanFeatureRF_CV.csv", row.names=FALSE)

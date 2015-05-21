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

# abstract bag, not good to acc & auc
library(tm)
Corpus = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))

Corpus = tm_map(Corpus, tolower)
Corpus = tm_map(Corpus, PlainTextDocument)
Corpus = tm_map(Corpus, removePunctuation)
Corpus = tm_map(Corpus, removeWords, stopwords("english"))
Corpus = tm_map(Corpus, stemDocument)

dtm = DocumentTermMatrix(Corpus)

sparse = removeSparseTerms(dtm, 0.98)

NewsWords = as.data.frame(as.matrix(sparse))
colnames(NewsWords) = make.names(colnames(NewsWords))

# split train and test
NewsTmpWords = head(NewsWords, nrow(NewsTrain))
NewsTestWords = head(NewsWords, nrow(NewsTest))

# add other variables
NewsTmpWords$Popular = NewsTrain$Popular
NewsTmpWords$WordCount = NewsTrain$WordCount
NewsTmpWords$Weekday = NewsTrain$Weekday
NewsTmpWords$Hour = NewsTrain$Hour
NewsTmpWords$NewsDesk = NewsTrain$NewsDesk
NewsTmpWords$SectionName = NewsTrain$SectionName
NewsTmpWords$SubsectionName = NewsTrain$SubsectionName

NewsTestWords$UniqueID = NewsTest$UniqueID
NewsTestWords$WordCount = NewsTest$WordCount
NewsTestWords$Weekday = NewsTest$Weekday
NewsTestWords$Hour = NewsTest$Hour
NewsTestWords$NewsDesk = NewsTest$NewsDesk
NewsTestWords$SectionName = NewsTest$SectionName
NewsTestWords$SubsectionName = NewsTest$SubsectionName

# split train data to training set and valid set
library(caTools)
set.seed(333)
spl = sample.split(NewsTmpWords$Popular, 0.7)
Train = subset(NewsTmpWords, spl==TRUE)
Valid = subset(NewsTmpWords, spl==FALSE)

# randomForest 
library(randomForest)
set.seed(12345)
SimpleRF = randomForest(Popular~., data=Train, ntree = 800, nodesize=11)
Pred = predict(SimpleRF, newdata=Valid, type="prob")
table(Valid$Popular,Pred[,2]>0.5)  
(1580+219)/nrow(Valid)  # 0.9178571
(1580+217)/nrow(Valid)  # 0.9168367

# auc
library("ROCR")
ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
auc  # 0.9472423, 0.9470835

# change ntree and nodesize: 
# SimpleRF = randomForest(Popular~., data=Train, ntree=1000,nodesize=5)

# for (ntr in seq(100, 2000, 100)) {
#   set.seed(12345)
#   SimpleRF = randomForest(Popular~., data=Train)
#   Pred = predict(SimpleRF, newdata=Valid, type="prob")
#   t = table(Valid$Popular,Pred[,2]>0.5)
#   acc = (t[1,1] + t[2,2])/nrow(Valid)
#   
#   ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
#   auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
#   
#   print(paste("ntree = ",ntr,": acc=", acc, ", auc=", auc))
# }
# 
# for (nz in seq(1, 50, 1)) {
#   set.seed(12345)
#   SimpleRF = randomForest(Popular~., data=Train, ntree=800, nodesize=nz)
#   Pred = predict(SimpleRF, newdata=Valid, type="prob")
#   t = table(Valid$Popular,Pred[,2]>0.5)
#   acc = (t[1,1] + t[2,2])/nrow(Valid)
#   
#   ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
#   auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
#   
#   print(paste("nodesize = ",nz,": acc=", acc, ", auc=", auc))
# }

library(caret)
library(pROC)
set.seed(12345)
fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
tr = train(Popular~., Train, method="rf", nodesize=7, ntree=800, metric="ROC", trControl=fitControl)

# auc
library("ROCR")
ROCR.SimpleRF.pred = prediction(Pred[,2], Valid$Popular)
auc = as.numeric(performance(ROCR.SimpleRF.pred, "auc")@y.values)
auc  # 0.9320268


# predict test
PredTest = predict(SimpleRF, newdata=NewsTestWords, type="prob")

MySubmission = data.frame(UniqueID = NewsTestWords$UniqueID, Probability1 = PredTest[,2])

write.csv(MySubmission, "SubmissionBagAbstractRF.csv", row.names=FALSE)

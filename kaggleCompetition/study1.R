setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# baseline
table(NewsTrain$Popular)

# 0: 5439, 1:1093
#  AUC
library("ROCR")
ROCRpredTest = prediction(rep(0, nrow(NewsTrain)), NewsTrain$Popular)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc  # 0.5

# glm
SimpleMod = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)
simpleMod.pred = predict(SimpleMod, type = "response")
ROCR.simpleMod.pred = prediction(simpleMod.pred, NewsTrain$Popular)
auc2 = as.numeric(performance(ROCR.simpleMod.pred, "auc")@y.values)
auc2  # 0.7361707

# text data: headline
library(tm)
CorpusHeadline = Corpus(VectorSource(NewsTrain$Headline))

CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWordsTrain = as.data.frame(as.matrix(sparse))
colnames(HeadlineWordsTrain) = make.names(colnames(HeadlineWordsTrain))

HeadlineWordsTrain$Popular = NewsTrain$Popular

HeadlineWordsTrain$WordCount = NewsTrain$WordCount

HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

HeadlineWordsLog.pred = predict(HeadlineWordsLog, type = "response")
ROCR.HeadlineWordsLog.pred = prediction(HeadlineWordsLog.pred, HeadlineWordsTrain$Popular)
auc3 = as.numeric(performance(ROCR.HeadlineWordsLog.pred, "auc")@y.values)
auc3  # 0.7904727

# randomForest
library("randomForest")
set.seed(123)
HeadlineWordsRF = randomForest(Popular ~ ., data=HeadlineWordsTrain)
HeadlineWordsRF.pred = predict(HeadlineWordsRF)
ROCR.HeadlineWordsRF.pred = prediction(HeadlineWordsRF.pred, HeadlineWordsTrain$Popular)
auc4 = as.numeric(performance(ROCR.HeadlineWordsRF.pred, "auc")@y.values)
auc4  # 0.7758517


# randomForest
library("rpart")
HeadlineWordsCART = rpart(Popular ~ ., data=HeadlineWordsTrain, minbucket=25)
HeadlineWordsCART.pred = predict(HeadlineWordsCART)
ROCR.HeadlineWordsCART.pred = prediction(HeadlineWordsCART.pred, HeadlineWordsTrain$Popular)
auc5 = as.numeric(performance(ROCR.HeadlineWordsCART.pred, "auc")@y.values)
auc5  # 0.7329899



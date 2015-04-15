setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain$Popular  = as.factor( NewsTrain$Popular)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# bag: headline
library(tm)
library(SnowballC)
CorpusHeadline = Corpus(VectorSource(NewsTrain$Headline))

CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWordsTrain = as.data.frame(as.matrix(sparse))
colnames(HeadlineWordsTrain) = paste0("headline.", make.names(colnames(HeadlineWordsTrain)))


# bag: snippet
library(tm)
CorpusSnippet = Corpus(VectorSource(NewsTrain$Snippet))

CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

dtm = DocumentTermMatrix(CorpusSnippet)

sparse = removeSparseTerms(dtm, 0.98)

SnippetWordsTrain = as.data.frame(as.matrix(sparse))
colnames(SnippetWordsTrain) = paste0("Snippet.", make.names(colnames(SnippetWordsTrain)))

# # bag: abstract
library(tm)
CorpusAbstract = Corpus(VectorSource(NewsTrain$Abstract))

CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtm = DocumentTermMatrix(CorpusAbstract)

sparse = removeSparseTerms(dtm, 0.98)

AbstractWordsTrain = as.data.frame(as.matrix(sparse))
colnames(AbstractWordsTrain) = paste0("Abstract.", make.names(colnames(AbstractWordsTrain)))


NewsTrainWords = cbind(HeadlineWordsTrain, SnippetWordsTrain, AbstractWordsTrain)
NewsTrainWords$Popular = NewsTrain$Popular
NewsTrainWords$WordCount = NewsTrain$WordCount 
NewsTrainWords$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrainWords$SectionName = as.factor(NewsTrain$SectionName)
NewsTrainWords$SubsectionName = as.factor(NewsTrain$SubsectionName)

# glm
NewsWordsLog = glm(Popular ~ ., data=NewsTrainWords, family=binomial)

NewsWordsLog.pred = predict(NewsWordsLog, type = "response")

library(ROCR)
ROCR.NewsWordsLog.pred = prediction(NewsWordsLog.pred, NewsTrainWords$Popular)
auc1 = as.numeric(performance(ROCR.NewsWordsLog.pred, "auc")@y.values)
auc1  # 0.831404, 0.9176791, 0.9314769, 0.9433447

# randomForest, auc change little
library(randomForest)
set.seed(12345)
NewsWordsRF = randomForest(Popular~.,data=NewsTrainWords)
NewsWordsRF.pred = predict(NewsWordsRF, type="prob")
ROCR.NewsWordsRF.pred = prediction(NewsWordsRF.pred[,2], NewsTrainWords$Popular)
auc2 = as.numeric(performance(ROCR.NewsWordsRF.pred, "auc")@y.values)
auc2  # 0.7865993, 0.9020076,0.9250309, 0.9289548

# Tree, change little
library(rpart)
library(rpart.plot)
library(ROCR)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.01,0.001)) 

# Perform the cross validation
train(Popular ~ ., data = NewsTrainWords, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
NewsWordsTree = rpart(Popular ~ ., data=NewsTrainWords, method="class", cp=0.006)
NewsWordsTree.pred = predict(NewsWordsTree)
ROCR.NewsWordsTree.pred = prediction(NewsWordsTree.pred[,2], NewsTrainWords$Popular)
auc3 = as.numeric(performance(ROCR.NewsWordsTree.pred, "auc")@y.values)
auc3  # 0.5, 0.7273768, 0.7781507, 0.7821516, 0.8909988

prp(NewsWordsTree)




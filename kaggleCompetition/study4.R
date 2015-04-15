setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain$Popular  = as.factor( NewsTrain$Popular)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(NewsTrain)

# bag: headline
library(tm)
library(SnowballC)

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = paste0("headline.", make.names(colnames(HeadlineWords)))

# bag: snippet
library(tm)
CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet,NewsTest$Snippet)))

CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

dtm = DocumentTermMatrix(CorpusSnippet)

sparse = removeSparseTerms(dtm, 0.98)

SnippetWords = as.data.frame(as.matrix(sparse))
colnames(SnippetWords) = paste0("Snippet.", make.names(colnames(SnippetWords)))

# bag: abstract
library(tm)
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))

CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtm = DocumentTermMatrix(CorpusAbstract)

sparse = removeSparseTerms(dtm, 0.98)

AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = paste0("Abstract.", make.names(colnames(AbstractWords)))

NewsWords = cbind(HeadlineWords, SnippetWords, AbstractWords)

# split 
NewsTrainWords = head(NewsWords, nrow(NewsTrain))
NewsTestWords = head(NewsWords, nrow(NewsTest))

NewsTrainWords$Popular = NewsTrain$Popular

NewsTrainWords$WordCount = NewsTrain$WordCount
NewsTestWords$WordCount = NewsTest$WordCount

newsDesk = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
NewsTrainWords$NewsDesk = head(newsDesk, nrow(NewsTrain))
NewsTestWords$NewsDesk = head(newsDesk, nrow(NewsTest))

SectionName = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
NewsTrainWords$SectionName = head(SectionName, nrow(NewsTrain))
NewsTestWords$SectionName = head(SectionName, nrow(NewsTest))

SubsectionName = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
NewsTrainWords$SubsectionName = head(SubsectionName, nrow(NewsTrain))
NewsTestWords$SubsectionName = head(SubsectionName, nrow(NewsTest))

pdate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrainWords$Weekday = pdate$wday
NewsTrainWords$Hour = pdate$hour

pdate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTestWords$Weekday = pdate$wday
NewsTestWords$Hour = pdate$hour

# glm
NewsWordsLog = glm(Popular ~ ., data=NewsTrainWords, family="binomial")
NewsWordsLog.pred = predict(NewsWordsLog, newdata = NewsTestWords, type = "response")

library(ROCR)
pred = predict(NewsWordsLog, type = "response")
ROCR.pred = prediction(pred, NewsTrainWords$Popular)
auc1 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc1  

# glm 2
NewsLog2 = glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords, family=binomial)
NewsTest.pred2 = predict(NewsLog2, newdata = NewsTestWords, type = "response")
# 0.82859, score = 0.60406

# 
# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTest.pred2)

write.csv(MySubmission, "SubmissionLog.csv", row.names=FALSE)

# randome Forest
library(caret)
library(e1071)

# Perform the cross validation
train(Popular ~ ., data = NewsTrainWords, method = "rf")

library(randomForest)
set.seed(12345)
NewsWordsRF = randomForest(Popular~.,data=NewsTrainWords)

# auc
NewsWordsRF.pred = predict(NewsWordsRF, type="prob")
ROCR.NewsWordsRF.pred = prediction(NewsWordsRF.pred[,2], NewsTrainWords$Popular)
auc2 = as.numeric(performance(ROCR.NewsWordsRF.pred, "auc")@y.values)
auc2

# pred
predTest = predict(NewsWordsRF, newdata=NewsTestWords, type="prob")

# randomForest simple
NewsSimpleRF = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords)
testRF.pred = predict(NewsSimpleRF, newdata=NewsTestWords, type="prob")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testRF.pred[,2])

write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)
# 0.82859, score=0.62535

# simple tree
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.01,0.001)) 

# Perform the cross validation
train(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

NewsWordsSimpleTree = rpart(Popular ~  NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords, method="class", cp=0.002)
simple.testpred = predict(NewsWordsSimpleTree, newdata=NewsTestWords)
# 0.82859, score =  0.61426

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = simple.testpred[,2])
write.csv(MySubmission, "SubmissionCART.csv", row.names=FALSE)

# add text
NewsWordsSimpleTree = rpart(Popular~., data=NewsTrainWords, method="class", cp=0.002)
testpred = predict(NewsWordsSimpleTree, newdata=NewsTestWords)

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = simple.testpred[,2])
write.csv(MySubmission, "SubmissionCART.csv", row.names=FALSE)



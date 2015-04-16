# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

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

sparse = removeSparseTerms(dtm, 0.97)

HeadlineWords = as.data.frame(as.matrix(sparse))
HeadlineWords$wordNum = rowSums(as.matrix(sparse))
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

sparse = removeSparseTerms(dtm, 0.95)

SnippetWords = as.data.frame(as.matrix(sparse))
SnippetWords$wordNum = rowSums(as.matrix(sparse))
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

sparse = removeSparseTerms(dtm, 0.95)

AbstractWords = as.data.frame(as.matrix(sparse))
AbstractWords$wordNum = rowSums(as.matrix(sparse))
colnames(AbstractWords) = paste0("Abstract.", make.names(colnames(AbstractWords)))

# NewsWords = cbind(HeadlineWords, SnippetWords, AbstractWords)
NewsWords = cbind(HeadlineWords, SnippetWords)

# split 
NewsTmpWords = head(NewsWords, nrow(NewsTrain))
NewsTestWords = head(NewsWords, nrow(NewsTest))

NewsTmpWords$Popular = NewsTrain$Popular

NewsTmpWords$WordCount = NewsTrain$WordCount
NewsTestWords$WordCount = NewsTest$WordCount

newsDesk = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
NewsTmpWords$NewsDesk = head(newsDesk, nrow(NewsTrain))
NewsTestWords$NewsDesk = head(newsDesk, nrow(NewsTest))

SectionName = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
NewsTmpWords$SectionName = head(SectionName, nrow(NewsTrain))
NewsTestWords$SectionName = head(SectionName, nrow(NewsTest))

SubsectionName = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
NewsTmpWords$SubsectionName = head(SubsectionName, nrow(NewsTrain))
NewsTestWords$SubsectionName = head(SubsectionName, nrow(NewsTest))

pdate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTmpWords$Weekday = pdate$wday
NewsTmpWords$Hour = pdate$hour


pdate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTestWords$Weekday = pdate$wday
NewsTestWords$Hour = pdate$hour

# split NewsTrainWords to training set and validation set
library(caTools)
set.seed(123)
spl = sample.split(NewsTmpWords$Popular, 0.7)
NewsTrainWords = subset(NewsTmpWords, spl==TRUE)
NewsValidWords = subset(NewsTmpWords, spl==FALSE)

# complex glm with text bag: scores = 0.58749, accuracy = 0.9040816, auc = 0.9374146
NewsWordsLog = glm(Popular ~ ., data=NewsTrainWords, family="binomial")

pred = predict(NewsWordsLog, type = "response")
table(NewsTrainWords$Popular, pred>0.5)  
# train accuracy = 0.9092301, 0.9074803
(3644+505)/(3644+505+163+260)

pred = predict(NewsWordsLog, newdata = NewsValidWords, type = "response")

table(NewsValidWords$Popular, pred>0.5)  
# valid accuracy = 0.9030612, 0.9040816
(1546+226) / (1546+226+86+102)

# NewsWordsLog.pred = predict(NewsWordsLog, newdata = NewsTestWords, type = "response")

# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsWordsLog.pred)
# write.csv(MySubmission, "SubmissionLogBag.csv", row.names=FALSE)

library(ROCR)
pred = predict(NewsWordsLog, type = "response")
ROCR.pred = prediction(pred, NewsTrainWords$Popular)
auc1 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc1  

#################################################################

# glm 2 simple: scores = 0.60406, valid accuracy = 0.9021905, auc = 0.9356753
NewsLog2 = glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour+headline.wordNum+Snippet.wordNum, data=NewsTrainWords, family=binomial)

pred = predict(NewsLog2, type = "response")
table(NewsTrainWords$Popular, pred>0.5)  
# train accuracy = 0.9074803
(3642+507)/(3642+507+258+165)

pred = predict(NewsLog2, newdata = NewsValidWords, type = "response")
table(NewsValidWords$Popular, pred>0.5)  
# valid accuracy = 0.9021905
(1547+224) / (1547+227+85+104)

# auc = 0.9356753
pred = predict(NewsLog2, type = "response")
ROCR.pred = prediction(pred, NewsTrainWords$Popular)
auc1 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc1 

# NewsTest.pred2 = predict(NewsLog2, newdata = NewsTestWords, type = "response")
 
# Now we can prepare our submission file for Kaggle:
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTest.pred2)

# write.csv(MySubmission, "SubmissionLogSimple.csv", row.names=FALSE)

##################################################################################
# randome Forest with text bag: scores = 0.62870, accurrcy = 0.9096939, auc=0.933361
library(caret)
library(e1071)

library(randomForest)
set.seed(12345)
NewsWordsRF = randomForest(Popular~.,data=NewsTrainWords)

pred = predict(NewsWordsRF, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9111986
(3665+501)/(3665+501+264+142)

pred = predict(NewsWordsRF, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9096939
(1569+214) / (1569+214+114+63)

# predTest = predict(NewsWordsRF, newdata=NewsTestWords, type="prob")
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predTest[,2])
# write.csv(MySubmission, "SubmissionRFBag.csv", row.names=FALSE)

# auc
NewsWordsRF.pred = predict(NewsWordsRF, type="prob")
ROCR.NewsWordsRF.pred = prediction(NewsWordsRF.pred[,2], NewsTrainWords$Popular)
auc2 = as.numeric(performance(ROCR.NewsWordsRF.pred, "auc")@y.values)
auc2

#####################################################################################
# randomForest simple: scores=0.62413, accuracy = 0.9137755, auc = 0.9309307
set.seed(12345)

NewsSimpleRF = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords)

pred = predict(NewsSimpleRF, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9186352
(3670+530)/(3670+530+235+137)

pred = predict(NewsSimpleRF, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9137755
(1571+220) / (1571+220+108+ 61)

# testRF.pred = predict(NewsSimpleRF, newdata=NewsTestWords, type="prob")
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testRF.pred[,2])
# write.csv(MySubmission, "SubmissionRFSimple.csv", row.names=FALSE)

# auc
ROCR.pred = prediction(pred[,2], NewsValidWords$Popular)
auc2 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc2

##############################################################################
# simple tree, accuracy = 0.9076531, auc = 0.9037486
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = c(seq(0.001,0.01,0.001), seq(0.01, 0.5, 0.01)) )

# Perform the cross validation
train(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

NewsWordsSimpleTree = rpart(Popular ~  NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrainWords, method="class", cp=0.002)
pred = predict(NewsWordsSimpleTree, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9245407
(3686+541)/(3686+541+ 224+121)

pred = predict(NewsWordsSimpleTree, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9096939
(1562+217) / (1562+217+111+ 70)

ROCR.CART.pred = prediction(pred[,2], NewsValidWords$Popular)
auc3 = as.numeric(performance(ROCR.CART.pred, "auc")@y.values)
auc3


# simple.testpred = predict(NewsWordsSimpleTree, newdata=NewsTestWords)
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = simple.testpred[,2])
# write.csv(MySubmission, "SubmissionCARTSimple.csv", row.names=FALSE)

#################################################################################
# add text: score = 0.61426, accuracy = 0.9091837, auc = 0.8953439
train(Popular ~ ., data=NewsTrainWords, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

NewsWordsTree = rpart(Popular~., data=NewsTrainWords, method="class", cp=0.002)
pred = predict(NewsWordsTree, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9210411
(3693+518)/(3693+518+114+247)

pred = predict(NewsWordsTree, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9091837
(1570+212) / (1570+212+116+ 62)

ROCR.CART.pred = prediction(pred[,2], NewsValidWords$Popular)
auc3 = as.numeric(performance(ROCR.CART.pred, "auc")@y.values)
auc3

# testpred = predict(NewsWordsTree, newdata=NewsTestWords)
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = simple.testpred[,2])
# write.csv(MySubmission, "SubmissionCARTBag.csv", row.names=FALSE)

# RandomForest with bag is best, but not better much than without, that means words is not importance?



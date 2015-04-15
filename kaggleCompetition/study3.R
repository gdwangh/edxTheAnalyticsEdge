# setwd("D:/workspace/The Analytics Edge/kaggleCompetition")
setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain$Popular  = as.factor( NewsTrain$Popular)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(NewsTrain)

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Month = NewsTrain$PubDate$mon
NewsTrain$Date = NewsTrain$PubDate$mday
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Hour = NewsTrain$PubDate$hour


# glm
NewsLog = glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Month+Date+Hour, data=NewsTrain, family=binomial)

library("ROCR")
NewsLog.pred = predict(NewsLog, type = "response")
ROCR.NewsLog.pred = prediction(NewsLog.pred, NewsTrain$Popular)
auc1 = as.numeric(performance(ROCR.NewsLog.pred, "auc")@y.values)
auc1  # 0.9311218

summary(NewsLog) # Month & mdate is not enough
NewsLog = glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour, data=NewsTrain, family=binomial)

NewsLog.pred = predict(NewsLog, type = "response")
ROCR.NewsLog.pred = prediction(NewsLog.pred, NewsTrain$Popular)
auc1 = as.numeric(performance(ROCR.NewsLog.pred, "auc")@y.values)
auc1  # 0.9329245

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
NewsTrainWords$NewsDesk = NewsTrain$NewsDesk
NewsTrainWords$SectionName = NewsTrain$SectionName
NewsTrainWords$SubsectionName = NewsTrain$SubsectionName
NewsTrainWords$Weekday = NewsTrain$Weekday
NewsTrainWords$Hour = NewsTrain$Hour

NewsTrainWordsLog = glm(Popular ~ ., data=NewsTrainWords, family=binomial)
NewsTrainWordsLog.pred = predict(NewsTrainWordsLog, type = "response")


# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

library(ROCR)
ROCR.NewsTrainWordsLog.pred = prediction(NewsTrainWordsLog.pred, NewsTrainWords$Popular)
auc1 = as.numeric(performance(ROCR.NewsTrainWordsLog.pred, "auc")@y.values)
auc1  # 0.9437113



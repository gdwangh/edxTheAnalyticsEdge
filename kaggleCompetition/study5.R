setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain$Popular  = as.factor( NewsTrain$Popular)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsCultureTrain = subset(NewsTrain, NewsDesk=="Culture")
NewsCultureTest = subset(NewsTest, NewsDesk=="Culture")

# bag: headline
library(tm)
library(SnowballC)

CorpusHeadline = Corpus(VectorSource(c(NewsCultureTrain$Headline, NewsCultureTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.98)

HeadlineWords = as.data.frame(as.matrix(sparse))
HeadlineWords$wordNum = rowSums(as.matrix(sparse))
colnames(HeadlineWords) = paste0("headline.", make.names(colnames(HeadlineWords)))

# bag: snippet
library(tm)
CorpusSnippet = Corpus(VectorSource(c(NewsCultureTrain$Snippet,NewsCultureTest$Snippet)))

CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

dtm = DocumentTermMatrix(CorpusSnippet)

sparse = removeSparseTerms(dtm, 0.96)

SnippetWords = as.data.frame(as.matrix(sparse))
SnippetWords$wordNum = rowSums(as.matrix(sparse))
colnames(SnippetWords) = paste0("Snippet.", make.names(colnames(SnippetWords)))

# bag: abstract
library(tm)
CorpusAbstract = Corpus(VectorSource(c(NewsCultureTrain$Abstract, NewsCultureTest$Abstract)))

CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtm = DocumentTermMatrix(CorpusAbstract)

sparse = removeSparseTerms(dtm, 0.96)

AbstractWords = as.data.frame(as.matrix(sparse))
AbstractWords$wordNum = rowSums(as.matrix(sparse))
colnames(AbstractWords) = paste0("Abstract.", make.names(colnames(AbstractWords)))

# NewsWords = cbind(HeadlineWords, SnippetWords, AbstractWords)
NewsWords = cbind(HeadlineWords, AbstractWords)

# split 
NewsTmpWords = head(NewsWords, nrow(NewsCultureTrain))
NewsTestWords = head(NewsWords, nrow(NewsCultureTest))

NewsTmpWords$Popular = NewsCultureTrain$Popular

NewsTmpWords$WordCount = NewsCultureTrain$WordCount
NewsTestWords$WordCount = NewsCultureTest$WordCount

newsDesk = as.factor(c(NewsCultureTrain$NewsDesk, NewsTest$NewsCultureTest))
NewsTmpWords$NewsDesk = head(newsDesk, nrow(NewsCultureTrain))
NewsTestWords$NewsDesk = head(newsDesk, nrow(NewsCultureTest))

SectionName = as.factor(c(NewsCultureTrain$SectionName, NewsCultureTest$SectionName))
NewsTmpWords$SectionName = head(SectionName, nrow(NewsCultureTrain))
NewsTestWords$SectionName = head(SectionName, nrow(NewsCultureTest))

SubsectionName = as.factor(c(NewsCultureTrain$SubsectionName, NewsCultureTest$SubsectionName))
NewsTmpWords$SubsectionName = head(SubsectionName, nrow(NewsCultureTrain))
NewsTestWords$SubsectionName = head(SubsectionName, nrow(NewsCultureTest))

pdate = strptime(NewsCultureTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTmpWords$Weekday = pdate$wday
NewsTmpWords$Hour = pdate$hour


pdate = strptime(NewsCultureTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTestWords$Weekday = pdate$wday
NewsTestWords$Hour = pdate$hour

# split NewsTrainWords to training set and validation set
library(caTools)
set.seed(123)
spl = sample.split(NewsTmpWords$Popular, 0.7)
NewsTrainWords = subset(NewsTmpWords, spl==TRUE)
NewsValidWords = subset(NewsTmpWords, spl==FALSE)

# randomForest simple
set.seed(12345)
# NewsSimpleRF = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour+headline.wordNum+Snippet.wordNum+Abstract.wordNum, data=NewsTrainWords)
NewsSimpleRF = randomForest(Popular~WordCount+Weekday+Hour+headline.wordNum+Snippet.wordNum+Abstract.wordNum, data=NewsTrainWords)

pred = predict(NewsSimpleRF, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9640592
(432+24)/(432+ 24+11+6)

pred = predict(NewsSimpleRF, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9458128
(188+4) / (188+4+11+ 0)

# testRF.pred = predict(NewsSimpleRF, newdata=NewsTestWords, type="prob")
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testRF.pred[,2])
# write.csv(MySubmission, "SubmissionRFSimple.csv", row.names=FALSE)

# auc, 0.7863475
ROCR.pred = prediction(pred[,2], NewsValidWords$Popular)
auc2 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc2

# randomForest with bag
set.seed(12345)
NewsRF = randomForest(Popular ~ ., data=NewsTrainWords)

pred = predict(NewsRF, type="prob")
table(NewsTrainWords$Popular, pred[,2]>0.5)  
# train accuracy = 0.9577167,0.9598309
(429+ 25)/(429+ 24+11+9)

pred = predict(NewsRF, newdata = NewsValidWords,type="prob")
table(NewsValidWords$Popular, pred[,2]>0.5)  
# valid accuracy = 0.9507389
(188+5) / (188+5+10+ 0)

# testRF.pred = predict(NewsSimpleRF, newdata=NewsTestWords, type="prob")
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testRF.pred[,2])
# write.csv(MySubmission, "SubmissionRFSimple.csv", row.names=FALSE)

# auc, 0.7904255
ROCR.pred = prediction(pred[,2], NewsValidWords$Popular)
auc2 = as.numeric(performance(ROCR.pred, "auc")@y.values)
auc2


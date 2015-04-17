# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/workspace/The Analytics Edge/kaggleCompetition")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)

NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTest$SectionName = as.factor(NewsTest$SectionName)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Hour = NewsTrain$PubDate$hour

NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$Hour = NewsTest$PubDate$hour

# split train data to training set and valid set
library(caTools)
set.seed(123)
spl = sample.split(NewsTrain$Popular, 0.7)
Train = subset(NewsTrain, spl==TRUE)
Valid = subset(NewsTrain, spl==FALSE)

# accuracy
SimpleLog = glm(Popular ~ WordCount+Weekday+Hour+NewsDesk+SectionName, data=Train, family="binomial")
Pred = predict(SimpleLog, newdata=Valid, type="response")
table(Valid$Popular, Pred>0.5)  
(1553+166)/nrow(Valid)  # 0.8770408
(1549+210)/nrow(Valid)  # 0.897449

# auc
library("ROCR")
SimpleLog.pred = predict(SimpleLog, type = "response")
ROCR.SimpleLog.pred = prediction(SimpleLog.pred, Train$Popular)
auc = as.numeric(performance(ROCR.SimpleLog.pred, "auc")@y.values)
auc  # 0.9155476

# predict test
PredTest = predict(SimpleLog, newdata=NewsTest, type="response")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionSimplestLog.csv", row.names=FALSE)

# headline bag, not good to acc & auc
library(tm)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

wordCounter = rowSums(HeadlineWords)
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

NewsTmpWords = head(HeadlineWords, nrow(NewsTrain))
NewsTestWords = head(HeadlineWords, nrow(NewsTest))

# add other variable
NewsTmpWords$Popular = NewsTrain$Popular
NewsTmpWords$WordCount = NewsTrain$WordCount
#NewsTmpWords$Weekday = NewsTrain$Weekday
NewsTmpWords$Hour = pdate$hour

NewsTestWords$Popular = NewsTest$Popular
NewsTestWords$WordCount = NewsTest$WordCount
#NewsTestWords$Weekday = NewsTest$Weekday
NewsTestWords$Hour = pdate$hour

# split train to training set and valid set
library(caTools)
set.seed(123)
spl = sample.split(NewsTmpWords$Popular, 0.7)
NewsTrainWords = subset(NewsTmpWords, spl==TRUE)
NewsValidWords = subset(NewsTmpWords, spl==FALSE)

# glm 
# HeadlineWordsLog = glm(Popular ~ ., data=NewsTrainWords, family=binomial)
HeadlineWordsLog = glm(Popular~WordCount+Hour+numWord, data=NewsTrainWords, family=binomial)

# accuracy
Pred = predict(HeadlineWordsLog, newdata=NewsValidWords, type="response")
table(NewsValidWords$Popular, Pred>0.5)  
(1588+28)/nrow(Valid)  # 0.8326531

HeadlineWordsLog.pred = predict(HeadlineWordsLog, type = "response")
ROCR.HeadlineWordsLog.pred = prediction(HeadlineWordsLog.pred, HeadlineWordsTrain$Popular)
auc3 = as.numeric(performance(ROCR.HeadlineWordsLog.pred, "auc")@y.values)
auc3  # 0.7904727

# setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

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

# # trans to factor
newsTrain$PopularFactor = as.factor(ifelse(newsTrain$Popular,"Yes", "No"))

newsTrain$NewsDeskFactor = as.factor(newsTrain$NewsDesk)
newsTest$NewsDeskFactor = factor(newsTest$NewsDesk, levels = levels(newsTrain$NewsDeskFactor))

newsTrain$SectionNameFactor = as.factor(newsTrain$SectionName)
newsTest$SectionNameFactor = factor(newsTest$SectionName, levels = levels(newsTrain$SectionNameFactor))


newsTrain$SubsectionNameFactor = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionNameFactor = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionNameFactor))



# check question words or ? in the headline
newsTrain$HeadlineIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsQuestion))

table(newsTrain$HeadlineIsQuestion, newsTrain$Popular)
tapply(newsTrain$Popular,newsTrain$HeadlineIsQuestion,mean)

# rf
library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)

rfGrid<- expand.grid(mtry=c(10:20))

set.seed(1000)
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour+HeadlineIsQuestion,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")

rfGrid<- expand.grid(mtry=c(14))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9054244

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/addFeatureQuestion.csv", row.names=FALSE)

#############################################################################
# text conpus
library(tm)
CorpusHeadline = Corpus(VectorSource(c(newsTrain$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# dtm = DocumentTermMatrix(CorpusHeadline, control=list(weighting=function(x) weightTfIdf(x, normalize=FALSE))) 

require(RWeka)
library(rJava)
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))  
dtm <- DocumentTermMatrix(CorpusHeadline, control = list(tokenize = TrigramTokenizer, weighting=function(x) weightTfIdf(x, normalize=FALSE)))

sparse = removeSparseTerms(dtm, 0.995)

# HeadlineWords = as.data.frame(as.matrix(dtm))
HeadlineWords = as.data.frame(as.matrix(sparse))

library(wordcloud)
wordcloud(colnames(HeadlineWords), colSums(HeadlineWords))

sort(colSums(HeadlineWords))

# popular words
popWords = c("week", "new york", "fashion", "daily", "day", "report", "today", "2015","Business","Spring/Summer","first","bank","2014","obama")
newsTrain$HeadlineIspopWords = as.factor(as.numeric(grepl(paste(popWords, collapse="|"), newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIspopWords = factor(as.numeric(grepl(paste(popWords, collapse="|"), newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIspopWords))

flag = as.factor(as.numeric(grepl("obama", newsTrain$Headline, ignore.case = TRUE) ))

table(flag, newsTrain$PopularFactor)
tapply(newsTrain$Popular,flag,mean)

table(newsTrain$HeadlineIspopWords, newsTrain$PopularFactor)
tapply(newsTrain$Popular,newsTrain$HeadlineIspopWords,mean)


newsTrain[HeadlineWords$springsumm>0, "Headline"]

library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)

rfGrid<- expand.grid(mtry=c(10:20))

set.seed(1000)
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour+HeadlineIsQuestion+HeadlineIspopWords,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")

rfGrid<- expand.grid(mtry=c(19))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9350237

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/addFeature.csv", row.names=FALSE)


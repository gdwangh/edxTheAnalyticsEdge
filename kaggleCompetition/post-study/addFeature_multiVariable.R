setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

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
newsTrain$HeadlineIsWeek = as.factor(as.numeric(grepl("week", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsWeek = factor(as.numeric(grepl("week", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsWeek))

newsTrain$HeadlineIsNY = as.factor(as.numeric(grepl("new york", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsNY = factor(as.numeric(grepl("new york", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsNY))

newsTrain$HeadlineIsfashion = as.factor(as.numeric(grepl("fashion", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsfashion = factor(as.numeric(grepl("fashion", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsfashion))

newsTrain$HeadlineIsday = as.factor(as.numeric(grepl("day", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsday = factor(as.numeric(grepl("day", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsday))

newsTrain$HeadlineIsreport = as.factor(as.numeric(grepl("report", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsreport = factor(as.numeric(grepl("report", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsreport))

newsTrain$HeadlineIstoday = as.factor(as.numeric(grepl("today", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIstoday = factor(as.numeric(grepl("today", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIstoday))

newsTrain$HeadlineIs2015 = as.factor(as.numeric(grepl("2015", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIs2015 = factor(as.numeric(grepl("2015", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIs2015))

newsTrain$HeadlineIsBusiness = as.factor(as.numeric(grepl("Business", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsBusiness = factor(as.numeric(grepl("Business", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsBusiness))

newsTrain$HeadlineIsSpringSummer = as.factor(as.numeric(grepl("Spring/Summer", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsSpringSummer = factor(as.numeric(grepl("Spring/Summer", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsSpringSummer))

newsTrain$HeadlineIsfirst = as.factor(as.numeric(grepl("first", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsfirst = factor(as.numeric(grepl("first", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsfirst))

newsTrain$HeadlineIsbank = as.factor(as.numeric(grepl("bank", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsbank = factor(as.numeric(grepl("bank", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsbank))

newsTrain$HeadlineIs2014 = as.factor(as.numeric(grepl("2014", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIs2014 = factor(as.numeric(grepl("2014", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIs2014))

newsTrain$HeadlineIsobama = as.factor(as.numeric(grepl("obama", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsobama = factor(as.numeric(grepl("obama", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsobama))

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
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour+HeadlineIsQuestion+HeadlineIsWeek+HeadlineIsNY+HeadlineIsfashion+HeadlineIsday+HeadlineIsreport+HeadlineIstoday+HeadlineIs2015+HeadlineIsBusiness+HeadlineIsSpringSummer+HeadlineIsfirst+HeadlineIsbank+HeadlineIs2014+HeadlineIsobama,
              data = newsTrain,
              method="rf", 
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")

rfGrid<- expand.grid(mtry=c(16))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9130969

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/addFeature.csv", row.names=FALSE)

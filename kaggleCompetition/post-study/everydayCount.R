setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
#setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

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

newsTrain$AbstractIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Abstract, ignore.case = TRUE) ))
newsTest$AbstractIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Abstract, ignore.case = TRUE)), levels = levels(newsTrain$AbstractIsQuestion))



#############################################################################
# text conpus
library(tm)
require(RWeka)
library(rJava)
library(wordcloud)

TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))  

cal_text_corpus = function(text, throld) {
  Corpus = Corpus(VectorSource(c(text)))
  
  # You can go through all of the standard pre-processing steps like we did in Unit 5:
  Corpus = tm_map(Corpus, tolower)
  Corpus = tm_map(Corpus, PlainTextDocument)
  Corpus = tm_map(Corpus, removePunctuation)
  Corpus = tm_map(Corpus, removeWords, stopwords("english"))
  Corpus = tm_map(Corpus, stemDocument)
  
  dtm <- DocumentTermMatrix(Corpus, control = list(tokenize = TrigramTokenizer, weighting=function(x) weightTfIdf(x, normalize=FALSE)))
  
  sparse = removeSparseTerms(dtm, throld)
  
  textWords = as.data.frame(as.matrix(sparse))
  
  #   wordcloud(colnames(textWords), colSums(textWords))
  #   sort(colSums(textWords))
  
  as.factor(ifelse( rowSums(textWords)==0, "No", "Yes"))
}

# popular words
HeadlineIsPop = cal_text_corpus(c(newsTrain$Headline, newsTest$Headline),0.985)
SnippetIsPop = cal_text_corpus(c(newsTrain$Snippet, newsTest$Snippet),0.965)
AbstractIsPop = cal_text_corpus(c(newsTrain$Abstract, newsTest$Abstract),0.965)

newsTrain$headlineIsPopWord = head(HeadlineIsPop, nrow(newsTrain))
newsTest$headlineIsPopWord = tail(HeadlineIsPop, nrow(newsTest))

newsTrain$SnippetIsPop = head(SnippetIsPop, nrow(newsTrain))
newsTest$SnippetIsPop = tail(SnippetIsPop, nrow(newsTest))

newsTrain$AbstractIsPop = head(AbstractIsPop, nrow(newsTrain))
newsTest$AbstractIsPop = tail(AbstractIsPop, nrow(newsTest))

# emotion
library(qdap)
# pol<- polarity(paste(newsTrain$Headline, newsTrain$Snippet,".") )
pol<- polarity(newsTrain$Headline)
newsTrain$HSpolarity = pol[[1]]$polarity
newsTrain[is.na(newsTrain$HSpolarity), ]$HSpolarity = 0

pol<- polarity(newsTest$Headline)
newsTest$HSpolarity = pol[[1]]$polarity
newsTest[is.na(newsTest$HSpolarity), ]$HSpolarity = 0

pol<- polarity(newsTrain$Abstract)
newsTrain$AbstractPolarity = pol[[1]]$polarity
newsTrain[is.na(newsTrain$AbstractPolarity), ]$AbstractPolarity = 0

pol<- polarity(newsTest$Abstract)
newsTest$AbstractPolarity = pol[[1]]$polarity
newsTest[is.na(newsTest$AbstractPolarity), ]$AbstractPolarity = 0

# cal the count of every day
newsTrain$yday = newsTrain$PubDate$yday
newsTest$yday = newsTest$PubDate$yday

library(dplyr)
newsTrain_state<- tbl_df(newsTrain) %>% select(yday, Popular) %>% group_by(yday) %>% 
  summarise(count = n(), 
            cnt1 = sum(Popular==1, na.rm = TRUE),  
            cnt0 = sum(Popular==0, na.rm = TRUE),
            Pop_perc = mean(Popular, na.rm = TRUE))
cor(newsTrain_state[2:5])  # Pop_perc and count有很强的副相关

newsTrain_state<- tbl_df(newsTrain) %>% select(yday) %>% group_by(yday) %>% 
  summarise(count = n())

tmp = merge(newsTrain[,c("UniqueID","yday")], newsTrain_state[,c(1,2)], by="yday")
newsTrain$everydayCount = tmp$count
  
newsTest_state<- tbl_df(newsTest) %>% select(yday) %>% group_by(yday) %>% 
  summarise(count = n())
tmp = merge(newsTest[,c("UniqueID","yday")], newsTest_state, by="yday")
newsTest$everydayCount = tmp$count

# rf
library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)

rfGrid<- expand.grid(mtry=c(1:20))

set.seed(1000)
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+logWordCount+Weekday+Hour+HeadlineIsQuestion+AbstractIsQuestion+headlineIsPopWord+SnippetIsPop+AbstractIsPop+AbstractPolarity+HSpolarity+everydayCount,
              data = newsTrain,
              method="rf",
              importance=TRUE,
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")

rfGrid<- expand.grid(mtry=c(11))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.941912

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/emotion.csv", row.names=FALSE)
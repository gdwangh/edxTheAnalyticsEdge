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
# not significant
# newsTrain$HeadlineIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Headline, ignore.case = TRUE) ))
# newsTest$HeadlineIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsQuestion))

# table(newsTrain$HeadlineIsQuestion, newsTrain$Popular)
# tapply(newsTrain$Popular,newsTrain$HeadlineIsQuestion,mean)
# 
# newsTrain$AbstractIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Abstract, ignore.case = TRUE) ))
# newsTest$AbstractIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Abstract, ignore.case = TRUE)), levels = levels(newsTrain$AbstractIsQuestion))



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

# not significant
# newsTrain$SnippetIsPop = head(SnippetIsPop, nrow(newsTrain))
# newsTest$SnippetIsPop = tail(SnippetIsPop, nrow(newsTest))
# 
# newsTrain$AbstractIsPop = head(AbstractIsPop, nrow(newsTrain))
# newsTest$AbstractIsPop = tail(AbstractIsPop, nrow(newsTest))

rm(HeadlineIsPop, SnippetIsPop, AbstractIsPop)

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
newsTrain_state<- tbl_df(newsTrain) %>% select(yday) %>% group_by(yday) %>% 
  summarise(count = n())

tmp = merge(newsTrain[,c("UniqueID","yday")], newsTrain_state[,c(1,2)], by="yday")
newsTrain$everydayCount = tmp$count

newsTest_state<- tbl_df(newsTest) %>% select(yday) %>% group_by(yday) %>% 
            summarise(count = n())
tmp = merge(newsTest[,c("UniqueID","yday")], newsTest_state, by="yday")
newsTest$everydayCount = tmp$count

rm(tmp)

## last - how much time has passed till publication of the last article
## the idea is that the bigger the number of articles published 
## the lesser the average popularity as attention is spread to more articles
library(zoo)
newsAll = rbind(newsTrain[,c("UniqueID","PubDate", "Weekday", "Hour")], 
                newsTest[,c("UniqueID","PubDate", "Weekday", "Hour")])
newsAll = newsAll[order(newsAll$PubDate),]

pd = as.POSIXlt( newsAll$PubDate )
z = zoo(as.numeric(pd))   # create time zoo list by pubdate
n = nrow(newsAll)
b = zoo(, seq(n))

# cal the time zoo interval units between a item and other item lag 1/3/5/10/20/50 element
newsAll$last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
newsAll$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
newsAll$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
newsAll$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
newsAll$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
newsAll$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))

# cal the average of last1/3/5/10/20/50
last.avg = newsAll[, c("Weekday", "Hour", "last1", "last3", "last5", "last10", "last20", "last50")] %>% 
          group_by(Weekday, Hour) %>% dplyr::summarise(
  last1.avg=mean(last1, na.rm=TRUE),
  last3.avg=mean(last3, na.rm=TRUE),
  last5.avg=mean(last5, na.rm=TRUE),
  last10.avg=mean(last10, na.rm=TRUE),
  last20.avg=mean(last20, na.rm=TRUE),
  last50.avg=mean(last50, na.rm=TRUE)
)

# replace na with average 
na.merge = merge(newsAll, last.avg, by=c("Weekday","Hour"))
na.merge = na.merge[order(na.merge$UniqueID),]

for(col in c("last1", "last3", "last5", "last10", "last20", "last50")) {
  y = paste0(col, ".avg")
  idx = is.na(newsAll[[col]])   # get the column named col and check NA
  newsAll[idx,][[col]] <- na.merge[idx,][[y]]   # set the NA with average
}

newsTrain = merge(newsTrain, newsAll[,c("UniqueID","last1", "last3", "last5", "last10", "last20", "last50")], by="UniqueID")
newsTest = merge(newsTest, newsAll[,c("UniqueID","last1", "last3", "last5", "last10", "last20", "last50")], by="UniqueID")

cor(newsTrain[,c(10,23:28)])

rm(last.avg, na.merge, b, i, idx, n, pd, y, z)

boxplot(newsTrain$Popular, newsTrain$last10)

glmFit = glm(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
                          logWordCount+Weekday+Hour+headlineIsPopWord+
                          AbstractPolarity+HSpolarity+
                          last20,
             data = newsTrain,  family=binomial)
summary(glmFit)

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
rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
                            logWordCount+Weekday+Hour+headlineIsPopWord+
                            AbstractPolarity+HSpolarity+everydayCount+
                            last20,
              data = newsTrain,
              method="rf",
              importance=TRUE,
              trControl=ensCtrl,
              tuneGrid=rfGrid,
              metric="ROC")

rfGrid<- expand.grid(mtry=c(14))

pred.rf <- predict(rfFit, newdata=newsTrain, type='prob')

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, pred.rf[,2]>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9930345

pred.test = predict(rfFit, newdata=newsTest, type="prob")
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test[,2])
write.csv(MySubmission, "post-study/zoo.csv", row.names=FALSE)

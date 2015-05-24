# setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(newsTrain)
summary(newsTrain)

# NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
# SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
# SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
# Headline = the title of the article
# Snippet = a small portion of the article text
# Abstract = a summary of the blog article, written by the New York Times
# WordCount = the number of words in the article
# PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
# UniqueID = a unique identifier for each article

# short string, have 13 different value, have 1846 empty
table(newsTrain$NewsDesk)    
table(newsTest$NewsDesk)    # have 562 empty, 11 value

# short string, having 16 different value, have 2300 empty
table(newsTrain$SectionName) 
table(newsTest$SectionName)   # have 599 empty, 14 value

# short string, have 9 different value, have 4826 empty
table(newsTrain$SubsectionName) 
table(newsTest$SubsectionName)  # have 1350 empty, 7 value

# long text, length:[4,124]
summary(nchar(newsTrain$Headline))
summary(nchar(newsTest$Headline))  # [7, 101]
newsTrain$HeadlineCount = nchar(newsTrain$Headline)
newsTest$HeadlineCount = nchar(newsTest$Headline)

# long text, length: [0, 253]
summary(nchar(newsTrain$Snippet))
summary(nchar(newsTest$Snippet))  # [0,252]
newsTrain$SnippetCount = nchar(newsTrain$Snippet)
newsTest$SnippetCount = nchar(newsTest$Snippet)

# long text, length: [0, 581]
summary(nchar(newsTrain$Abstract))
summary(nchar(newsTest$Abstract))  # [0, 381]
newsTrain$AbstractCount = nchar(newsTrain$Abstract)
newsTest$AbstractCount = nchar(newsTest$Abstract)

# int, [0,10910], 
summary(newsTrain$WordCount)  
summary(newsTest$WordCount)  # [0, 5956]

hist(newsTrain$WordCount,breaks=100)  # 左偏的很厉害，试试log
hist(log(1+newsTrain$WordCount),breaks=100) # 好多了，稍微右偏
newsTrain$logCount = log(1+newsTrain$WordCount)
newsTest$logCount = log(1+newsTest$WordCount)


table(newsTrain$Popular)    # int flag 0/1

# get info from pubdate
newsTrain$PubDate = strptime(newsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
newsTest$PubDate = strptime(newsTest$PubDate, "%Y-%m-%d %H:%M:%S")

hist(newsTrain$PubDate$wday)
table(newsTrain$PubDate$wday)  # weekday: [0,6], 工作日多，周末少
table(newsTest$PubDate$wday)
newsTrain$Weekday = newsTrain$PubDate$wday
newsTest$Weekday = newsTest$PubDate$wday

hist(newsTrain$PubDate$hour)
table(newsTrain$PubDate$hour)  # hour: [0,23]，工作时间多，周末少，近似正态分布
table(newsTest$PubDate$hour)   # [0,23]
newsTrain$Hour = newsTrain$PubDate$hour
newsTest$Hour = newsTest$PubDate$hour


hist(newsTrain$PubDate$mday)
table(newsTrain$PubDate$mday) # monthday: [1,31], 无明显差别
table(newsTest$PubDate$mday)  # [0,31]
newsTrain$monthday = newsTrain$PubDate$mday

hist(newsTrain$PubDate$mon)  
table(newsTrain$PubDate$mon)  # monthday: [8,10], 无明显差异
table(newsTest$PubDate$mon)   # =11
newsTrain$month = newsTrain$PubDate$mon

table(newsTrain$PubDate$year)  # 2014
table(newsTest$PubDate$year)  # 2014
newsTrain$year = newsTrain$PubDate$year


# select significant feature
glmFit = glm(Popular~NewsDesk+SectionName+SubsectionName+WordCount+logCount+Weekday+Hour+monthday+year+HeadlineCount+SnippetCount+AbstractCount, data=newsTrain, family=binomial )
summary(glmFit)
# WordCount, logCount, monthday, year, HeadlineCount, SnippetCount, AbstractCount is not significant
newsTrain$monthday = NULL
newsTrain$year = NULL
newsTrain$HeadlineCount = NULL
newsTrain$SnippetCount=NULL
newsTrain$AbstractCount = NULL

# trans to factor
newsTrain$Popular = as.factor(newsTrain$Popular)

newsTrain$NewsDesk = as.factor(newsTrain$NewsDesk)
newsTest$NewsDesk = factor(newsTest$NewsDesk, levels = levels(newsTrain$NewsDesk))

newsTrain$SectionName = as.factor(newsTrain$SectionName)
newsTest$SectionName = factor(newsTest$SectionName, levels = levels(newsTrain$SectionName))


newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))

newsTrain$SubsectionName = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionName = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionName))


glmFit = glm(Popular~NewsDesk+SectionName+SubsectionName+logCount+Weekday+Hour, data=newsTrain, family=binomial )
summary(glmFit)

pred = predict(glmFit, type="response")
# auc
library("ROCR")
ROCR.SimpleRF.Pred = prediction(pred, newsTrain$Popular)
auc = as.numeric(performance(ROCR.SimpleRF.Pred, "auc")@y.values)
auc  # 0.9340012





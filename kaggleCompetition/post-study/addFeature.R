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

# trans to factor
newsTrain$PopularFactor = as.factor(ifelse(newsTrain$Popular,"Yes", "No"))

# add empty NewsDesk & sectionName
allCat = rbind(newsTrain[,c("NewsDesk", "SectionName","SubsectionName")], newsTest[,c("NewsDesk", "SectionName","SubsectionName"), ])

table(allCat$SectionName,allCat$NewsDesk)

allCat[allCat$NewsDesk == "Business" & allCat$SectionName=="",]$SectionName = "Business Day"
allCat[allCat$NewsDesk == "Culture" & allCat$SectionName=="",]$SectionName = "Arts"
allCat[allCat$NewsDesk == "Foreign" & allCat$SectionName=="",]$SectionName = "World"
allCat[allCat$NewsDesk == "National" & allCat$SectionName=="",]$SectionName = "U.S."
allCat[allCat$NewsDesk == "OpEd" & allCat$SectionName=="",]$SectionName = "Opinion"
allCat[allCat$NewsDesk == "Science" & allCat$SectionName=="",]$SectionName = "Health"
allCat[allCat$NewsDesk == "Sports" & allCat$SectionName=="",]$SectionName = "Sports"
allCat[allCat$NewsDesk == "Styles" & allCat$SectionName=="",]$SectionName = "U.S."
allCat[allCat$NewsDesk == "TStyle" & allCat$SectionName=="",]$SectionName = "TStyle"

# TStyle all empty

allCat[allCat$NewsDesk == "" & allCat$SectionName=="Arts",]$NewsDesk = "Culture"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Business Day",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Crosswords/Games",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Health",]$NewsDesk = "Science"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="N.Y. / Region",]$NewsDesk = "Metro"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Open",]$NewsDesk = "Open"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Opinion",]$NewsDesk = "OpEd"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Travel",]$NewsDesk = "Travel"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Technology",]$NewsDesk = "Business"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="U.S.",]$NewsDesk = "Styles"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="World",]$NewsDesk = "Foreign"
allCat[allCat$NewsDesk == "" & allCat$SectionName=="Multimedia",]$NewsDesk = "Multimedia"

allCat$NewsDesk = as.factor(allCat$NewsDesk)
allCat$SectionName = as.factor(allCat$SectionName)
allCat$SubsectionName = as.factor(allCat$SubsectionName)

newsTrain$NewsDeskFactor = head(allCat$NewsDesk, nrow(newsTrain))
newsTest$NewsDeskFactor = tail(allCat$NewsDesk, nrow(newsTest))

newsTrain$SectionNameFactor = head(allCat$SectionName, nrow(newsTrain))
newsTest$SectionNameFactor = tail(allCat$SectionName, nrow(newsTest))

newsTrain$SubsectionNameFactor = head(allCat$SubsectionName, nrow(newsTrain))
newsTest$SubsectionNameFactor = tail(allCat$SubsectionName, nrow(newsTest))

# 
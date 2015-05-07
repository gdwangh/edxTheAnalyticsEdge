setwd("D:/workspace/The Analytics Edge/kaggleCompetition")
#setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(tm)

# all <- rbind(newsTrain[,!(colnames(newsTrain) %in% "Popular")], newsTest)
# CorpusHeadline = Corpus(VectorSource(c(all$Headline)))

# Popu = subset(newsTrain, Popular==1)
# CorpusHeadline = Corpus(VectorSource(c(Popu$Headline)))
# CorpusHeadline = Corpus(VectorSource(c(newsTrain$Headline)))
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

sparse = removeSparseTerms(dtm, 0.999)

# HeadlineWords = as.data.frame(as.matrix(dtm))
HeadlineWords = as.data.frame(as.matrix(sparse))

library(wordcloud)
wordcloud(colnames(HeadlineWords), colSums(HeadlineWords))

sort(colSums(HeadlineWords))

flag <- as.factor(ifelse(grepl("No Comment Necessary", newsTrain$Headline, ignore.case=TRUE)==TRUE, "Yes","No"))
table(newsTrain$Popular, flag)
prop.table(table(newsTrain$Popular, flag),2)
tapply(newsTrain$Popular,flag,mean)

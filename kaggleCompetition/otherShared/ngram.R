setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

newsTrain$group = 1
newsTest$group= 0 
alldata <- bind_rows(newsTrain,newsTest)

library(tm)
library(tau)
library(dplyr)
library(tidyr)

corpus_hl <- Corpus(VectorSource(alldata$Headline))
corpus_hl <- tm_map(corpus_hl,tolower)
corpus_hl <- tm_map(corpus_hl,PlainTextDocument)
corpus_hl <- tm_map(corpus_hl,removePunctuation)
corpus_hl <- tm_map(corpus_hl,removeWords, stopwords("english"))
corpus_hl <- tm_map(corpus_hl,stemDocument)

headlines <- data.frame(text=sapply(1:nrow(alldata),function (x) unlist(corpus_hl[[x]][[1]])),
                        stringsAsFactors=F)


#calculateth n-gram combos
ngrams2_hl = sapply(headlines$text,textcnt,method='string',n=2)
ngrams3_hl = sapply(headlines$text,textcnt,method='string',n=3)

# change the row's columns holding ngram into the rows 
mydf <- function(data, row_id) {
  if (length(data)>0) {
    data.frame(ngram=names(data),
               freq=as.numeric(data),index=row_id,
               stringsAsFactors=F)
  }
}

tmp2_hl = bind_rows(Map(mydf,ngrams2_hl, 1:nrow(headlines)))
tmp3_hl = bind_rows(Map(mydf,ngrams3_hl, 1:nrow(headlines)))

# combine and summary the ngram in one case.
ngrams_hl <- bind_rows(tmp2_hl,tmp3_hl) %>% mutate(ngram=paste0(gsub(' ','.',ngram))) %>%
  group_by(index,ngram) %>% summarise(freq=sum(freq)) 

# summary the same ngram and calc percentage in all case, and then sort ngram descreasing
top_hl <- ngrams_hl %>% group_by(ngram) %>% summarise(P=n()/nrow(headlines)) %>% ungroup() %>% arrange(desc(P))

# check and get the top ngrams 
sum(top_hl$P >=0.005) # 

# get the top ngrams and change them from rows to columns
ngrams_hl1 <- ngrams_hl %>% filter(ngram %in% top_hl$ngram[top_hl$P >=0.005]) %>% spread(ngram,freq,fill=0)
colSums(ngrams_hl1)

alldata$index = as.numeric(row.names(alldata))
alldata = merge(alldata, ngrams_hl1, all.x=TRUE)

#create a new test and train from modeling3
train <- subset(alldata,group==1,-c(group,index))
train$Popular <- as.factor(train$Popular)
test <- subset(alldata,group==0,-c(group,Popular,index))



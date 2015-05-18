library(tm)

assign.clusters <- function( all, k, cutoff, prefix) {
  corpus = Corpus(VectorSource( all$Abstract ))
  
  # You can go through all of the standard pre-processing steps like we did in Unit 5:
  corpus = tm_map(corpus, tolower)
  
  # Remember this extra line is needed after running the tolower step:
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stemDocument)
  
  # Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
  # We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!
  
  dtm = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(dtm, cutoff)
  words = as.data.frame(as.matrix(sparse))
  #  colnames(words) = paste(prefix, colnames(words))
  
  # Let's make sure our variable names are okay for R:
  colnames(words) = make.names(colnames(words))
  
  # cluster
  #   k = 17
  kmc = kmeans(words, centers = k, iter.max = 2000)
  
  return(kmc$cluster)
}


######
#
######
source("Utils.R")

news.train = read.csv( "NYTimesBlogTrain.csv", stringsAsFactors = FALSE )
news.test  = read.csv( "NYTimesBlogTest.csv",  stringsAsFactors = FALSE )

# add a fake Popular columnw to news.test so everything is the same shape
library(dplyr)
news.test = mutate( news.test, Popular = -1 )

# sample it
library(caTools)
set.seed(1002)
split = sample.split( news.train$Popular, SplitRatio = 0.7 )
train = subset( news.train, split == TRUE )
test = subset( news.train, split == FALSE )

# rebind it into one big frame
all = rbind( train, test, news.test) 

end_trn  = nrow(train)
end_test = nrow(test)      + end_trn
end      = nrow(news.test) + end_test

# sections
all = mutate( all
              , NewsDesk       = as.factor( NewsDesk )
              , SectionName    = as.factor( SectionName )
              , SubsectionName = as.factor( SubsectionName )
              # word count
              , logWordCount   = round(log( WordCount+1 ))
              # text analysis
              , question       = grepl( "[?]$",  Headline )
              , contains.date  = grepl( "201.$", Headline )
              , contains.daily = grepl( "Daily", Headline )
              , hl.length      = nchar( Headline )
              # publish time
              , hour           = strptime( PubDate, "%Y-%m-%d %H:%M:%S" )$hour
              , wday           = strptime( PubDate, "%Y-%m-%d %H:%M:%S" )$wday
              , weekend        = (wday == 0 | wday == 6)
              , weekday        = (!weekend)
              , primetime      = (weekday & (hour >= 5 & hour <= 18)) | (wday == 0 & hour >= 17)
              , daytime        = hour > 4 & hour <= 18
)

# compute boring & interesting-ness
source("BoringWords.R")
source("InterestingWords.R")

library(stringr)
all = mutate( all, Headline    = gsub("[[:punct:]]", "", tolower(Headline)) )
all = mutate( all, Headline    = str_trim(Headline) )
all = mutate( all, boring      = grepl( paste( boring.words, collapse="|"), Headline ) )
all = mutate( all, interesting = grepl( paste( interesting.words, collapse="|"), Headline ) )
#all = mutate( all, first.word  = sapply( strsplit(Headline, " "), '[', 1 ) )

#
k = 31
x = assign.clusters( all, k, 0.99, "A")
all$cluster = x


# # headline analysis
# p = select( arrange(filter(all, Popular==1, interesting==FALSE), Headline), Headline)
# u = select( arrange(filter(all, Popular==0, boring==FALSE), Headline), Headline)
# write.csv(p, "popular.headlines.csv")
# write.csv(u, "unpopular.headlines.csv")

# # snippet analysis
# p = select( arrange(filter(all, Popular==1), Headline), Headline)
# u = select( arrange(filter(all, Popular==0, boring==FALSE), Headline), Headline)
# write.csv(p, "popular.headlines.csv")
# write.csv(u, "unpopular.headlines.csv")

# set aside any columns we'll want later
keep = select(all, UniqueID)

library(dplyr)
all = select(all
             , Popular
             , NewsDesk
             , SectionName
             , SubsectionName
             , WordCount
             , logWordCount
             , primetime
             , hour
             , wday
             , question
             , hl.length
             , boring
             , interesting
             , cluster
)

train = slice( all, 1:end_trn )
test  = slice( all, (end_trn+1) :end_test )

####
# random forest
####

library(randomForest)
x = tuneRF(train, train$Popular)
set.seed(1002)
model = randomForest( Popular ~ ., data=train, type="classification", ntree=1000, nodesize=3)
pred1 = pred = predict( model, newdata=test, type="response" )
Utils.GetAuc( pred, test$Popular )

# train on ALL the training data
train = slice( all, 1:end_test)
x = tuneRF(train, train$Popular)
model = randomForest( Popular ~ ., data=train, type="classification", ntree=1000, nodesize=3)

news.test = slice( all, (end_test+1):end )
news.test$UniqueID = keep$UniqueID[(end_test+1):end]

# Now we can prepare our submission file for Kaggle:
pred.test = abs(predict( model, newdata=news.test, type="response" ))
MySubmission = data.frame(UniqueID = news.test$UniqueID, Probability1 = pred.test)
write.csv(MySubmission, "Submission17.csv", row.names=FALSE)
# Read data and make sure system language for analysis
Sys.setlocale('LC_ALL', 'English')

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Load the "tm" package

library(tm)

# Take Headline text
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)



dtm = DocumentTermMatrix(CorpusHeadline)

# Change the proportion in removeSparseTerms
sparse = removeSparseTerms(dtm, 0.995)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# HeadlineWordsTrain$Popular = as.factor(NewsTrain$Popular) use this for not overing prob(0-1)

HeadlineWordsTrain$Popular = NewsTrain$Popular # use for better performance but need to replace number <0 in prediction

# Change variable WordCount value
HeadlineWordsTrain$WordCount = log(NewsTrain$WordCount+1)
HeadlineWordsTest$WordCount = log(NewsTest$WordCount+1)


# Add variable as factor and clean the data
# Add time
library(lubridate)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, format="%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, format="%Y-%m-%d %H:%M:%S")


HeadlineWordsTrain$Weekdays = as.factor(weekdays(NewsTrain$PubDate))
HeadlineWordsTrain$Hour = as.factor(hour(NewsTrain$PubDate))

HeadlineWordsTest$Weekdays = as.factor(weekdays(NewsTest$PubDate))
HeadlineWordsTest$Hour = as.factor(hour(NewsTest$PubDate))


# Add blog's category
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName)

NewsTest$NewsDesk = factor(NewsTest$NewsDesk, levels(NewsTrain$NewsDesk))
NewsTest$SectionName = factor(NewsTest$SectionName, levels(NewsTrain$SectionName))
NewsTest$SubsectionName = factor(NewsTest$SubsectionName, levels(NewsTrain$SubsectionName))


HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk
HeadlineWordsTrain$SectionName = NewsTrain$SectionName
HeadlineWordsTrain$SubsectionName = NewsTrain$SubsectionName
HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk
HeadlineWordsTest$SectionName = NewsTest$SectionName
HeadlineWordsTest$SubsectionName = NewsTest$SubsectionName


# Use RandomForest model and use Parallel computing packages
library(foreach)
library(doParallel)
library(randomForest)

# registerDoParallel(x).  x depends on number of your cpu's cores
registerDoParallel(6)

# ntree=rep(1700, x) depends on number of your cpu's cores. 1700* x = tree number  
# mtry = 8 from tuneRF packages 
HeadlineWordsrf = foreach(ntree=rep(1667, 6), .combine=combine, .packages = "randomForest") %dopar%
        
        randomForest(Popular ~ ., data=HeadlineWordsTrain,mtry = 8,ntree=ntree,allowParallel=TRUE)
# use glm model
HeadlineWordslog = glm(Popular ~ ., data=HeadlineWordsTrain,family="binomial")

# use gbm model # n.core(x) x depends on number of your cpu's cores
library(gbm)
HeadlineWordsgbm = gbm(Popular ~.  ,data=HeadlineWordsTrain,n.tree= 10000, shrinkage = 0.001, n.core = 6)

# And make predictions on our test set:
PredTest1 = predict(HeadlineWordsgbm, newdata=HeadlineWordsTest, type="response",n.tree = 10000)
PredTest2 = predict(HeadlineWordsrf, newdata=HeadlineWordsTest, type="response")
PredTest3 = predict(HeadlineWordslog, newdata=HeadlineWordsTest, type="response")

newpredict = (0.15*PredTest1 + 0.7*PredTest2 +0.15*PredTest3)

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = newpredict)

write.csv(MySubmission, "SubmissionHeadlinerfgbmlog.csv", row.names=FALSE)

# AUC = 0.92830  451th in public borad
# AUC = 0.91306  20th  in private borad

#Kaggle Competition - predicting which New York Times blog articles will be the most popular

#Loading training and testing set
Sys.setlocale("LC_ALL", "C")
train = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
test = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

#Converting the date/time
train$PubDate = strptime(train$PubDate, "%Y-%m-%d %H:%M:%S")
test$PubDate = strptime(test$PubDate, "%Y-%m-%d %H:%M:%S")

#Preparing the corpus in the training set
require(tm)
corpusHeadLine <- Corpus(VectorSource(c(train$Headline,test$Headline)))
corpusSnippet <- Corpus(VectorSource(c(train$Snippet,test$Snippet)))
corpusAbstract <- Corpus(VectorSource(c(train$Abstract,test$Abstract)))
corpusHeadLine <- tm_map(corpusHeadLine,tolower)
corpusSnippet <- tm_map(corpusSnippet,tolower)
corpusAbstract <- tm_map(corpusAbstract,tolower)
corpusHeadLine <- tm_map(corpusHeadLine,PlainTextDocument)
corpusSnippet <- tm_map(corpusSnippet,PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract,PlainTextDocument)
corpusHeadLine <- tm_map(corpusHeadLine,removePunctuation)
corpusSnippet <- tm_map(corpusSnippet,removePunctuation)
corpusAbstract <- tm_map(corpusAbstract,removePunctuation)
corpusHeadLine <- tm_map(corpusHeadLine,removeWords,stopwords("english"))
corpusSnippet <- tm_map(corpusSnippet,removeWords,stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusHeadLine <- tm_map(corpusHeadLine,stemDocument)
corpusSnippet <- tm_map(corpusSnippet,stemDocument)
corpusAbstract <- tm_map(corpusAbstract,stemDocument)

#Using bag of words
# dtmHeadLine <- DocumentTermMatrix(corpusHeadLine)
# dtmSnippet <- DocumentTermMatrix(corpusSnippet)
# dtmAbstract <- DocumentTermMatrix(corpusAbstract)

#Using N-grams
require(RWeka)
library(rJava)
# NGramTokenizer splits strings into n-grams with given minimal and maximal numbers of grams
# 单个词有时候并不能完全表达出作者的意思，所以记号化（Tokenization）是将一段文本分割成叫做
# token（象征）过程，token可能是单词、短语、符号或其他有意义的元素
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))  
dtmHeadLine <- DocumentTermMatrix(corpusHeadLine, control = list(tokenize = TrigramTokenizer))
dtmSnippet <- DocumentTermMatrix(corpusSnippet, control = list(tokenize = TrigramTokenizer))
dtmAbstract <- DocumentTermMatrix(corpusAbstract, control = list(tokenize = TrigramTokenizer))

# Remove sparse terms
sparseHeadLine <- removeSparseTerms(dtmHeadLine,0.99)
sparseSnippet <- removeSparseTerms(dtmSnippet,0.97)
sparseAbstract <- removeSparseTerms(dtmAbstract,0.97)

# Convert to a data frame
dtHeadLine <- as.data.frame(as.matrix(sparseHeadLine))
dtSnippet <- as.data.frame(as.matrix(sparseSnippet))
dtAbstract <- as.data.frame(as.matrix(sparseAbstract))

#Adding a specific letter in front of all the variable names to differ
colnames(dtHeadLine) <- paste0("H",colnames(dtHeadLine))
colnames(dtSnippet) <- paste0("S",colnames(dtSnippet))
colnames(dtAbstract) <- paste0("A",colnames(dtAbstract))

# Make all variable names R-friendly
colnames(dtHeadLine) = make.names(colnames(dtHeadLine))
colnames(dtSnippet) = make.names(colnames(dtSnippet))
colnames(dtAbstract) = make.names(colnames(dtAbstract))

#Split the observations back into the training set and testing set.
train.headline <- head(dtHeadLine,nrow(train))
train.snippet <- head(dtSnippet,nrow(train))
train.abstract <- head(dtAbstract,nrow(train))

test.headline <- tail(dtHeadLine,nrow(test))
test.snippet <- tail(dtSnippet,nrow(test))
test.abstract <- tail(dtAbstract,nrow(test))

#Combining training and testing datasets in one
train.dtm <- cbind(train.headline,train.snippet,train.abstract)
test.dtm <- cbind(test.headline,test.snippet,test.abstract)

#Adding the dependent variable to training set
train.dtm$Popular <- train$Popular

#Adding more independents variables for both testing and training set
train.dtm$WordCount <- train$WordCount
test.dtm$WordCount <- test$WordCount

train.dtm$Month <- train$PubDate$mon
test.dtm$Month <- test$PubDate$mon

#train.dtm$Hour <- train$PubDate$hour
#test.dtm$Hour <- test$PubDate$hour

# train.dtm$Weekday <- train$PubDate$wday
# test.dtm$Weekday <- test$PubDate$wday

train.dtm$Weekday <- factor(weekdays(train$PubDate))
test.dtm$Weekday <- factor(weekdays(test$PubDate), levels=levels(train.dtm$Weekday))

# train.dtm$Mday <- train$PubDate$mday
# test.dtm$Mday <- test$PubDate$mday 

train.dtm$NewsDesk <- as.factor(train$NewsDesk)
test.dtm$NewsDesk <- factor(test$NewsDesk,levels=levels(train.dtm$NewsDesk))

train.dtm$SectionName <- as.factor(train$SectionName)
test.dtm$SectionName <- factor(test$SectionName,levels(train.dtm$SectionName))

train.dtm$SubsectionName <- as.factor(train$SubsectionName)
test.dtm$SubsectionName <- factor(test$SubsectionName,levels(train.dtm$SubsectionName))

#-------------------------------Create this function for comparing accuracy and AUC localy---------------------------------
if (!exists("train.accuracy")) { train.accuracy <- c()}
if (!exists("train.AUC")) { train.AUC <- c()}
accuracy <- function(table) {
  if (is.table(table))
  {
    v <- as.vector(table)
    return((v[1]+v[4])/sum(v))
  }
  else
    warning('This function needs a table as parameter')
}

#----------------------------------Random Forest------------------
require(randomForest)
#ROC Curve
require(ROCR)
train.dtm$Popular <- as.factor(train.dtm$Popular)
set.seed(2000)
#train.rf <- randomForest(Popular~.,data=train.dtm,importance = TRUE,proximity = TRUE)
train.rf <- randomForest(Popular~.,data=train.dtm)
train.pred.rf <- predict(train.rf,type="prob")[,2]
train.accuracy[length(train.accuracy)+1] <- accuracy(table(train.dtm$Popular,train.pred.rf>=0.5))

train.prediction.rf <- prediction(train.pred.rf,train.dtm$Popular)
train.AUC[length(train.AUC)+1] <-as.numeric(performance(train.prediction.rf, "auc")@y.values)

train.accuracy
train.AUC

test.pred.rf <- predict(train.rf,newdata=test.dtm,type="prob")[,2]

#Plotting
train.prediction.rf <- prediction(train.pred.rf,train.dtm$Popular)
plot(performance(train.prediction.rf,"tpr","fpr"), colorize=TRUE)

#--------------------------Preparing Submission-----------------------------------

MySubmission2 = data.frame(UniqueID = test$UniqueID, Probability1 = test.pred.rf)
write.csv(MySubmission2, "SubmissionRandomForest11x.csv", row.names=FALSE)

#-----------------------------------------------------------------------------------

#My best score was using all corpus with the following params
# Remove Sparse terms in Headlines 0.99
# Remove Sparse terms in Abstract 0.99
# Remove Sparse terms in Snippet 0.99
# Add variables like WordCount, Month, Weekday, NewsDesk,SectionName,SubsectionName
# Using random forest with the following params
#importance = TRUE,proximity = TRUE and seed(2000)
#Results: 
#accuracyintrain: 0.9110533
#aucintrain: 0.9300078
#aucintest: 0.92206 (60% data)

#Second best Params using al corpus with the following params
# Remove Sparse terms in Headlines 0.99
# Remove Sparse terms in Abstract 0.98
# Remove Sparse terms in Snippet 0.98
# Add variables like WordCount, Month, Weekday, NewsDesk,SectionName,SubsectionName
# Using random forest with the following params
#importance = TRUE,proximity = TRUE and seed(2000)
#Results: 
#accuracyintrain: 0.9104409
#aucintrain: 0.9348722
#aucintest: 0.92335 (60% data)
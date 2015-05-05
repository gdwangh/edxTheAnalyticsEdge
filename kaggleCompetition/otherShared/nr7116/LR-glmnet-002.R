# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
setwd( "C:\\Neeraj\\backup\\tech\\coursera\\AnalyticEdge\\kaggle")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

## default Train subsection name from sectionName |NewsDesk
table(NewsTrain$SubsectionName)
for (i in 1:nrow(NewsTrain)) {  
  NewsTrain$SubsectionName[i] <- ifelse(nchar(NewsTrain$SubsectionName[i])==0, NewsTrain$SectionName[i], NewsTrain$SubsectionName[i])
}
for (i in 1:nrow(NewsTrain)) {  
  NewsTrain$SubsectionName[i] <- ifelse(nchar(NewsTrain$SubsectionName[i])==0, NewsTrain$NewsDesk[i], NewsTrain$SubsectionName[i])
}
table(NewsTrain$SubsectionName)

## default Test subsection name from sectionName |NewsDesk
table(NewsTest$SubsectionName)
for (i in 1:nrow(NewsTest)) {  
  NewsTest$SubsectionName[i] <- ifelse(nchar(NewsTest$SubsectionName[i])==0, NewsTest$SectionName[i], NewsTest$SubsectionName[i])
}
for (i in 1:nrow(NewsTest)) {  
  NewsTest$SubsectionName[i] <- ifelse(nchar(NewsTest$SubsectionName[i])==0, NewsTest$NewsDesk[i], NewsTest$SubsectionName[i])
}
table(NewsTest$SubsectionName)

library(tm) # Load tm package
corpus = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))  # Create corpus
corpus = tm_map(corpus, tolower) # Pre-process data
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm1 = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm1, 0.99)  # create sparse terms
HeadlineWords = as.data.frame(as.matrix(dtm)) # Create data frame
colnames(HeadlineWords) = make.names(colnames(HeadlineWords)) # Let's make sure our variable names are okay for R:
colnames(HeadlineWords) <- paste0("H", colnames(HeadlineWords))

#CombineWords <- cbind(HeadlineWords)

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

HeadlineWordsTrain$WordCount = log(NewsTrain$WordCount+1)
HeadlineWordsTrain$SubsectionName <- as.factor(NewsTrain$SubsectionName)
HeadlineWordsTrain$comment <- as.factor(grepl("Open for Comments", NewsTrain$Headline, ignore.case=T))
HeadlineWordsTrain$nocomment <- as.factor(grepl("No Comment Necessary", NewsTrain$Headline, ignore.case=T))
HeadlineWordsTrain$york <- as.factor(grepl("new york", NewsTrain$Headline, ignore.case=T))

HeadlineWordsTest$WordCount <- log(NewsTest$WordCount+1)
HeadlineWordsTest$SubsectionName <- as.factor(NewsTest$SubsectionName)
HeadlineWordsTest$comment <- as.factor(grepl("Open for Comments", NewsTest$Headline, ignore.case=T))
HeadlineWordsTest$nocomment <- as.factor(grepl("No Comment Necessary", NewsTest$Headline, ignore.case=T))
HeadlineWordsTest$york <- as.factor(grepl("new york", NewsTest$Headline, ignore.case=T))

# One more helpful hint:
# This dataset has a date/time field (PubDate). You might remember dealing with date and time data in some of the Unit 1 homework problems. 
# In this dataset, the following commands might be useful to you when trying to get date and time variables.

# To convert the date/time to something R will understand, you can use the following commands:
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

# The second argument tells the strptime function how the data is formatted. 
# If you opened the file in Excel or another spreadsheet software before loading it into R, you might have to adjust the format. 
# See the help page ?strptime for more information.

# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:
HeadlineWordsTrain$Weekday = as.factor(NewsTrain$PubDate$wday)
HeadlineWordsTest$Weekday = as.factor(NewsTest$PubDate$wday)
HeadlineWordsTrain$Hour = as.factor(NewsTrain$PubDate$hour)
HeadlineWordsTest$Hour = as.factor(NewsTest$PubDate$hour)

# Weekday could now be used as an independent variable in your predictive models.
# For more fields that you can extract from a date/time object in R, see the help page ?POSIXlt

HeadlineWordsTest$SubsectionName <- factor(HeadlineWordsTest$SubsectionName,levels=levels(HeadlineWordsTrain$SubsectionName))

#################
## training/testing
library(glmnet)
library(caTools)
library(caret)
library(ROCR)

set.seed(144)

## train part of HeadlineWordsTrain
lambda.seq <- exp(seq(log(1e-5), log(1e0), length.out = 20))
eGrid = expand.grid(alpha = 1,lambda = lambda.seq)

xdummy = dummyVars("~.",data=HeadlineWordsTrain, fullRank=T)
xtrain <- as.data.frame(predict(xdummy,HeadlineWordsTrain))
ytrain = as.factor(ifelse( NewsTrain$Popular==1, 'yes', 'no'))
xtest <- as.data.frame(predict(xdummy,HeadlineWordsTest))

nf  <- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)
tr = train(x=xtrain, y=ytrain, method="glmnet", family = "binomial", metric="ROC", trControl=nf, tuneGrid=eGrid)
#alpha = 1 and lambda = 0.002335721
alpha <- tr$bestTune$alpha
lambda <- tr$bestTune$lambda
pred <- predict(tr, newdata=xtrain, type='prob')[,2]
PredTest <- predict(tr, newdata=xtest, type='prob')[,2]
###test.pred =predict (tr$finalModel ,s=tr$bestTuen$lambda ,newx=xtrain, type="response")

PredTestROC = prediction(pred, ytrain)
performance(PredTestROC, "auc")@y.values

# Now we can prepare our submission file for Kaggle:
logfit = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(logfit, "logit-glmnet-lasso.csv", row.names=FALSE)


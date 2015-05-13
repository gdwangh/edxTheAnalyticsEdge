# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.

summary(HeadlineWordsTest)

# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
str(NewsTrain)
summary(NewsTrain)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

table(NewsTrain$RR, NewsTrain$Popular)

hist(log(40+NewsTest$WordCount))
NewsTrain$logWC = log(40+NewsTrain$WordCount)
NewsTest$logWC = log(40+NewsTest$WordCount)

HeadlineWordsTest$logWC = NewsTest$logWC
HeadlineWordsTrain$logWC = NewsTrain$logWC

summary(HeadlineWordsTrain)
NewsTrain$AbstractWordCount <- HeadlineWordsTrain$AbstractWordCount
NewsTrain$HeadlineWordCount <- HeadlineWordsTrain$HeadlineWordCount

NewsTrain$AbstractWC <- HeadlineWordsTrain2$AbstractWC
NewsTrain$HeadlineWC <- HeadlineWordsTrain2$HeadlineWC


HeadlineWordsTrain2$HeadlineWC

summary(HeadlineWordsTest)

HeadlineWordsTest$HeadlineWC

HeadlineWordsTrain$HeadlineWC


NewsTest$AbstractWC <- HeadlineWordsTest$AbstractWC
NewsTest$HeadlineWC <- HeadlineWordsTest$HeadlineWC



# We will just create a simple logistic regression model, to predict Popular using WordCount:
SimpleMod = glm(Popular ~ music + abmusic + War + abwar + friday + fashion + abfashion + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + HeadlineWC + AbstractWC + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, data=NewsTrain, family=binomial)
summary(SimpleMod)
predictionLog <- predict(SimpleMod, type = "response")
table(predictionLog > 0.5, NewsTrain$Popular)

ROCRpredTrain = prediction(predictionLog, NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

predictionLogTest <- predict(SimpleMod, newdata= NewsTest, type = "response")

table(predictionLogTest >0.5)

MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionLogTest)
write.csv(MySubmission2, "Logultimateallvaryt.csv", row.names=FALSE)

MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionLogTest)
write.csv(MySubmission2, "Logultimateallvarmusicwar.csv", row.names=FALSE)


SimpleCART = rpart(Popular ~senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + HeadlineWC + AbstractWC + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, data=NewsTrain, minbucket=20, method="class")
prp(SimpleCART)
predictionCART <- predict(SimpleCART, type = "class")
table(predictionCART, NewsTrain$Popular)

# And then make predictions on the test set:
library(ROCR)

ROCRpredTrain = prediction(predictionCART, NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

predictionLogTest <- predict(SimpleMod, newdata= NewsTest, type = "response")

table(predictionLogTest> 0.5)

library(rpart)
library(rpart.plot)

TrainCART <- rpart(Popular ~ WordCount + NewsDesk + Weekday + SectionName, data=NewsTrain, method= "class")
prp(TrainCART)

predictionCART <- predict(TrainCART, type = "prob")
predictionCART 
table(predictionCART[,2] > 0.5, NewsTrain$Popular)

predictionCART2 <- predict(TrainCART, type = "class")
table(predictionCART2 , NewsTrain$Popular)
summary(predictionCART2)

ROCRpredTrainCART = prediction(predictionCART[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrainCART, "auc")@y.values)

library(randomForest)

SimpleRF = randomForest(as.factor(Popular) ~ music + abmusic + War + abwar + friday+ fashion + abfashion + China + Chinese + yt + SectionName + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + HeadlineWC + AbstractWC + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, data=NewsTrain, method="class" , ntree=1000, nodesize=10)
varImpPlot(SimpleRF)
predictionSimpleRF <- predict(SimpleRF, type = "prob")
table(predictionSimpleRF[,2] > 0.5, NewsTrain$Popular)

ROCRpredTrainSimpleRF = prediction(predictionSimpleRF[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrainSimpleRF, "auc")@y.values)

predictionSimpleRFTest <- predict(SimpleRF, newdata= NewsTest, type = "prob")

table(predictionSimpleRFTest[,2]> 0.5)


MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionSimpleRFTest[,2])

write.csv(MySubmission2, "SimpleRF2.csv", row.names=FALSE)

SimpleRF2 = randomForest(as.factor(Popular) ~ music + abmusic + War + abwar + friday+ fashion + abfashion + China + Chinese + yt + SectionName + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, data=NewsTrain, method="class" , ntree=1000, nodesize=10)
varImpPlot(SimpleRF2)

predictionSimpleRF2 <- predict(SimpleRF2, type = "prob")
table(predictionSimpleRF2[,2] > 0.5, NewsTrain$Popular)

ROCRpredTrainSimpleRF2 = prediction(predictionSimpleRF2[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrainSimpleRF2, "auc")@y.values)

predictionSimpleRF2Test <- predict(SimpleRF2, newdata= NewsTest, type = "prob")

table(predictionSimpleRF2Test[,2]> 0.5)

MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionSimpleRF2Test[,2])

write.csv(MySubmission2, "SimpleRFnoabstractheadlinewc.csv", row.names=FALSE)

SimpleRF3 = randomForest(as.factor(Popular) ~ SectionName + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, data=NewsTrain, method="class" , ntree=1000, nodesize=10)
varImpPlot(SimpleRF3)

predictionSimpleRF3 <- predict(SimpleRF3, type = "prob")
table(predictionSimpleRF3[,2] > 0.5, NewsTrain$Popular)

ROCRpredTrainSimpleRF3 = prediction(predictionSimpleRF3[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrainSimpleRF3, "auc")@y.values)

predictionSimpleRF3Test <- predict(SimpleRF3, newdata= NewsTest, type = "prob")

MySubmission3 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionSimpleRF3Test[,2])

write.csv(MySubmission2, "SimpleRFnoabstractheadlinewcshort.csv", row.names=FALSE)


summary(NewsTrain2)

NewsTrain2 <- NewsTrain
NewsTrain2 <- NewsTrain

for(i in 1:6532)
{if(NewsTrain2$Popular[i] == 1) NewsTrain2$Popular[i] = "Yes" else NewsTrain2$Popular[i] = "No"}


trgbm = train(as.factor(Popular) ~ music + abmusic + War + abwar + friday+ fashion + abfashion+ China + Chinese + yt + SectionName + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + HeadlineWC + AbstractWC + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, NewsTrain2, method="gbm", distribution= "bernoulli", metric="ROC", trControl=fitControl, verbose= FALSE)
predictiongbm <- predict(trgbm, NewsTrain2, type = "prob")



table(predictiongbm[,2] > 0.5, NewsTrain$Popular)

ROCRpredTraingbm = prediction(predictiongbm[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTraingbm, "auc")@y.values)

trgbm2 = train(as.factor(Popular) ~ music + abmusic + War + abwar + friday+ fashion + abfashion+ China + Chinese + yt + SectionName + eu + abRR + comments + RR + Times + pres + abreader+  abreaders + abpuzzle + The + Facebook + qna + obama + nc + apple + recap + fourteen + excl + ten + abferg + Facebook + senator + NYAbs+ test + Ask + morning + we + senator + Facebook + uber +  abuber + abebola + abobama  + abdollar + ebola + ferg + wordof + report + today + daily + qna + quandary + facts + excl +eighteen + nineteen + OFC + six + Verbatim + picture + Abstractqmark+  NY + Headlineqmark + logWC + NewsDesk + Weekday + SubsectionName + Hour, NewsTrain2, method="gbm", distribution= "bernoulli", metric="ROC", trControl=fitControl, verbose= FALSE)
predictiongbm2 <- predict(trgbm2, NewsTrain2, type = "prob")


predictiongbm <- predict(trgbm, NewsTrain2, type = "prob")

predictiongbmTest <- predict(trgbm, NewsTest, type = "prob")

predictiongbmTest2 <- predict(trgbm2, NewsTest, type = "prob")

predictiongbmTest

table(predictiongbmTest[,2]> 0.5)

MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictiongbmTest[,2])
write.csv(MySubmission2, "Simplegbm2.csv", row.names=FALSE)

MySubmission3 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictiongbmTest2[,2])
write.csv(MySubmission3, "Simplegbmnowc.csv", row.names=FALSE)


NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)

NewsTest$SectionName = factor(NewsTest$SectionName, levels= levels(NewsTrain$SectionName))
NewsTest$NewsDesk = factor(NewsTest$NewsDesk, levels= levels(NewsTrain$NewsDesk))
NewsTest$SubsectionName = factor(NewsTest$SubsectionName, levels= levels(NewsTrain$SubsectionName))


summary(NewsTrain)
summary(NewsTest)

table(NewsTrain$SectionName)

TrainForest <- randomForest(as.factor(Popular) ~ WordCount + NewsDesk + Weekday + SectionName, data=NewsTrain, method= "class")
predictionForest <- predict(TrainForest, type = "prob")
predictionForest
table(predictionForest[,2] > 0.5, NewsTrain$Popular)
ROCRpredTrainForest = prediction(predictionForest[,2], NewsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrainForest, "auc")@y.values)


predictionForest2 <- predict(TrainForest, newdata= NewsTest, type = "prob")
predictionForest2[,2]
MySubmission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predictionForest2[,2])
write.csv(MySubmission2, "RandomForestfirst.csv", row.names=FALSE)

# We can't compute the accuracy or AUC on the test set ourselves, since we don't have the dependent variable on the test set (you can compute it on the training set though!). 
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

predTest <- predict(SimpleMod, newdata= NewsTest, type = "response")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

# One more helpful hint:
# This dataset has a date/time field (PubDate). You might remember dealing with date and time data in some of the Unit 1 homework problems. 
# In this dataset, the following commands might be useful to you when trying to get date and time variables.

# To convert the date/time to something R will understand, you can use the following commands:

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

summary(NewsTrain)
summary(NewsTrain$PubDate)

# The second argument tells the strptime function how the data is formatted. 
# If you opened the file in Excel or another spreadsheet software before loading it into R, you might have to adjust the format. 
# See the help page ?strptime for more information.

# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTest$Hour = NewsTest$PubDate$hour

NewsTrain$Minute = NewsTrain$PubDate$min
NewsTest$Minute = NewsTest$PubDate$min

NewsTrain$Second = NewsTrain$PubDate$sec
NewsTest$Second = NewsTest$PubDate$sec

summary(NewsTest)

table(NewsTrain$Second)

table(NewsTrain$Weekday)

# Weekday could now be used as an independent variable in your predictive models.

# For more fields that you can extract from a date/time object in R, see the help page ?POSIXlt

library(tm)
library(SnowballC)

# Then create a corpus from the headline variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.995)

HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# Note that this split of HeadlineWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

HeadlineWordsTrain$Popular = NewsTrain$Popular

HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount

HeadlineWordsTrain$Weekday = NewsTrain$Weekday
HeadlineWordsTest$Weekday = NewsTest$Weekday

HeadlineWordsTrain$Hour= NewsTrain$Hour
HeadlineWordsTest$Hour = NewsTest$Hour

HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk
HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk

HeadlineWordsTrain$SectionName = NewsTrain$SectionName
HeadlineWordsTest$SectionName = NewsTest$SectionName

HeadlineWordsTrain$SubsectionName = NewsTrain$SectionName
HeadlineWordsTest$SubsectionName = NewsTest$SectionName

HeadlineWords$Popular = NewsTrain$Popular

sort(colSums(subset(HeadlineWordsTrain, Popular == 1)))

sort(colSums(HeadlineWordsTest))

HeadlineWordsTrain$HeadlineWC = rowSums(HeadlineWordsTrain)
HeadlineWordsTest$HeadlineWC = rowSums(HeadlineWordsTest)


sort(colSums(HeadlineWordsTrain))

summary(HeadlineWordsTest)
str(HeadlineWordsTrain)
summary(HeadlineWordsTrain)

HeadlineWordsTrain2 = HeadlineWordsTrain

HeadlineWordsTrain2$Popular = NewsTrain$Popular

summary(HeadlineWordsTrain2)

table(HeadlineWordsTrain$Popular)

table(NewsTrain$Popular)

HeadlineWordsLog <- glm(Popular ~ AbstractWordCount + HeadlineWordCount + appl + ebola + obama + day+ fashion + morn + new + read + today + word + logWC + NewsDesk + Weekday + SectionName + SubsectionName + Hour , data= HeadlineWordsTrain2, family= binomial)
summary(HeadlineWordsLog )

HeadlineWordsLog2 <- glm(Popular ~ . , data= HeadlineWordsTrain, family= binomial)
summary(HeadlineWordsLog2)

predHeadlineWordsLog <- predict(HeadlineWordsLog , type = "response")
table(predHeadlineWordsLog > 0.5, HeadlineWordsTrain$Popular)

ROCRpredTrain = prediction(predHeadlineWordsLog2, HeadlineWordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

HeadlineWordsForest <- randomForest(as.factor(Popular) ~  . , data= HeadlineWordsTrain, method= "class")
predHeadlineWordsForest <- predict(HeadlineWordsForest, type="prob")

table(predHeadlineWordsForest[,2] > 0.5, HeadlineWordsTrain$Popular)

ROCRpredTrain = prediction(predHeadlineWordsForest[,2], HeadlineWordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

HeadlineWordsForest2 <- randomForest(as.factor(Popular) ~ music + recap + war + day+ fashion + morn + new + read + today + word + WordCount + NewsDesk + Weekday + SectionName + SubsectionName, data= HeadlineWordsTrain, method= "class")
predHeadlineWordsForest2 <- predict(HeadlineWordsForest2, type="prob")

table(predHeadlineWordsForest2[,2] > 0.5, HeadlineWordsTrain$Popular)

ROCRpredTrain = prediction(predHeadlineWordsForest2[,2], HeadlineWordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

predHeadlineWordsForest3 <- predict(HeadlineWordsForest2, newdata= HeadlineWordsTest, type="prob")
predHeadlineWordsForest3[,2]

MySubmission3 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predHeadlineWordsForest3[,2])
write.csv(MySubmission3, "RandomForestheadline.csv", row.names=FALSE)

NewsTrain$Abstract[2000]
NewsTrain$Snippet[2000]

###

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusAbstract = tm_map(CorpusAbstract, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)

CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)

CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))

CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusAbstract)

sparse = removeSparseTerms(dtm, 0.995)

AbstractWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(AbstractWords) = make.names(colnames(AbstractWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of AbstractWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from AbstractWords, and putting them in a new data frame called "AbstractWordsTrain"

AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of AbstractWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from AbstractWords, and putting them in a new data frame called "AbstractWordsTest"

AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))

summary(AbstractWordsTrain)

colnames(AbstractWordsTrain) <- paste("Ab", colnames(AbstractWordsTrain), sep = "_")
colnames(AbstractWordsTest) <- paste("Ab", colnames(AbstractWordsTest), sep = "_")



WordsTrain <- cbind(HeadlineWordsTrain , AbstractWordsTrain)
summary(WordsTrain)

WordsTest <- cbind(HeadlineWordsTest , AbstractWordsTest)
summary(WordsTest)


sort(colSums(subset(WordsTrain, Popular == 1)))

sort(colSums(subset(HeadlineWordsTrain, Popular == 1)))

sort(colSums(AbstractWordsTest))

sort(colSums(AbstractWordsTrain))

summary(HeadlineWordsTrain)
summary(HeadlineWordsTest)

HeadlineWordsTrain$AbstractWC = rowSums(AbstractWordsTrain)

HeadlineWordsTest$AbstractWC = rowSums(AbstractWordsTest)

hist(HeadlineWordsTrain$HeadlineWC)

hist(HeadlineWordsTest$AbstractWC )

WordsLog <- glm(Popular ~ Hour + AbstractWC + day+ fashion + morn + new + read + today + word + logWC + NewsDesk + Weekday +  Ab_play + Ab_american + Ab_latest + Ab_live + Ab_music + Ab_recent + Ab_say + Ab_school + Ab_system + Ab_women +Ab_write+ Ab_reader + Ab_play, data= WordsTrain, family= binomial)
summary(WordsLog )

WordsTrain2 <- WordsTrain

WordsTrain2$Popular <- NewsTrain$Popular

WordsLog2 <- glm(Popular ~ . , data= WordsTrain2, family= binomial)
summary(WordsLog2 )

predWordsLog = predict(WordsLog, data= WordsTrain, type = "response" )
table(WordsTrain$Popular, predWordsLog > 0.5)
predWordsLog 


ROCRpredTrain = prediction(predWordsLog, WordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

predWordsLog2 = predict(WordsLog2, data= WordsTrain, type = "response" )
table(WordsTrain$Popular, predWordsLog2 > 0.5)

ROCRpredTrain2 = prediction(predWordsLog2, WordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain2, "auc")@y.values)


predWordsLog3 = predict(WordsLog, newdata= WordsTest, type = "response" )
predWordsLog3

MySubmission4 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predWordsLog3)
write.csv(MySubmission4, "LogRegressioncorpus.csv", row.names=FALSE)


WordsForest <- randomForest(as.factor(Popular) ~ music+ recap + war + day+ fashion + morn + new + read + today + word + WordCount + NewsDesk + Weekday + SubsectionName+ SectionName + Sub_play + Sub_american + Sub_latest + Sub_live + Sub_music + Sub_recent + Sub_say + Sub_school + Sub_system + Sub_women + Sub_write+ Sub_reader + Sub_play, data= WordsTrain, method= "class", ntree=1000, nodesize=15)
summary(WordsForest )

predWordsForest = predict(WordsForest, data= WordsTrain, type = "prob" )
table(WordsTrain$Popular, predWordsForest[,2] > 0.5)

ROCRpredTrain = prediction(predWordsForest[,2], WordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

WordsForest2 <- randomForest(as.factor(Popular) ~ . , data= WordsTrain, method= "class", ntree= 1000, nodesize= 15)
summary(WordsForest2 )

predWordsForest2 = predict(WordsForest2, data= WordsTrain, type = "prob" )
table(WordsTrain$Popular, predWordsForest2[,2] > 0.5)

ROCRpredTrain2 = prediction(predWordsForest2[,2], WordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain2, "auc")@y.values)

predWordsForest3 = predict(WordsForest, newdata= WordsTest, type = "prob" )

MySubmission5 = data.frame(UniqueID = NewsTest$UniqueID, Probability1= predWordsForest3[,2])
write.csv(MySubmission5, "RandomForestcorpus.csv", row.names=FALSE)


WordsCART1 = rpart(Popular ~ music+ recap + war + day+ fashion + morn + new + read + today + word + WordCount + NewsDesk + Weekday + SectionName + Sub_play + Sub_american + Sub_latest + Sub_live + Sub_music + Sub_recent + Sub_say + Sub_school + Sub_system + Sub_women + Sub_write+ Sub_reader + Sub_play, data= WordsTrain, method= "class")
predWordsCART1 = predict(WordsCART1, data=WordsTrain, type = "prob" )
prp(WordsCART1)

table(WordsTrain$Popular, predWordsCART1[,2] > 0.5)

ROCRpredTrain = prediction(predWordsCART1[,2], WordsTrain$Popular)

auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)

str(NewsTrain$Headline)


NewsTrain$qna = grepl("Q. and A." , NewsTrain$Headline )

NewsTest$qna = grepl("Q. and A." , NewsTest$Headline )

NewsTrain$Headlineqmark = grepl("\\?" , NewsTrain$Headline )
NewsTest$Headlineqmark = grepl("\\?" , NewsTest$Headline )

table(NewsTrain$Headlineqmark , NewsTrain$Popular)
table(NewsTrain$abdollar , NewsTrain$Popular)
table(NewsTrain$headlinequote , NewsTrain$Popular)


NewsTrain$Abstractqmark = grepl("\\?" , NewsTrain$Abstract )
NewsTest$Abstractqmark = grepl("\\?" , NewsTest$Abstract )

NewsTrain$weirdmark= grepl("\\|" , NewsTrain$Headline )

NewsTrain$comma= grepl("\\," , NewsTrain$Headline )

NewsTrain$dollar= grepl("\\$" , NewsTrain$Headline )
NewsTest$dollar= grepl("\\$" , NewsTest$Headline )

NewsTrain$abdollar= grepl("\\$" , NewsTrain$Abstract )
NewsTest$abdollar= grepl("\\$" , NewsTest$Abstract )

HeadlineWordsTrain$abdollar <- NewsTrain$abdollar
HeadlineWordsTest$abdollar <- NewsTest$abdollar
NewsTrain$headlinequote = grepl("\\'" , NewsTrain$Headline )

NewsTrain$period = grepl("\\." , NewsTrain$Headline )

NewsTrain$colon = grepl("\\:" , NewsTrain$Headline )
NewsTest$colon = grepl("\\:" , NewsTest$Headline )

NewsTrain$Abcolon = grepl("\\:" , NewsTrain$Abstract )
NewsTest$Abcolon = grepl("\\:" , NewsTest$Abstract )

WordsTrain$nineteen <- NewsTrain$nineteen
WordsTrain$eighteen <- NewsTrain$eighteen

NewsTrain$nineteen = grepl("19" , NewsTrain$Headline )
NewsTrain$eighteen = grepl("18" , NewsTrain$Headline )

summary(NewsTrain)


NewsTest$nineteen = grepl("19" , NewsTest$Headline )
NewsTest$eighteen = grepl("18" , NewsTest$Headline )


summary(NewsTest)

WordsTest$nineteen <- NewsTest$nineteen
WordsTest$eighteen <- NewsTest$eighteen

WordsTest$eighteen


NewsTrain$fourteen = grepl("2014" , NewsTrain$Headline )
NewsTest$fourteen = grepl("2014" , NewsTest$Headline )

grepl("\\$" , NewsTrain$Headline )

NewsTrain$OFC = grepl("Open for Comments" , NewsTrain$Headline )
NewsTest$OFC = grepl("Open for Comments" , NewsTest$Headline )


HeadlineWordsTrain$OFC = NewsTrain$OFC
HeadlineWordsTest$OFC = NewsTest$OFC

NewsTrain$Obama = grepl("Obama" , NewsTrain$Headline )

table(NewsTrain$Obama , NewsTrain$Popular)

NewsTrain$review = grepl("review" , NewsTrain$Headline )

table(NewsTrain$review , NewsTrain$Popular)


NewsTrain$movie = grepl("review" , NewsTrain$Headline )

table(NewsTrain$movie , NewsTrain$Popular)


NewsTrain$NY = grepl("\\New York" , NewsTrain$Headline )
NewsTest$NY = grepl("\\New York" , NewsTest$Headline )

HeadlineWordsTrain$NY = NewsTrain$NY 
HeadlineWordsTest$NY = NewsTest$NY 

NewsTrain$comments = grepl("\\Comments" , NewsTrain$Headline )

NewsTrain$comments = grepl("comments" , NewsTrain$Abstract )
NewsTest$comments = grepl("comments" , NewsTest$Abstract )

NewsTrain$wordof = grepl("Word of" , NewsTrain$Headline )

NewsTrain$NYAbs = grepl("\\New York" , NewsTrain$Abstract )

NewsTrain$obama = grepl("\\Obama" , NewsTrain$Headline )
NewsTrain$abobama = grepl("\\Obama" , NewsTrain$Abstract)

NewsTrain$report = grepl("\\Report" , NewsTrain$Headline )

NewsTrain$today = grepl("Today in" , NewsTrain$Headline )

NewsTrain$ten = grepl("10" , NewsTrain$Headline )

NewsTrain$t = grepl("\\T's" , NewsTrain$Headline )

NewsTrain$A = grepl("A " , NewsTrain$Headline )

NewsTrain$we = grepl("What We" , NewsTrain$Headline )

NewsTest$comments = grepl("\\Comments" , NewsTest$Headline )

NewsTest$wordof = grepl("Word of" , NewsTest$Headline )

NewsTest$NYAbs = grepl("\\New York" , NewsTest$Abstract )

NewsTest$obama = grepl("\\Obama" , NewsTest$Headline )
NewsTest$abobama = grepl("\\Obama" , NewsTest$Abstract)

NewsTest$report = grepl("\\Report" , NewsTest$Headline )

NewsTest$today = grepl("Today in" , NewsTest$Headline )

NewsTest$ten = grepl("10" , NewsTest$Headline )

NewsTest$t = grepl("\\T's" , NewsTest$Headline )

NewsTest$A = grepl("A " , NewsTest$Headline )

NewsTest$we = grepl("What We" , NewsTest$Headline )

NewsTrain$Verbatim = grepl("Verbatim" , NewsTrain$Headline )
NewsTest$Verbatim = grepl("Verbatim" , NewsTest$Headline )

HeadlineWordsTrain$Verbatim = NewsTrain$Verbatim
HeadlineWordsTest$Verbatim = NewsTest$Verbatim 

table(NewsTrain$picture, NewsTrain$Popular)

NewsTrain$picture= grepl("\\Pictures of", NewsTrain$Headline)

NewsTest$picture= grepl("\\Pictures of", NewsTest$Headline)

HeadlineWordsTrain$picture= NewsTrain$picture

HeadlineWordsTest$picture= NewsTest$picture


NewsTrain$six= grepl("6 Q", NewsTrain$Headline)
NewsTest$six= grepl("6 Q", NewsTest$Headline)

HeadlineWordsTrain$six= NewsTrain$six
HeadlineWordsTest$six= NewsTest$six

HeadlineWordsTrain$daily <- NewsTrain$daily
HeadlineWordsTest$daily <- NewsTest$daily
NewsTrain$daily= grepl("Daily", NewsTrain$Headline)
NewsTest$daily= grepl("Daily", NewsTest$Headline)


NewsTrain$DC= grepl("Daily Clip", NewsTrain$Headline)
NewsTest$DC= grepl("Daily Clip", NewsTest$Headline)


NewsTrain$test= grepl("Test Yourself", NewsTrain$Headline)

NewsTest$test= grepl("Test Yourself", NewsTest$Headline)


NewsTrain$DR= grepl("Daily Report", NewsTrain$Headline)
NewsTest$DR= grepl("Daily Report", NewsTest$Headline)

NewsTrain$Ask = grepl("Ask Well", NewsTrain$Headline)
NewsTest$Ask = grepl("Ask Well", NewsTest$Headline)


NewsTrain$nc = grepl("No Comment", NewsTrain$Headline)
NewsTest$nc = grepl("No Comment", NewsTest$Headline)

NewsTrain$China = grepl("China", NewsTrain$Headline)
NewsTest$China = grepl("China", NewsTest$Headline)

NewsTrain$Chinese = grepl("Chinese", NewsTrain$Headline)
NewsTest$Chinese = grepl("Chinese", NewsTest$Headline)

WordsTrain$ferg <- NewsTrain$ferg
WordsTest$ferg <- NewsTest$ferg

NewsTrain$ferg = grepl("\\Ferguson", NewsTrain$Headline)
NewsTest$ferg = grepl("\\Ferguson", NewsTest$Headline)

NewsTrain$abferg = grepl("\\Ferguson", NewsTrain$Abstract)
NewsTest$abferg = grepl("\\Ferguson", NewsTest$Abstract)

WordsTrain$abferg <- NewsTrain$abferg
WordsTest$abferg <- NewsTest$abferg


NewsTrain$ebola = grepl("\\Ebola", NewsTrain$Headline)
NewsTest$ebola = grepl("\\Ebola", NewsTest$Headline)

HeadlineWordsTrain$abebola <- NewsTrain$abebola
HeadlineWordsTest$abebola <- NewsTest$abebola

NewsTrain$abebola = grepl("\\Ebola", NewsTrain$Abstract)
NewsTest$abebola = grepl("\\Ebola", NewsTest$Abstract)

NewsTrain$abuber = grepl("\\Uber", NewsTrain$Abstract)
NewsTest$abuber = grepl("\\Uber", NewsTest$Abstract)

NewsTrain$abpuzzle = grepl("\\puzzle", NewsTrain$Abstract)
NewsTest$abpuzzle = grepl("\\puzzle", NewsTest$Abstract)

NewsTrain$abreader = grepl("reader", NewsTrain$Abstract)
NewsTest$abreader = grepl("reader", NewsTest$Abstract)

NewsTrain$abreaders = grepl("readers", NewsTrain$Abstract)
NewsTest$abreaders = grepl("readers", NewsTest$Abstract)

NewsTrain$pres = grepl("president", NewsTrain$Abstract)
NewsTest$pres = grepl("president", NewsTest$Abstract)

NewsTrain$abTimes = grepl("Times", NewsTrain$Abstract)
NewsTest$abTimes = grepl("Times", NewsTest$Abstract)

NewsTrain$Times = grepl("Times", NewsTrain$Headline)
NewsTest$Times = grepl("Times", NewsTest$Headline)


summary(NewsTrain)

table(NewsTrain$abuber, NewsTrain$Popular)

HeadlineWordsTrain$abuber<- NewsTrain$abuber 
HeadlineWordsTest$abuber<- NewsTest$abuber

NewsTrain$uber = grepl("Uber", NewsTrain$Headline)
NewsTest$uber = grepl("Uber", NewsTest$Headline)

NewsTrain$The = grepl("The", NewsTrain$Headline)
NewsTest$The = grepl("The", NewsTest$Headline)

HeadlineWordsTrain$uber<- NewsTrain$uber 
HeadlineWordsTest$uber<- NewsTest$uber


NewsTrain$Facebook = grepl("Facebook", NewsTrain$Headline)
NewsTest$Facebook = grepl("Facebook", NewsTest$Headline)

NewsTrain$RR = grepl("Readers Respond", NewsTrain$Headline)
NewsTest$RR = grepl("Readers Respond", NewsTest$Headline)

NewsTrain$yt = grepl("Your Turn", NewsTrain$Headline)
NewsTest$yt = grepl("Your Turn", NewsTest$Headline)

NewsTrain$fashion = grepl("Fashion", NewsTrain$Headline)
NewsTest$fashion = grepl("Fashion", NewsTest$Headline)


NewsTrain$abfashion = grepl("fashion", NewsTrain$Abstract)
NewsTest$abfashion = grepl("fashion", NewsTest$Abstract)

NewsTrain$abRR = grepl("Readers", NewsTrain$Abstract)
NewsTest$abRR = grepl("Readers", NewsTest$Abstract)

NewsTrain$senator = grepl("Senator", NewsTrain$Abstract)
NewsTest$senator = grepl("Senator", NewsTest$Abstract)

NewsTrain$eu = grepl("Euro", NewsTrain$Headline)
NewsTest$eu = grepl("Euro", NewsTest$Headline)


NewsTrain$music = grepl("Music", NewsTrain$Headline)
NewsTest$music = grepl("Music", NewsTest$Headline)

NewsTrain$abmusic = grepl("music", NewsTrain$Abstract)
NewsTest$abmusic = grepl("music", NewsTest$Abstract)

NewsTrain$War = grepl("War", NewsTrain$Headline)
NewsTest$War = grepl("War", NewsTest$Headline)

NewsTrain$abwar = grepl("war", NewsTrain$Abstract)
NewsTest$abwar = grepl("war", NewsTest$Abstract)

summary(NewsTrain)
table(NewsTrain$ferg, NewsTrain$Popular)
table(NewsTrain$ebola, NewsTrain$Popular)


HeadlineWordsTrain$nc = NewsTrain$nc
HeadlineWordsTest$nc =NewsTest$nc 

NewsTrain$recap = grepl("\\Recap", NewsTrain$Headline)

NewsTrain$facts = grepl("Facts", NewsTrain$Headline)

NewsTrain$morning = grepl("Morning Agenda", NewsTrain$Headline)

NewsTrain$friday = grepl("Friday Night", NewsTrain$Headline)

NewsTrain$apple = grepl("\\Apple", NewsTrain$Headline)

NewsTrain$quandary = grepl("Weekly Quandary", NewsTrain$Headline)

NewsTrain$why = grepl("Why", NewsTrain$Headline)

NewsTrain$excl =  grepl("\\!", NewsTrain$Headline)

NewsTrain$posvar = NewsTrain$quandary | NewsTrain$facts | NewsTrain$nc | NewsTrain$Ask

NewsTest$recap = grepl("\\Recap", NewsTest$Headline)

NewsTest$facts = grepl("Facts", NewsTest$Headline)

NewsTest$morning = grepl("Morning Agenda", NewsTest$Headline)

NewsTest$friday = grepl("Friday Night", NewsTest$Headline)

NewsTest$apple = grepl("\\Apple", NewsTest$Headline)

NewsTest$quandary = grepl("Weekly Quandary", NewsTest$Headline)

NewsTest$why = grepl("Why", NewsTest$Headline)

NewsTest$excl =  grepl("\\!", NewsTest$Headline)

table(NewsTrain$recap, NewsTrain$Popular)

table(NewsTrain$excl, NewsTrain$Popular)

table(grepl("Open for Comments" , NewsTest$Headline ))

install.packages("startsWith")


=NewsTrain$recap 

=NewsTrain$facts 

=NewsTrain$morning 

=NewsTrain$friday 

=NewsTrain$apple 

NewsTrain$quandary 

NewsTrain$why  

NewsTrain$excl



NewsTest$recap

NewsTest$facts 

NewsTest$morning 

NewsTest$friday 

NewsTest$apple 

NewsTest$quandary 

NewsTest$why  
NewsTest$excl 

library(startsWith)
startsWith(NewsTrain$Headline, A, trim=FALSE, ignore.case=FALSE)

summary(NewsTrain)

summary(NewsTest)

pop <- subset(HeadlineWordsTrain, Popular == "Yes")
summary(pop)

hist(pop$AbstractWordCount)

hist(HeadlineWordsTrain$AbstractWC)

table(NewsTrain$NY, NewsTrain$Popular)

HeadlineWordsTrain$AbstractWordCount = sapply(gregexpr("\\W+", NewsTrain$Abstract), length) 

HeadlineWordsTrain$HeadlineWordCount = sapply(gregexpr("\\W+", NewsTrain$Headline), length) 

hist(log(1 + NewsTrain$HeadlineWC))

hist(HeadlineWordsTrain$HeadlineWordCount)

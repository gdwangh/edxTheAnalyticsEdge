# setting the working directory
setwd("D:/R/tutorial")

# Reading Datasets
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Changing into Date Posix
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

# Deriving Hour in Training Set
NewsTrain$hour = NewsTrain$PubDate$hour

# Deriving Hour in Test Set
NewsTest$hour = NewsTest$PubDate$hour

str(NewsTrain)

# Loading the Text Mining Library
library(tm)

#########################################
######### Headline ######################

# Creating Corpus from both Train and Test Dataset
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Creating DocumentTermMatrix
dtm = DocumentTermMatrix(CorpusHeadline)

# Remove Sparse words/terms
sparse = removeSparseTerms(dtm, 0.99)

# Converting into DataFrame
HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Prepend all the words with the letter A
colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))

#########################################
######### Snippet ######################

# Creating Corpus from both Train and Test Dataset
CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusSnippet = tm_map(CorpusSnippet, tolower)

# Remember this extra line is needed after running the tolower step:
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

# Creating DocumentTermMatrix
dtmSnippet = DocumentTermMatrix(CorpusSnippet)

# Remove Sparse words/terms
sparseSnippet = removeSparseTerms(dtmSnippet, 0.99)

# Converting into DataFrame
SnippetWords = as.data.frame(as.matrix(sparseSnippet))

# Let's make sure our variable names are okay for R:
colnames(SnippetWords) = make.names(colnames(SnippetWords))

# Prepend all the words with the letter A
colnames(SnippetWords) = paste0("S", colnames(SnippetWords))

#########################################
######### SectionName ######################

# Creating Corpus from both Train and Test Dataset
CorpusSectionName = Corpus(VectorSource(c(NewsTrain$SectionName, NewsTest$SectionName)))
CorpusSectionName = tm_map(CorpusSectionName, tolower)

# Remember this extra line is needed after running the tolower step:
CorpusSectionName = tm_map(CorpusSectionName, PlainTextDocument)
CorpusSectionName = tm_map(CorpusSectionName, removePunctuation)
CorpusSectionName = tm_map(CorpusSectionName, removeWords, stopwords("english"))
CorpusSectionName = tm_map(CorpusSectionName, stemDocument)

# Creating DocumentTermMatrix
dtmSectionName = DocumentTermMatrix(CorpusSectionName)

# Remove Sparse words/terms
sparseSectionName = removeSparseTerms(dtmSectionName, 0.99)

# Converting into DataFrame
sparseSectionNameDF = as.data.frame(as.matrix(sparseSectionName))

# Let's make sure our variable names are okay for R:
colnames(sparseSectionNameDF) = make.names(colnames(sparseSectionNameDF))

# Prepend all the words with the letter A
colnames(sparseSectionNameDF) = paste0("Z", colnames(sparseSectionNameDF))

#########################################
######### NewsDesk ######################

# Creating Corpus from both Train and Test Dataset
CorpusNewsDesk = Corpus(VectorSource(c(NewsTrain$NewsDesk, NewsTest$NewsDesk)))
CorpusNewsDesk = tm_map(CorpusNewsDesk, tolower)

# Remember this extra line is needed after running the tolower step:
CorpusNewsDesk = tm_map(CorpusNewsDesk, PlainTextDocument)
CorpusNewsDesk = tm_map(CorpusNewsDesk, removePunctuation)
CorpusNewsDesk = tm_map(CorpusNewsDesk, removeWords, stopwords("english"))
CorpusNewsDesk = tm_map(CorpusNewsDesk, stemDocument)

# Creating DocumentTermMatrix
dtmNewsDesk = DocumentTermMatrix(CorpusNewsDesk)

# Remove Sparse words/terms
sparseNewsDesk = removeSparseTerms(dtmNewsDesk, 0.99)

# Converting into DataFrame
sparseNewsDeskDF = as.data.frame(as.matrix(sparseNewsDesk))

# Let's make sure our variable names are okay for R:
colnames(sparseNewsDeskDF) = make.names(colnames(sparseNewsDeskDF))

# Prepend all the words with the letter A
colnames(sparseNewsDeskDF) = paste0("A", colnames(sparseNewsDeskDF))

################################################################
# Combine the two data frames into a data frame called NYWords
################################################################

NYWords = cbind(HeadlineWords,SnippetWords,sparseSectionNameDF,sparseNewsDeskDF)

# split the observations back into the training set and testing set.
NYWordsTrain = head(NYWords, nrow(NewsTrain))
NYWordsTest = tail(NYWords, nrow(NewsTest))

# Adding the dependent variables
NYWordsTrain$Popular = NewsTrain$Popular

# Adding WordCount variable
NYWordsTrain$WordCount = NewsTrain$WordCount
NYWordsTest$WordCount = NewsTest$WordCount

# Adding Hour variable
NYWordsTrain$Hour = NewsTrain$hour
NYWordsTest$Hour = NewsTest$hour

# See the data Structure
str(NYWordsTrain)

####################################################################
################### LOGISTIC REGRESSION ############################

# Now let's create a logistic regression model using all of the variables:
NYWordsLog = glm(Popular ~ ., data=NYWordsTrain, family=binomial)

# And make predictions on our test set:
PredTestNYWord = predict(NYWordsLog, newdata=NYWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTestNYWord)

#Writing to a file
write.csv(MySubmission, "HeadlineSnippetSecLog.csv", row.names=FALSE)

####################################################################
################### CLASSIFICATION TREE ############################
#  Found Less accurate

require(rpart)
require(rpart.plot)

NYWordsTrain$Popular = as.factor(NYWordsTrain$Popular)

# Selecting cp through Cross validation of CART model
require(caret)
require(e1071)

set.seed(2)
How many folds we want
numFolds = trainControl(method="cv",number=10)
cpGrid = expand.grid(.cp = seq(0.002,0.1,0.002))
train(Popular ~ ., data=NYWordsTrain,method="rpart",trControl = numFolds,tuneGrid=cpGrid)

NYWordsCART = rpart(Popular ~ ., data=NYWordsTrain, method="class",cp = 0.002)
prp(NYWordsCART)

# Accuracy on Test Set
PredictCARTcensus = predict(NYWordsCART,newdata = NYWordsTest,type="class")

# Predict the Accuracy (Confusion Matrix)
confusionMatrix = table(censusTest$over50k,PredictCARTcensus)
#(9243+1596)/nrow(censusTest)

# ROC Curve on Test Set
require(ROCR)
PredNYWordsCART = predict(NYWordsCART,newdata = NYWordsTrain)
head(PredNYWordsCART) ## Give 2 numbers for each set the percentage of Data in trainingSet with outcome 0 and 1

predNYWords = prediction(PredNYWordsCART[,2],NYWordsTrain$Popular)

perfNYWords = performance(predNYWords,"tpr","fpr")
plot(perfNYWords,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

# Calculate AUC
as.numeric(performance(predNYWords, "auc")@y.values) ##  0.8997982


####################################################################
################### RANDOM FOREST ############################
### Giving max accuracy

require(randomForest)

NYWordsTrain$Popular = as.factor(NYWordsTrain$Popular)

# Improving RandomForest
NYWordsRF = randomForest(Popular ~ ., data=NYWordsTrain,importance = TRUE,mtry = 10,corr.bias = TRUE,ntree = 500)

#str(NYWordsTrain)

#?randomForest

#varImpPlot(NYWordsRF)

#print(NYWordsRF)
#importance(NYWordsRF)

#summary(NYWordsRF)

# Prediction on entire Training Set
PredNYWordsRFTrain = predict(NYWordsRF,type="prob")
pred.prob.RF = PredNYWordsRFTrain[ ,2]

library(ROCR)
predROCR = prediction(pred.prob.RF, NYWordsTrain$Popular)

# Compute AUC
as.numeric(performance(predROCR, "auc")@y.values) ## 0.9221567 ## 0.9299331 ## 0.9316264

# Prediction on Test Set - NYWordsTest
PredNYWordsRFTest = predict(NYWordsRF,newdata = NYWordsTest,type="prob")

# Now we can prepare our submission file for Kaggle:
MySubmissionRF = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredNYWordsRFTest[ ,2])

# Submission Structure
head(MySubmissionRF)

#Writing to a file
write.csv(MySubmissionRF, "HeadlineSnippetSecNewsDeskRF4.csv", row.names=FALSE)

###########################################################################
################## Using Clustering on Logistic regression - POOR PERF ###########################
###########################################################################

# Always remove the dependant variables
# Training Dataset
limitedNYWordsTrain = NYWordsTrain
limitedNYWordsTrain$Popular = NULL

# Test Dataset
limitedNYWordsTest = NYWordsTest
limitedNYWordsTest$Popular = NULL

# Normalizing Training dataset and Testing dataset W.R.T. trainingSet
library(caret)
preproc = preProcess(limitedNYWordsTrain)
normTrain = predict(preproc, limitedNYWordsTrain)
normTest = predict(preproc, limitedNYWordsTest)

summary(normTrain) # Note mean is 0 and sd = 1

# kmean clustering
set.seed(1)
k = 5
km = kmeans(x = normTrain,centers = k)
table(km$cluster)

# obtain training set and testing set cluster assignments 
library(flexclust)

# Convert the results of kmeans to objects of class "kcca"
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

# Creating Training Subset
NYWordsTrain1 = subset(NYWordsTrain,clusterTrain ==1) 
#NYWordsTrain2 = subset(NYWordsTrain,clusterTrain ==2)
#NYWordsTrain3 = subset(NYWordsTrain,clusterTrain ==3) 
NYWordsTrain4 = subset(NYWordsTrain,clusterTrain ==4)
#NYWordsTrain5 = subset(NYWordsTrain,clusterTrain ==5)

# Merging cluster record NYWordsTrain2 + NYWordsTrain3 into NYWordsTrain5
NYWordsTrain235 = subset(NYWordsTrain,clusterTrain %in% c(2,3,5))

# Creating Testing Subset
NYWordsTest1 = subset(NYWordsTest,clusterTest ==1)
#NYWordsTest2 = subset(NYWordsTest,clusterTest ==2)
#NYWordsTest3 = subset(NYWordsTest,clusterTest ==3)
NYWordsTest4 = subset(NYWordsTest,clusterTest ==4)
#NYWordsTest5 = subset(NYWordsTest,clusterTest ==5)

# Merging cluster record NYWordsTest2 + NYWordsTest3 into NYWordsTest5
NYWordsTest235 = subset(NYWordsTest,clusterTest %in% c(2,3,5))

NYWordsTrain1$Popular = as.factor(NYWordsTrain1$Popular)
#NYWordsTrain2$Popular = as.factor(NYWordsTrain2$Popular)
#NYWordsTrain3$Popular = as.factor(NYWordsTrain3$Popular)
NYWordsTrain4$Popular = as.factor(NYWordsTrain4$Popular)
#NYWordsTrain5$Popular = as.factor(NYWordsTrain5$Popular)

NYWordsTrain235$Popular = as.factor(NYWordsTrain235$Popular)

table(NYWordsTrain1$Popular)
#table(NYWordsTrain2$Popular)
#table(NYWordsTrain3$Popular)
table(NYWordsTrain4$Popular)
table(NYWordsTrain235$Popular)

# Creating Model with 5 clusters
NYWordsModel1 = randomForest(Popular ~ ., data=NYWordsTrain1,importance = TRUE)
#NYWordsModel2 = randomForest(Popular ~ ., data=NYWordsTrain2,importance = TRUE)
#NYWordsModel3 = randomForest(Popular ~ ., data=NYWordsTrain3,importance = TRUE)
NYWordsModel4 = randomForest(Popular ~ ., data=NYWordsTrain4,importance = TRUE)
NYWordsModel235 = randomForest(Popular ~ ., data=NYWordsTrain235,importance = TRUE)

# Predicting on Test
predtestNYWordsModel1 = predict(NYWordsModel1,type="prob",newdata = NYWordsTest1)
#predtestNYWordsModel2 = predict(NYWordsModel2,type="prob",newdata = NYWordsTest2)
#predtestNYWordsModel3 = predict(NYWordsModel3,type="prob",newdata = NYWordsTest3)
predtestNYWordsModel4 = predict(NYWordsModel4,type="prob",newdata = NYWordsTest4)
predtestNYWordsModel235 = predict(NYWordsModel235,type="prob",newdata = NYWordsTest235)

head(predtestNYWordsModel1)

# Combining all predict and outcome
AllPredNYWordsModel = c(predtestNYWordsModel1[,1],predtestNYWordsModel4[,1],predtestNYWordsModel235[,1])
#AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
head(AllPredNYWordsModel)
# Now we can prepare our submission file for Kaggle:
MySubmissionRFCluster = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = AllPredNYWordsModel)

# Submission Structure
head(MySubmissionRFCluster)

#Writing to a file
write.csv(MySubmissionRFCluster, "HLSnippetSecNewsDeskRFCluster1.csv", row.names=FALSE)


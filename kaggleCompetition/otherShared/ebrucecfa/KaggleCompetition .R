# ========================================================================
# Test your analytics skills by predicting which New York Times blog articles will be the most popular
#
# Newspapers and online news aggregators like Google News need to understand which news articles will
# be the most popular, so that they can prioritize the order in which stories appear. In this competition, 
# you will predict the popularity of a set of New York Times blog articles from the time period 
# September 2014-December 2014.
#
# ========================================================================

# ========================================================================
# INSTALL PACKAGES AND LOAD LIBRARIES
# ========================================================================

library(caTools)
library(tm)
# install.packages("SnowballC")
library(SnowballC)
# install.packages("randomForest")
library(randomForest)
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
# install.packages("pROC")
library("pROC")
# install.packages("kernlab")
library(kernlab)
# install.packages("neuralnet")
library(neuralnet)
#
# ========================================================================

# ========================================================================
# LOAD DATA
# ========================================================================
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Load this file which has Data Cleansed NewsDesk, SectionName & Subsection Name. Also has redundant Snippets removed.
News = read.csv("NewsNewCats.csv", stringsAsFactors=FALSE)
News$PubDate[1:6532] = NewsTrain$PubDate # Requied to clean up date format
News$PubDate[6533:8402] = NewsTest$PubDate # Required to clean up date format

#
# ========================================================================

# ========================================================================
# DATA PRE-PROCESSING
# ========================================================================

# Make Headline and SectionName suitable factor names by removing white space and improper chars
News$NewsDesk = gsub(" ", "", News$NewsDesk, fixed = TRUE)
News$SectionName = gsub(" ", "", News$SectionName, fixed = TRUE)
News$SectionName = gsub("/", "", News$SectionName, fixed = TRUE)
#Remove SubsectionName - Not Needed and some blank values
News$SubsectionName = NULL 
# Use Log(WordCount + 1)
News$LogWordCount = log(News$WordCount+1)
# Remove Word Count - Not Needed
News$WordCount = NULL
# Change Popular from 1,0 to Yes,No for suitable factor names
News$Popular = as.character(ifelse(News$Popular == 1, "Yes", "No") )
#
# ========================================================================

# ========================================================================
# DATA FEATURE ENGINEERING
# ========================================================================

# Convert the Date variable to a format that R will recognize:
News$PubDate = strptime(News$PubDate, format="%m/%d/%y %H:%M")
# Extract the hour and the day of the week:
News$Weekday = weekdays(News$PubDate)
News$Hour = News$PubDate$hour
# Remove PubDate - Not Needed
News$PubDate = NULL
# Add boolean for '?' in Headline which is property of engaging content
News$Question = as.character(ifelse(grepl("\\?", News$Headline), "Yes", "No") )
# Add boolean for 'Comment' in headline which is property of engaging content
News$Comment = as.character(ifelse(grepl("comment", News$Headline, ignore.case=T), "Yes", "No") )
# Add boolean for 'Sex' in headline which is property of engaging content
News$Sex = as.character(ifelse(grepl("sex", News$Headline, ignore.case=T), "Yes", "No") )
# Add boolean for 'york' or NY in headline which is property of engaging NY Times content
News$NY = as.character(ifelse(grepl("york", News$Headline, ignore.case=T) | grepl("NY", News$Headline, ignore.case=F), "Yes", "No") )
#
# ========================================================================

# ========================================================================
# DATA CONVERT TO FACTORS
# ========================================================================

News$Popular = as.factor(News$Popular)
News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$Weekday = as.factor(News$Weekday)
News$Hour =as.factor(News$Hour)
News$Question = as.factor(News$Question)
News$Comment = as.factor(News$Comment)
News$Sex = as.factor(News$Sex)
News$NY = as.factor(News$NY) 
#
# ========================================================================

# ========================================================================
# BUILD HEADLINE CORPUS - Bag of Words
# ========================================================================
CorpusHeadline = Corpus(VectorSource(News$Headline))

# You can go through all of the standard pre-processing steps like we did in Unit 5:
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument, lazy=T)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.9975) # Headlines are important so we want high sparsity value
HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Since we will combine Headline and Abstract, add an 'zHL' in colnames
colnames(HeadlineWords) = paste0("zHL", colnames(HeadlineWords))

#
# ========================================================================

# ========================================================================
# BUILD ABSTRACT CORPUS - Bag of Words
# ========================================================================
CorpusAbstract = Corpus(VectorSource(News$Abstract))

# You can go through all of the standard pre-processing steps like we did in Unit 5:
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument, lazy=T)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!
rm(dtm)
rm(sparse)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.9975) # Abstract is not that important so lower sparsity
AbstractWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(AbstractWords) = make.names(colnames(AbstractWords))

# Since we will combine Abstract and Abstract, add an 'zAB' in colnames
colnames(AbstractWords) = paste0("zAB", colnames(AbstractWords))
#
# ========================================================================


# ========================================================================
# BUILD TRAIN AND TEST
# ========================================================================

# Combine Abstract and Headlines
dfAll = cbind(AbstractWords, HeadlineWords, row.names=NULL)
str(dfAll)

# Add important vars from News
dfAll$Popular = News$Popular 
dfAll$NewsDesk = News$NewsDesk
dfAll$SectionName = News$SectionName 
dfAll$Weekday = News$Weekday
dfAll$Hour = News$Hour 
dfAll$Question = News$Question
dfAll$Comment = News$Comment
dfAll$Sex = News$Sex
dfAll$NY = News$NY
dfAll$LogWordCount = News$LogWordCount
dfAll$IsTest = News$IsTest
dfAll$UniqueID = News$UniqueID

#Finally - make Test and Train
Train = dfAll[News$isTest==0,]
Test = dfAll[News$isTest==1,]
Train$isTest=NULL
Train$UniqueID=NULL
Test$isTest=NULL
Test$Popular = NULL

# Make Out of Sample Test and Train
set.seed(123)
split = sample.split(Train$Popular, SplitRatio = 0.7)
tr = subset(Train, split==TRUE)
te = subset(Train, split==FALSE)
#
# ========================================================================


# ========================================================================
# RUN GLM Model TO EXTRACT SIG VARS
# ========================================================================

# - 10 folds cross-validated Tuned glm Model with sig variables
# Perform the cross validation
fitControl = trainControl("cv", 10, classProbs = TRUE, summaryFunction = twoClassSummary)
GLM.Extract = train(Popular~., data=Train, method="glm", family="binomial", metric="ROC", trControl=fitControl)
summary(GLM.Extract)
#
# ========================================================================

# ========================================================================
# BUILD MODEL PARAMETER ARGS AS 'pargs1' & 'pargs2'
# ========================================================================

pargs3 = Popular~.

pargs1 = Popular~
  NewsDesk+
  LogWordCount+
  Hour+
  Weekday+
  Question+
  zHLwall+
  zHLuber+
  zHLtoday+
  zHLthanksgiving+
  zHLrecap+
  zHLanother+
  zABwrite+
  zABtalk+
  zABsaid+
  zABdiari+
  zHLwar+
  zHLrole+
  zHLrights+
  zHLnews+
  zHLkids+
  zHLhealth+
  zHLgood+
  zHLgets+
  zHLgame+
  zHLfacebook+
  zHLchange+
  zHLbusiness+
  zABshare+
  zABrecent+
  zABcan+
  zABarticl+
  zABamerican+
  HLvNegTerms+
  HLnegTerms+
  HLvPosTerms+
  HLposTerms+
  ABvNegTerms+
  ABnegTerms+
  ABvPosTerms+
  ABposTerms

# pargs 2 is slightly superior
pargs2 = Popular~
  SectionName+
  LogWordCount+
  Hour+
  Weekday+
  Question+
  zHLwall+
  zHLuber+
  zHLtoday+
  zHLthanksgiving+
  zHLrecap+
  zHLanother+
  zABwrite+
  zABtalk+
  zABsaid+
  zABdiari+
  zHLwar+
  zHLrole+
  zHLrights+
  zHLnews+
  zHLkids+
  zHLhealth+
  zHLgood+
  zHLgets+
  zHLgame+
  zHLfacebook+
  zHLchange+
  zHLbusiness+
  zABshare+
  zABrecent+
  zABcan+
  zABarticl+
  zABamerican
#
# ========================================================================


# ========================================================================
# RUN Stacked Model on SIG VARS - 
# Score  .89454 knn pargs2 #PreProc
# Score  .91670 glm2 pargs2 #PreProc
# Score  .92144 rf pargs2 #PreProc
# Score  .92559 gbm pargs2 #PreProc

# Score  .92761 xRF= 60 yGLM = 40 pargs2 #BEST non-PreProc ENTRY OVERALL
# Score  .92774 xRF= 60 yGLM = 40 Pre-Proc pargs2
# Score  .92745 xRF= 55 yGLM = 45 pargs2
# Score  .92784 xRF= 65 yGLM = 35 Pre-Proc pargs2
# Score  .92791 xRF= 625 yGLM = 375 Pre-Procpargs2
#
# Score  .93111 xRF = .42 yGLM = .25 zGBM = .33 Pre-Procpargs2 
# Score  .93169 xRF = .42 yGLM = .16 zGBM = .42 Pre-Procpargs2 
# Score  .93171 xRF = .44 yGLM = .12 zGBM = .44 Pre-Procpargs2 
# Score  .93171 xRF = .44 yGLM = .12 zGBM = .44 Pre-Procpargs2 
# Score  .93178 xRF = .29 yGLM = .12 zGBM = .30 WSVM=.29 Pre-Procpargs2 #BEST ENTRY OVERALL
# Score  .93169 xRF = .30 yGLM = .08 zGBM = .32 WSVM=.30 Pre-Procpargs2

# ========================================================================

# RF & GLM
xRF = .625
yGLM = .375
Probability1 = (PredTest.RF.prob*xRF+PredTest.GLM2.prob*yGLM)
MySubmission.StackedRFGLM = data.frame(UniqueID = NewsTest$UniqueID, Probability1)
write.csv(MySubmission.StackedRFGLM, "SubmissionPreProcCVStacked625RF375glm.csv", row.names=FALSE)

# RF & GLM & GBM & svm
xRF = .29
yGLM = .12
zGBM = .30
wSVM = .29
vKNN = .0

Probability1 = (PredTest.RF.prob*xRF+
                  PredTest.GLM2.prob*yGLM+
                  PredTest.gbm.prob*zGBM+
                  PredTest.svm.prob*wSVM+
                  PredTest.KNN.prob*vKNN
                  )

MySubmission.StackedRFGLMGBMSVMKNN = data.frame(UniqueID = NewsTest$UniqueID, Probability1)
write.csv(MySubmission.StackedRFGLMGBMSVMKNN, "SubmissionPreProcCVStacked225RF10glm225GBM225SVM225KNN.csv", row.names=FALSE)



#
# ========================================================================




# ========================================================================
# RUN GLM Model on SIG VARS - Score 0.91062 w pargs2, 0.91055 w pargs1
# ========================================================================

# - 10 folds cross-validated Tuned glm Model with sig variables
# Perform the cross validation
fitControl = trainControl("cv", 10, classProbs = TRUE, summaryFunction = twoClassSummary)
# GLM.tr1 = train(pargs1, data=Train, method="glm", family="binomial", metric="ROC", trControl=fitControl)
# PredTest.GLM1 = predict(GLM.tr1, newdata=Test, type="prob")
# PredTest.GLM1.prob = PredTest.GLM1[,2]

GLM.tr2 = train(pargs2, data=Train, method="glm", family="binomial", preProcess = c('center', 'scale'), metric="ROC", trControl=fitControl)
PredTest.GLM2 = predict(GLM.tr2, newdata=Test, type="prob")
PredTest.GLM2.prob = PredTest.GLM2[,2]

# Now we can prepare our submission file for Kaggle:
p = PredTest.GLM2.prob

MySubmission.CVGLM = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = p)
write.csv(MySubmission.CVGLM, "SubmissionTrainedGLM2.csv", row.names=FALSE)
#
# ========================================================================

# ========================================================================
# RUN RF Model on SIG VARS - Score 0.91829 w pargs2
# ========================================================================

# - 10 folds cross-validated Tuned RF Model with sig variables
# Perform the cross validation
fitControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
RF.tr = train(pargs2, data=Train, preProcess = c('center', 'scale'), method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)

# train(Popular~ ., dataa=a.tr, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

PredTest.RF = predict(RF.tr, newdata=Test, type="prob")
PredTest.RF.prob = PredTest.RF[,2]

# Now we can prepare our submission file for Kaggle:
MySubmission.cvRF = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest.RF.prob)
write.csv(MySubmission.cvRF, "SubmissiondTrainedRF.csv", row.names=FALSE)
#
# ========================================================================



# ========================================================================
# RUN GBM Model on SIG VARS - Score XXXXX pargs2 #BEST ENTRY OVERALL
# ========================================================================

# - 10 folds cross-validated Tuned GBM Model with sig variables
# Perform the cross validation
gbmGrid =  expand.grid(interaction.depth = c(5, 9),
                       n.trees = (1:3)*500,
                       shrinkage = c(0.05, 0.1))

fitControl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE)

GBM.tr = train(pargs2, preProcess = c('center', 'scale'), data=Train, method="gbm", verbose=TRUE, tuneGrid=gbmGrid, metric="ROC", trControl=fitControl)

PredTest.gbm = predict(GBM.tr, newdata=Test, type="prob")
PredTest.gbm.prob = PredTest.gbm[,2]

# Now we can prepare our submission file for Kaggle:
MySubmission.CVGBM = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest.gbm.prob)
write.csv(MySubmission.CVGBM, "SubmissiondtmPreProcTrainedGBM.csv", row.names=FALSE)

# ---------------------------- different fit control
# Perform the cross validation
gbmGrid =  expand.grid(interaction.depth = c(5, 9),
                       n.trees = (1:3)*500,
                       shrinkage = c(0.05, 0.1))

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = T,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

GBM.tr2 = train(pargs2, 
                preProcess = c('center', 'scale'), 
                data=Train, 
                method="gbm",
                verbose=T, 
                tuneGrid=gbmGrid, 
                metric="ROC", 
                trControl=fitControl)

PredTest.gbm2 = predict(GBM.tr2, newdata=Test, type="prob")
PredTest.gbm2.prob = PredTest.gbm2[,2]

# Now we can prepare our submission file for Kaggle:
MySubmission.CVGBM2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest.gbm2.prob)
write.csv(MySubmission.CVGBM2, "SubmissiondtmTrainedGBM2.csv", row.names=FALSE)
#







# ========================================================================
# RUN SVM Model on SIG VARS - Score XXXXX pargs2 #BEST ENTRY OVERALL
# ========================================================================

# - 10 folds cross-validated Tuned SVM Model with sig variables
# Perform the cross validation
svmTuneGrid <- expand.grid(.C=3^(-15:15))

fitControl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  number=10,
  classProb = T,
  savePred=F,
  allowParallel = TRUE
)


SVM.tr = train(pargs2, 
               data=Train, 
               method="svmRadial", 
               tunelength = 9,
               metric="ROC",
               preProcess = c('center', 'scale'),
               verbose=T, 
               trControl = fitControl
               )

PredTest.svm = predict(SVM.tr, newdata=Test, type="prob")
PredTest.svm.prob = PredTest.svm[,2]

# Now we can prepare our submission file for Kaggle:
MySubmission.CVSVM = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest.svm.prob)
write.csv(MySubmission.CVSVM, "SubmissiondtmTrainedSVM.csv", row.names=FALSE)
#
# ========================================================================


# ========================================================================
# RUN KNN Model on SIG VARS - Score  w pargs2
# ========================================================================

# - 10 folds cross-validated Tuned RF Model with sig variables
# Perform the cross validation

fitControl = trainControl(classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          method="repeatedcv",
                          repeats = 5)
knn.tr3 = train(pargs3, 
              data=Train, 
              preProcess = c('center', 'scale'), 
              method="knn", 
              metric="ROC", 
              trControl=fitControl,
              tuneLength = 20)


PredTest.KNN = predict(knn.tr, newdata=Test, type="prob")
PredTest.KNN.prob = PredTest.KNN[,2]

# Now we can prepare our submission file for Kaggle:
MySubmission.cvKNN = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest.KNN.prob)
write.csv(MySubmission.cvKNN, "SubmissiondTrainedKNN.csv", row.names=FALSE)
#
# ========================================================================

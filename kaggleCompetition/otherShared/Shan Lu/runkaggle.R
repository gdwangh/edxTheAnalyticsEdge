# edX-MIT Analytics Edge (Spring 2015): Kaggle Competition
# Author: Shan Lu (GitHub: baizhima) 
# Contact: shanlu at brown dot edu
# Affiliation: Brown University, Computer Science
# Date: Apr. 25th, 2015

# Set environment variables (locale to English)
Sys.setlocale("LC_ALL","C")
Sys.setenv(LANG="en")

### 1. Load libraries/data
# 1.1 Load libraries
library(tm)
library(SnowballC)
library(caTools)
library(ROCR)
library(rpart)
library(caret)
library(randomForest)
library(e1071) # svm

# 1.2 Load Training/Test datasets
# UniqueID: Train 1-6532, Test 6533-8402
setwd("~/Desktop/coursera/analyticsEdge/kaggle/")
Train = read.csv("NYTimesBlogTrain.csv",stringsAsFactors=FALSE)
Test = read.csv("NYTimesBlogTest.csv",stringsAsFactors=FALSE)
testID = Test$UniqueID

# 1.3 Merge Train and Test set into Data
Test$Popular = 2
Data = rbind(Train, Test)

# 1.4 Find patterns on Headline(research purposes)
popularData = subset(Data, Popular == 1)
unpopularData = subset(Data, Popular == 0)




### 2. Build Abstract Corpus
# 2.1 Build Headline Corpus
corpusH = Corpus(VectorSource(Data$Headline))
corpusH = tm_map(corpusH, tolower)
corpusH = tm_map(corpusH, PlainTextDocument)
corpusH = tm_map(corpusH, removePunctuation)
corpusH = tm_map(corpusH, removeWords, stopwords("english"))
corpusH = tm_map(corpusH, stemDocument)
frequenciesH = DocumentTermMatrix(corpusH)
sparseH = removeSparseTerms(frequenciesH, 0.995) # 142 words remained
HeadlineSparse = as.data.frame(as.matrix(sparseH))
colnames(HeadlineSparse) = paste0("h_",colnames(HeadlineSparse))
HeadlineSparse$UniqueID = Data$UniqueID

# 2.2 Build Abstract Corpus
corpusA = Corpus(VectorSource(Data$Abstract))
corpusA = tm_map(corpusA, tolower)
corpusA = tm_map(corpusA, PlainTextDocument)
corpusA = tm_map(corpusA, removePunctuation)
corpusA = tm_map(corpusA, removeWords, stopwords("english"))
corpusA = tm_map(corpusA, stemDocument)
frequenciesA = DocumentTermMatrix(corpusA)
sparseA = removeSparseTerms(frequenciesA, 0.985) # 223 words remained 0.99 
AbstractSparse = as.data.frame(as.matrix(sparseA))
colnames(AbstractSparse) = paste0("a_",colnames(AbstractSparse))
AbstractSparse$UniqueID = Data$UniqueID

### 3. Build cleaned data for modeling

# 3.1 Merge words from Headline and Abstract corpus together
wordSparse = merge(HeadlineSparse, AbstractSparse, by="UniqueID")
allData = merge(Data, wordSparse, by="UniqueID")

# 3.2 Ignore useless variables
allData$Abstract = NULL
allData$Snippet = NULL
allData$Headline = NULL 
allData$UniqueID = NULL

# 3.3 Factorize section/subsetction name strings
allData$NewsDesk = as.factor(allData$NewsDesk)
allData$SectionName = as.factor(allData$SectionName)
allData$SubsectionName = as.factor(allData$SubsectionName)
#allData$Popular = as.factor(allData$Popular)

# 3.4 Extract Year, Month, Day, HH/MM/SS from $PubDate
allData$PubDate = strptime(allData$PubDate,format="%Y-%m-%d %H:%M:%S")
allData$Weekday = as.factor(weekdays(allData$PubDate))
allData$Hour = as.factor(allData$PubDate$hour)
allData$PubDate = NULL

# 3.5 split word counts into different levels by quartiles
quartiles = summary(allData$WordCount)
wordsLevel = c(quartiles["1st Qu."], quartiles["Median"],quartiles["3rd Qu."])
allData$LenType = as.integer(allData$WordCount > wordsLevel[1])
allData$LenType = allData$LenType + as.integer(allData$WordCount > wordsLevel[2])
allData$LenType = allData$LenType + as.integer(allData$WordCount > wordsLevel[3])
allData$LenType = as.factor(allData$LenType)
allData$WordCount = NULL

# 3.6 Rearrange column orders by name
allData = allData[, order(names(allData))] 

# 3.7 Split Train/Test data by Popular
blogTrain = subset(allData, Popular <= 1)
set.seed(20)
spl = sample.split(blogTrain$Popular, SplitRatio = 0.9)
pureTrain = subset(blogTrain, spl==TRUE)
pureVal = subset(blogTrain, spl==FALSE)
blogTest = subset(allData, Popular == 2)
blogTest$Popular = NULL # to be predicted

### 4. Model 4: Logistic Regression
# 4.1 Build LR model
modelLR = glm(Popular ~ ., data=pureTrain, family="binomial")

# 4.2 Make predictions on validation data
predictLR = predict(modelLR, newdata=pureVal, type='response')
predLR = prediction(predictLR, pureVal$Popular)
aucLR = as.numeric(performance(predLR, "auc")@y.values)
aucLR # 0.9249

# 4.3 Build LR model using (pureTrain + pureVal) data
modelLR = glm(Popular ~ ., data=blogTrain, family="binomial")

# 4.4 Write predictions into CSV file "submissionRF.csv"
predictLR2 = predict(modelLR, newdata=blogTest, type='response')
predictLR2[predictLR2 > 1] = 1.00 # fix floating-point arithmetic errors
predictLR2[predictLR2 < 0] = 0.00 # fix floating-point arithmetic errors
submissionLR = data.frame(UniqueID=testID, Probability1= predictLR2)
write.csv(submissionLR, "submissionLR.csv", row.names=FALSE)

### 5. Model 2: Random Forest
# 5.1 Build RF model using training(pureTrain) data
# nodesize | ntree | auc
#   25     |  200  | 0.9207
#   50     |  200  | 0.9343
#   75     |  200  | 0.9355
#  100     |  200  | 0.9378
#  150     |  200  | 0.9360
#  100     |  250  | 0.9379
modelRF = randomForest(Popular ~ ., data=pureTrain, nodesize=100, ntree=250)



# 5.2 Make predictions on validation data
predictRF = predict(modelRF, newdata=pureVal, type='class')
predRF = prediction(predictRF, pureVal$Popular)
aucRF = as.numeric(performance(predRF, "auc")@y.values)
aucRF # 0.9379

# 5.3 Build RF model using (pureTrain + pureVal) data
modelRF = randomForest(Popular ~ ., data=blogTrain, nodesize=100, ntree=250)

# 5.4 Write predictions into CSV file "submissionRF.csv"
predictRF2 = predict(modelRF, newdata=blogTest, type='class')
predictRF2[predictRF2 > 1] = 1.00 # fix floating-point arithmetic errors
predictRF2[predictRF2 < 0] = 0.00 # fix floating-point arithmetic errors
submissionRF = data.frame(UniqueID = testID, Probability1=predictRF2)
write.csv(submissionRF, "submissionRF.csv", row.names=FALSE)

### 6. Model 3: Support Vector Machine
# 6.1 Build SVM model on pureTrain data
modelSVM = svm(Popular ~ ., data=pureTrain)

# 6.2 Make predictions on validation data
predictSVM = predict(modelSVM, newdata= pureVal, type="class")
predSVM = prediction(predictSVM, pureVal$Popular)
aucSVM = as.numeric(performance(predSVM, "auc")@y.values)
aucSVM

# 6.3 Build SVM model using (pureTrain + pureVal) data
modelSVM = svm(Popular ~ . - UniqueID, data=blogTrain)

# 6.3 Write predictions into CSV file "submissionSVM.csv"
predictSVM2 = predict(modelSVM, newdata=blogTest)
predictSVM2[predictSVM2 > 1] = 1.00
predictSVM2[predictSVM2 < 0] = 1e-8
submissionSVM = data.frame(UniqueID = testID, Probability1=predictSVM2)
write.csv(submissionSVM, "submissionSVM.csv", row.names=FALSE)


### 7. Ensemble learning
# 7.1 Define ensemble equation
#   RF     |  LR   | auc
#   0.6    |  0.4  | 0.9408
#   0.7    |  0.3  | 0.9413
#   0.75   |  0.25 | 0.9414
#   0.8    |  0.2  | 0.9408

predictEnsemble = 0.5 * predictRF + 0.5 * predictLR
# 7.2 Make predictions on validation data
predEnsemble = prediction(predictEnsemble, pureVal$Popular)
aucEnsemble = as.numeric(performance(predEnsemble, "auc")@y.values)
aucEnsemble # 0.9360
# 7.3 Write predictions into CSV file "submissionEnsemble.csv"
predictEnsemble2 = 0.5 * predictRF2 + 0.5 * predictLR2
predictEnsemble2[predictEnsemble2 > 1] = 1.00
predictEnsemble2[predictEnsemble2 < 0] = 0.00
submissionEnsemble = data.frame(UniqueID = testID, Probability1 = predictEnsemble2)
write.csv(submissionEnsemble, "submissionEnsemble.csv", row.names=FALSE)
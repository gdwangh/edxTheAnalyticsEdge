# File Descriptions
# The data provided for this competition is split into two files:
# NYTimesBlogTrain.csv = the training data set. It consists of 6532 articles.
# NYTimesBlogTest.csv = the testing data set. It consists of 1870 articles.  
# We have also provided a sample submission file, SampleSubmission.csv. This file gives an example of the
# format of submission files (see the Evaluation page for more information). The data for this competition
# comes from the New York Times website.

# Variable Descriptions
# The dependent variable in this problem is the variable Popular, which labels if an article had 25 or more
# comments in its online comment section (equal to 1 if it did, and 0 if it did not). The dependent variable
# is provided in the training data set, but not the testing dataset. This is an important difference from
# what you are used to - you will not be able to see how well your model does on the test set until you make
# a submission on Kaggle.

# The independent variables consist of 8 pieces of article data available at the time of publication, and a
# unique identifier:

# NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
# SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
# SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
# Headline = the title of the article
# Snippet = a small portion of the article text
# Abstract = a summary of the blog article, written by the New York Times
# WordCount = the number of words in the article
# PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
# UniqueID = a unique identifier for each article

a = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
b = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

a$PubDate = strptime(a$PubDate, "%Y-%m-%d %H:%M:%S")
b$PubDate = strptime(b$PubDate, "%Y-%m-%d %H:%M:%S")

a$PubDay = a$PubDate$wday
b$PubDay = b$PubDate$wday

a$PubHour = a$PubDate$hour
b$PubHour = b$PubDate$hour

popTemp = a$Popular
a$Popular = NULL

c = rbind(a,b)

c$NewsDesk[c$NewsDesk==""] = "NDeskNA"
c$SectionName[c$SectionName==""] = "SecNA"
c$SubsectionName[c$SubsectionName==""] = "SSecNA"

c$NewsDesk = as.factor(c$NewsDesk)
c$SectionName = as.factor(c$SectionName)
c$SubsectionName = as.factor(c$SubsectionName)
c$PubDay = as.factor(c$PubDay)
c$PubHour = as.factor(c$PubHour)
c$Snippet = NULL
#c$Abstract = NULL

# Create a corpus using only the headline
require(tm)

# Create corpus
# use text from abstract
corpus = Corpus(VectorSource(c$Headline))
# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
# Remove stopwords and a few others and create DTM
corpus = tm_map(corpus, removeWords, c(stopwords("english"), "dec", "hong", "times", "new", "york","week","daily","news", "today", "day", "city"))
textfreq = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(textfreq, 0.999) # get lots of terms
# create a data frame
textfields = as.data.frame(as.matrix(sparse))
# clean up the names
colnames(textfields) = make.names(colnames(textfields))
textfields$WordCount = c$WordCount
# create a data frame
trndf = head(textfields,nrow(a)) #do this only for filtering because we need a matrix of numerical values

# convert trn to a matrix of inputs (notice we don't have Popular in this frame)
Xvals = as.matrix(trndf)
# get the Y vector
Yvals = popTemp
require(glmnet)
# if want to use multiple cores, comment out if not
require(doParallel)
cl = makeCluster(detectCores())
registerDoParallel(cl)

# below due to oconnoda and Utah777
m1glmnet = cv.glmnet(Xvals, Yvals, family='binomial', type.measure='auc', parallel=TRUE, intercept=FALSE, alpha=1)
coeffs = coef(m1glmnet, s = "lambda.1se")
coeffs = as.data.frame(as.table(as.matrix(coeffs)))
filtered_coeffs = coeffs[abs(coeffs$Freq) > 0,] # Set 0 to higher if you want to cull extra parameters
filtered_coeffs = filtered_coeffs[order(abs(filtered_coeffs$Freq), decreasing = TRUE),] # View these to look through the selected variables in decreasing order of importance

# Select out features and create new Training and Test data
# add back the dependent variable, and some others
fil_features = names(trndf)[names(trndf) %in% filtered_coeffs$Var1]
textfields_filtered = textfields[,fil_features]

textfields_filtered$NewsDesk = as.factor(c$NewsDesk)
textfields_filtered$SectionName = as.factor(c$SectionName)
textfields_filtered$SubsectionName = as.factor(c$SubsectionName)

textfields_filtered$PubDay = as.factor(c$PubDay)
textfields_filtered$PubHour = as.factor(c$PubHour)

# create trn and tst datasets
trn = head(textfields_filtered,nrow(a))
trn$Popular = as.factor(popTemp)
tst = tail(textfields_filtered,nrow(b))

# so we now have our processed tst and trn datasets

# MODELS

# GBM with full trn
# create a GMB model with the full dataset

require(gbm)
m2gbm = gbm(Popular~.,data=trn,n.trees=500,shrinkage=0.05,interaction.depth=4,n.minobsinnode=10, distribution="multinomial")
p2gbmtrn = predict.gbm(object=m2gbm,newdata=trn,n.trees=500,type="response")
table(trn$Popular,p2gbmtrn[,2,]>=0.5)
# Compute AUC on trainig
require(ROCR)
predROCR = prediction(p2gbmtrn[,2,], trn$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values
# AUC on training is 0.974952
p2gbmtst = predict.gbm(object=m2gbm,newdata=tst,n.trees=500,type="response")
p2gbmtstP = p2gbmtst[,2,]
uidtst = tail(c$UniqueID,1870)
sub = data.frame(UniqueID = uidtst, Probability1 = p2gbmtst[,2,])
# This is the model that got #95 on the private leaderboard

# glm model on full dataset
m2glm = glm(Popular~.,data=trn,family=binomial)
p2glmtrn = predict(m2glm,type="response")
table(trn$Popular,p2glmtrn>=0.5)
# Compute AUC on training
require(ROCR)
predROCR = prediction(p2glmtrn, trn$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
# plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
pm2glmtst = predict(m2glm, newdata = tst, type="response")
# DIDNT SUBMIT THIS ONE

# rf on full dataset
# recreate the rf model above
set.seed(123)
require(randomForest)
#kfold(trn,randomForest,pargs=list(type="prob"),nodesize=1,ntree=500)
m2rf = randomForest(Popular ~ .,data = trn, nodesize = 5, ntree = 500)
p2rftrn = predict(m2rf,type="prob")[,2]
table(trn$Popular,p2rftrn>=0.5)
# Compute AUC on training
require(ROCR)
predROCR = prediction(p2rftrn, trn$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
# plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
pm2rftst = predict(m2rf, newdata = tst, type="prob")[,2]
# DIDNT SUBMIT THIS ONE

# svm on full dataset
# create a SVM  model with full dataset
require(e1071)
m2svm = svm(Popular~.,data=trn,probability=TRUE)
p2svmtrn = predict(m2svm,newdata=trn,probability=TRUE)
pp1=attr(p2svmtrn,"probabilities")[,1]
table(trn$Popular,pp1>=0.5)
# accuracy on training set svm 0.9009492
p2svmtst = predict(m2svm, newdata=tst,probability=TRUE)
p2svmtstP = attr(p2svmtst,"probabilities")[,1]
# DIDNT SUBMIT THIS ONE

# create different submissions by averaging combimations of the above 4 models for the full dataset
# vectors are pm1glmtst, p1gbmtstP, p1svmtstP, pm1rftst
dffull = cbind(pm2glmtst, p2gbmtstP, p2svmtstP, pm2rftst)
cor(dffull)
fullAvg = as.numeric((pm2glmtst+p2gbmtstP+p2svmtstP+pm2rftst)/4)
sub = data.frame(UniqueID = uidtst, Probability1 = fullAvg)
#write.csv(sub, "sub0504-04.csv", row.names=FALSE)
# resulted in AUC of 0.92928 on LB. Meh

# submit one with just GBM and rf models
fullAvgHalf = as.numeric((p2gbmtstP+pm2rftst))/2
sub = data.frame(UniqueID = uidtst, Probability1 = fullAvgHalf)
#write.csv(sub, "sub0504-05.csv", row.names=FALSE)
# resulted in AUC of 0.93126
# THIS WAS THE SECOND SUBMISSION, DIDNT DO AS WELL AS GBM ALONE



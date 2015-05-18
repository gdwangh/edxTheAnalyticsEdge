# Script by Scott Stevens (stevens@fulbrightmail.org)


## KAGGLE COMPETITION

# 1. NewsDesk       -   NYT desk that produced story (Business, Culture, Foreign, etc.)
# 2. SectionName    -   section article appeared in (Opinion, Arts, Technology, etc.)
# 3. SubsectionName -   subsection article appeared in (Education, Small Business, Room for Debate, etc.)
# 4. Headline       -   article title
# 5. Snippet        -   small portion of article text
# 6. Abstract       -   a summary of the blog article, written by the New York Times
# 7. WordCount      -   the number of words in the article
# 8. PubDate        -   the publication date, in the format "Year-Month-Day Hour:Minute:Second"
# 9. UniqueID       -   a unique identifier for each article
# 10. Popular       -   "1" if number of comments >= 25

#################################################
# Here goes...

library(tm)
library(SnowballC)
library(ROCR)
library(randomForest)
library(caTools)
library(caret)
library(pROC)
library(gbm)

# read data
train <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors = FALSE)
test <- read.csv("NYTimesBlogTest.csv", stringsAsFactors = FALSE)

# join train and test for processing (remove Popular and UniqueID to do so)
trimtrain <- train[1:8]
trimtest <- test[1:8]
combined <- rbind(trimtrain, trimtest)

# format time, add Day and Hour
combined$PubDate <- strptime(combined$PubDate, "%Y-%m-%d %H:%M:%S")
combined$Day <- combined$PubDate$wday
combined$Hour <- combined$PubDate$hour

# question mark, sex, NY
combined$Question <- as.factor(grepl("[?]", combined$Headline))
combined$Sex <- as.factor(grepl("nude|naked|sex|intercourse|porn|penis|orgasm", combined$Headline, ignore.case = TRUE))
combined$NY <- as.factor(grepl("New York|NY", combined$Headline, ignore.case = TRUE))

# make factor of NewsDesk, SectionName, and SubsectionName
combined$NewsDesk <- as.factor(combined$NewsDesk)
combined$SectionName <- as.factor(combined$SectionName)
combined$SubsectionName <- as.factor(combined$SubsectionName)

# split back to train/test
trainB <- head(combined, nrow(train))
testB <- tail(combined, nrow(test))

# put Popular back in train, make factor
trainB$Popular <- as.factor(train$Popular)


#################################################
# Text Preprocessing

# replacement of some n-grams
train$Headline <- gsub("New York|New York City", "NewYork", train$Headline, ignore.case = TRUE)
test$Headline <- gsub("New York|New York City", "NewYork", test$Headline, ignore.case = TRUE)

train$Abstract <- gsub("New York|New York City", "NewYork", train$Abstract, ignore.case = TRUE)
test$Abstract <- gsub("New York|New York City", "NewYork", test$Abstract, ignore.case = TRUE)

# headlines
corpus.hl <- Corpus(VectorSource(c(train$Headline, test$Headline)))
corpus.hl <- tm_map(corpus.hl, tolower)
corpus.hl <- tm_map(corpus.hl, PlainTextDocument)
corpus.hl <- tm_map(corpus.hl, removePunctuation)
corpus.hl <- tm_map(corpus.hl, removeWords, stopwords("english"))
corpus.hl <- tm_map(corpus.hl, stemDocument, language = "english")
# corpus.hl <- Corpus(VectorSource(corpus.hl)) # Only needed on Mac?
dtm.hl <- DocumentTermMatrix(corpus.hl)
sparse.hl <- removeSparseTerms(dtm.hl, 0.995)
headlines <- as.data.frame(as.matrix(sparse.hl))
colnames(headlines) <- make.names(colnames(headlines))

# split headlines back to train/test, add Popular back in
headlines.train <- head(headlines, nrow(train))
headlines.test <- tail(headlines, nrow(test))
headlines.train$Popular <- as.factor(train$Popular)

# abstracts
corpus.ab <- Corpus(VectorSource(c(train$Abstract, test$Abstract)))
corpus.ab <- tm_map(corpus.ab, tolower)
corpus.ab <- tm_map(corpus.ab, PlainTextDocument)
corpus.ab <- tm_map(corpus.ab, removePunctuation)
corpus.ab <- tm_map(corpus.ab, removeWords, stopwords("english"))
corpus.ab <- tm_map(corpus.ab, stemDocument, language = "english")
# corpus.ab <- Corpus(VectorSource(corpus.ab)) # Only needed on Mac?
dtm.ab <- DocumentTermMatrix(corpus.ab)
sparse.ab <- removeSparseTerms(dtm.ab, 0.995)
abstracts <- as.data.frame(as.matrix(sparse.ab))
colnames(abstracts) <- make.names(colnames(abstracts))

# split abstracts back to train/test, add Popular back in
abstracts.train <- head(abstracts, nrow(train))
abstracts.test <- tail(abstracts, nrow(test))
abstracts.train$Popular <- as.factor(train$Popular)

# making headlines+abstract set
headlines.c <- headlines
abstracts.c <- abstracts

colnames(headlines.c) <- paste("HL", colnames(headlines.c))
colnames(abstracts.c) <- paste("AB", colnames(abstracts.c))
colnames(headlines.c) <- make.names(colnames(headlines.c))
colnames(abstracts.c) <- make.names(colnames(abstracts.c))
headabs <- cbind(headlines.c, abstracts.c)

headabs.train <- head(headabs, nrow(train))
headabs.test <- tail(headabs, nrow(test))
headabs.train$Popular <- as.factor(train$Popular)


#################################################
# Model 2: Log regression + Day + Hour

LR.2 <- glm(Popular ~ NewsDesk + SectionName + SubsectionName + log(WordCount + 50) + Day + Hour + Question + Sex + NY,
            data = trainB, family = binomial)

# accuracy on train
trainPred.2 <- predict(LR.2, type = "response")
table(trainB$Popular, trainPred.2 >= 0.5)
as.numeric(performance(prediction(trainPred.2, trainB$Popular), "auc")@y.values)
# train AUC: 0.9392159

# test predictions, save as csv
testPred.2 <- predict(LR.2, newdata = testB, type = "response")
Model.2 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.2)
write.csv(Model.2, "Kaggle_submission_2.csv", row.names = FALSE)
# test AUC: 0.90783


#################################################
# Model 3: Random forest

set.seed(123)
RF.3 <- randomForest(Popular ~ NewsDesk + SectionName + SubsectionName + WordCount, data = trainB, nodesize = 8)

# accuracy on train
trainPred.3 <- predict(RF.3, type = "prob")[,2]
table(trainB$Popular, trainPred.3 > 0.5)
as.numeric(performance(prediction(trainPred.3, trainB$Popular), "auc")@y.values)
# train AUC: 0.909717

# test predictions, save as csv
testPred.3 <- predict(RF.3, newdata = testB, type = "prob")[,2]
Model.3 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.3)
write.csv(Model.3, "Kaggle_submission_3.csv", row.names = FALSE)
# test AUC: 0.89557

#################################################
# Model 3t: model 3 trained

IV.3t <- trainB[c(1:3, 7)] # apparently it's less buggy if you remove everything not relevant
set.seed(123)
RF.3t <- train(IV.3t, trainB$Popular, method = "rf", nodesize = 5, ntree = 500, metric = "ROC",
                  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary))

# accuracy on train
RF.3t$finalModel
trainPred.3t <- predict(RF.3t$finalModel, type = "prob")[,2]
as.numeric(performance(prediction(trainPred.3t, trainB$Popular), "auc")@y.values)
# train AUC: 0.9121861

# test predictions, save as csv
testPred.3t <- predict(RF.3t$finalModel, newdata = testB, type = "prob")[,2]
Model.3t <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.3t)
write.csv(Model.3t, "Kaggle_submission_3t.csv", row.names = FALSE)
# test AUC: 0.89640


#################################################
# Model 4: Random forest but with Day + Hour

set.seed(123)
RF.4 <- randomForest(Popular ~ NewsDesk + SectionName + SubsectionName + WordCount + Day + Hour + Question, data = trainB, nodesize = 1)

# accuracy on train
trainPred.4 <- predict(RF.4, type = "prob")[,2]
table(trainB$Popular, trainPred.4 > 0.5)
as.numeric(performance(prediction(trainPred.4, trainB$Popular), "auc")@y.values)
# train AUC: 0.9386863

# test predictions, save as csv
testPred.4 <- predict(RF.4, newdata = testB, type = "prob")[,2]
Model.4 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.4)
write.csv(Model.4, "Kaggle_submission_4.csv", row.names = FALSE)
# test AUC: 0.92516
# test AUC: 0.92368 (w/Question)


#################################################
# Model 4t: model 4 trained

IV.4t <- trainB[c(1:3, 7, 9:11)] # apparently it's less buggy if you remove everything not relevant
set.seed(123)
RF.4t <- train(IV.4t, trainB$Popular, method = "rf", nodesize = 5, ntree = 500, metric = "ROC",
               trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary))

# accuracy on train
RF.4t$finalModel
trainPred.4t <- predict(RF.4t$finalModel, type = "prob")[,2]
as.numeric(performance(prediction(trainPred.4t, trainB$Popular), "auc")@y.values)
# train AUC: 0.9361285

# test predictions, save as csv
testPred.4t <- predict(RF.4t$finalModel, newdata = testB, type = "prob")[,2]
Model.4t <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.4t)
write.csv(Model.4t, "Kaggle_submission_4t.csv", row.names = FALSE)
# test AUC: 0.92331 (w/o cv)


#################################################
# Model 5: analysis of HEADLINES

set.seed(123)
RF.5 <- randomForest(Popular ~ ., data = headlines.train)

# accuracy on train
trainPred.5 <- predict(RF.5, type = "prob")[,2]
table(headlines.train$Popular, trainPred.5 > 0.5)
as.numeric(performance(prediction(trainPred.5, trainB$Popular), "auc")@y.values)
# train AUC: 0.5340462

# test predictions, save as csv
testPred.5 <- predict(RF.5, newdata = headlines.test, type = "prob")[,2]
Model.5 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.5)
write.csv(Model.5, "Kaggle_submission_5.csv", row.names = FALSE)
# test AUC: 


#################################################
# Model 6: analysis of ABSTRACTS

set.seed(123)
RF.6 <- randomForest(Popular ~ ., data = abstracts.train)

# accuracy on train
trainPred.6 <- predict(RF.6, type = "prob")[,2]
table(abstracts.train$Popular, trainPred.6 > 0.5)
as.numeric(performance(prediction(trainPred.6, trainB$Popular), "auc")@y.values)
# train AUC: 0.7901391

# test predictions, save as csv
testPred.6 <- predict(RF.6, newdata = abstracts.test, type = "prob")[,2]
Model.6 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.6)
write.csv(Model.6, "Kaggle_submission_6.csv", row.names = FALSE)
# test AUC: 0.76559

#################################################
# Model 7: analysis of HEADLINES + ABSTRACTS

set.seed(123)
RF.7 <- randomForest(Popular ~ ., data = headabs.train)

# accuracy on train
trainPred.7 <- predict(RF.7, type = "prob")[,2]
table(headabs.train$Popular, trainPred.7 > 0.5)
as.numeric(performance(prediction(trainPred.7, trainB$Popular), "auc")@y.values)
# train AUC: 0.8170428

# test predictions, save as csv
testPred.7 <- predict(RF.7, newdata = headabs.test, type = "prob")[,2]
Model.7 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.7)
write.csv(Model.7, "Kaggle_submission_7.csv", row.names = FALSE)
# test AUC: 0.80050


#################################################
# Model 7t: model 7 trained

IV.7t <- headabs.train[1:667] # apparently it's less buggy if you remove everything not relevant
RF.7t <- train(IV.7t, headabs.train$Popular, method = "rf", nodesize = 5, ntree = 500, metric = "ROC",
               trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary))

# accuracy on train
RF.7t$finalModel
trainPred.7t <- predict(RF.7t$finalModel, type = "prob")[,2]
as.numeric(performance(prediction(trainPred.7t, headabs.train$Popular), "auc")@y.values)
# train AUC: 0.8132872

# test predictions, save as csv
testPred.7t <- predict(RF.7t$finalModel, newdata = headabs.test, type = "prob")[,2]
Model.7t <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.7t)
write.csv(Model.7t, "Kaggle_submission_7t.csv", row.names = FALSE)
# test AUC: 0.79137

#################################################
# Model 8: GBM

train.8 <- trainB[c(1:3, 7, 9:11)]
train.8$Popular <- train$Popular
test.8 <- testB[c(1:3, 7, 9:11)]

set.seed(123)
gbm.8 <- gbm(formula = Popular ~ NewsDesk + SectionName + SubsectionName + WordCount + Day + Hour + Question,
             data = train.8, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4, n.minobsinnode = 10)
gbm.perf(gbm.8)

# accuracy on train
trainPred.8 <- predict(gbm.8, n.trees = 10000, type = "response")
table(trainB$Popular, trainPred.8 > 0.5)
as.numeric(performance(prediction(trainPred.8, trainB$Popular), "auc")@y.values)
# train AUC: 0.9798309

# test predictions, save as csv
testPred.8 <- predict(gbm.8, newdata = test.8, n.trees = 10000, type = "response")
Model.8 <- data.frame(UniqueID = test$UniqueID, Probability1 = testPred.8)
write.csv(Model.8, "Kaggle_submission_8.csv", row.names = FALSE)
# test AUC: 0.92764


#################################################
# Model Ensemble

Model.E <- data.frame(UniqueID = test$UniqueID, Probability1 = 0.1*testPred.2 + 0.1*testPred.4t + 0.1*testPred.7 + 0.7*testPred.8)
write.csv(Model.E, "Kaggle_submission_E.csv", row.names = FALSE)

# accuracy on train
trainPred.E <- 0.1*trainPred.2 + 0.1*trainPred.4t + 0.1*trainPred.7 + 0.7*trainPred.8
table(trainB$Popular, trainPred.E >= 0.5)
as.numeric(performance(prediction(trainPred.E, trainB$Popular), "auc")@y.values)
# train AUC: 0.9759312

# test AUC: 0.92755 (0.35/0.6/0.05 split for 2, 4, 7)
# test AUC: 0.92594 (0.5/0.4/0.1 split for 2, 4, 7--shows that maximizing on train set (couple lines above) doesn't yield the best)
# test AUC: 0.92563 (0.5/0.4/0.1 split for 2, 4t, 7)
# test AUC: 0.93030 (0.1/0.1/0.1/0.7 split for 2 4t, 7, 8)

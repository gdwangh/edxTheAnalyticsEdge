setwd('D:/workspace/The Analytics Edge/unit6')

# 1.1 load data
stocks = read.csv("stocksCluster.csv")
str(stocks)

# 1.2
table(stocks$PositiveDec)
6324/nrow(stocks)

# 1.3
sort(cor(stocks))

# 1.4
colMeans(stocks)


# 2.1
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

stockTrainlm = glm(PositiveDec~., data=stocksTrain, family="binomial")
stockTrainlm.pred = predict(stockTrainlm,type="response")

table(stocksTrain$PositiveDec, stockTrainlm.pred>0.5)
(990+3640)/nrow(stocksTrain)

# 2.2
stockTrainlm.predTest = predict(stockTrainlm, newdata = stocksTest, type="response")
table(stocksTest$PositiveDec, stockTrainlm.predTest>0.5)
(417+1553)/nrow(stocksTest)

# 2.3 baseline
table(stocksTrain$PositiveDec)
4427/nrow(stocksTrain)


# 3.1  cluster the stocks
limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

# 3.2 normalize data
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

# 3.3
summary(normTrain$ReturnJan)

summary(normTest$ReturnJan)

# 3.4
set.seed(144)
km = kmeans(normTrain, centers = 3)

table(km$cluster)

# 3.5 obtain training set and testing set cluster assignments 
library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

# 3.6
stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)

stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# 3.7
StocksModel1 = glm(PositiveDec~., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec~., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec~., data=stocksTrain3, family="binomial")

StocksModel1$coefficient 
StocksModel2$coefficient 
StocksModel3$coefficient 

# 3.8
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
table(stocksTest1$PositiveDec, PredictTest1>0.5)
(30+774)/nrow(stocksTest1)

PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
table(stocksTest2$PositiveDec, PredictTest2>0.5)
(388+757)/nrow(stocksTest2)

PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
table(stocksTest3$PositiveDec, PredictTest3>0.5)
(49+13)/nrow(stocksTest3)

# 4.4 compute the overall test-set accuracy of the cluster-then-predict approach
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions>0.5)

(467+1544)/length(AllOutcomes)


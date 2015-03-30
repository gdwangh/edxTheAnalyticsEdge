setwd('D:/workspace/The Analytics Edge/unit4')
setwd('D:/doc/study/TheAnalyticsEdge/unit4')


# 1.1
census = read.csv("census.csv")
str(census)

set.seed(2000)
spl = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

over50klm = glm(over50k~., data=train, family="binomial")
summary(over50klm)

# 1.2
over50klm.pred = predict(over50klm, newdata = test, type = "response")
table(test$over50k, over50klm.pred>=0.5)

(9051+1888)/nrow(test)

# 1.3
table(train$over50k)   # baseline: <=50k

table(test$over50k)

9713/nrow(test)

# 1.4
library(ROCR)
ROCRpred = prediction(over50klm.pred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
plot(performance(ROCRpred, "tpr", "fpr"))

# 2.1
library(rpart)
library(rpart.plot)

over50kTree = rpart(over50k~., data = train, method = "class")
prp(over50kTree)

# 2.4
over50kTree.pred = predict(over50kTree, newdata=test, type = "class")
table(test$over50k, over50kTree.pred)

(9243 + 1596)/nrow(test)

# 2.5
library(ROCR)

Pred = predict(over50kTree, newdata = test)
Pred = Pred[,2]
ROCRpred = prediction(Pred, test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)


# 3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
censusRF = randomForest(over50k~., data=trainSmall)
censusRF.pred = predict(censusRF, newdata=test)
table(test$over50k, censusRF.pred)
(9586+1093)/nrow(test)

# 3.2
vu = varUsed(censusRF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusRF$forest$xlevels[vusorted$ix]))

# 3.3
varImpPlot(censusRF)


# 4.1
library(caret)
library(e1071)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# 4.2
censusTreeCV = rpart(over50k ~ ., data = train, method="class", cp = 0.002)
censusTreeCV.pred = predict(censusTreeCV, newdata=test,type="class")
table(test$over50k, censusTreeCV.pred)
(9178+1838)/nrow(test)

# 4.3
prp(censusTreeCV)

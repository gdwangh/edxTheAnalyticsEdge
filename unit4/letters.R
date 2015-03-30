setwd('D:/workspace/The Analytics Edge/unit4')
setwd('D:/doc/study/TheAnalyticsEdge/unit4')

# 1.1
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(train$isB)
1175/(1175+383)

# 1.2
library(rpart)
set.seed(111)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
CARTb.pred = predict(CARTb, newdata=test, type="class")
table(test$isB, CARTb.pred)
(1118+340)/nrow(test)

# 1.3
library(randomForest)
set.seed(111)
RFb = randomForest(isB~. - letter, data = train)
RFb.pred = predict(RFb, newdata=test)
table(test$isB, RFb.pred)
(1166+373)/nrow(test)

# 2.1
letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio=0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(train$letter)

# P for base line
table(test$letter)
401/nrow(test)

# 2.2
letterTree = rpart(letter~.-isB, data = train, method = "class")
letterTree.pred = predict(letterTree, newdata = test, type = "class")
table(test$letter, letterTree.pred)

(348+318+363+340)/nrow(test)

# 2.3
set.seed(1000)
letterRF = randomForest(letter~.-isB, data = train)
letterRF.pred = predict(letterRF, newdata = test)
table(test$letter, letterRF.pred)

(390+380+393+364)/nrow(test)

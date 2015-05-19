#setwd('D:/workspace/The Analytics Edge/finalExam')
setwd('D:/doc/study/TheAnalyticsEdge/finalExam')

Sys.setlocale("LC_ALL", "C")
#  1
eBay = read.csv("eBay.csv", stringsAsFactors=FALSE)
table(eBay$sold)

799/nrow(eBay)

# 2
summary(eBay)

# 3
hist(eBay$size)
table(eBay$size)

# 4
eBay$sold = as.factor(eBay$sold)
eBay$condition  = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color  = as.factor(eBay$color)
eBay$material  = as.factor(eBay$material)

# 5
set.seed(144)

library(caTools)

spl = sample.split(eBay$sold, 0.7)
training = subset(eBay, spl==TRUE)
testing = subset(eBay, spl==FALSE)

# 6
glmFit = glm(sold~biddable+startprice+condition+heel+style+color+material, data = training, family = "binomial")
summary(glmFit)

# 7
ashoe = data.frame(biddable=0, startprice=100,
                   condition=factor("Pre-owned", levels(eBay$condition)), 
                   heel=factor("High", levels(eBay$heel)), 
                   style=factor("Open Toe",levels(eBay$style)),
                   color=factor("Black",levels(eBay$color)), 
                   material=factor("Satin",levels(eBay$material)))


pred = predict(glmFit, newdata=ashoe, type="response")

# 8
exp(0.8325406) - 1

# 9
pred = predict(glmFit, newdata=testing, type="response")

table(training$sold)  
# 0    1 
# 2098  559

table((pred > 0.5))

# 10
library("ROCR")
ROCRpredTest = prediction(pred, testing$sold)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# 11
ROCRperf = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

# 15

library(caret)
library(e1071)

set.seed(144)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:50)*0.001)
tr = train(sold~biddable+startprice+condition+heel+style+color+material, data = training, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# 16
library(rpart.plot)

prp(tr$finalModel)

# 17
library(tm)
corpus = Corpus(VectorSource(eBay$description))
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

dtm

# 18
spdtm = removeSparseTerms(dtm, 0.90)
spdtm

# 19
descriptionText = as.data.frame(as.matrix(spdtm))

sort(colSums(descriptionText))


# 20
names(descriptionText) = paste0("D", names(descriptionText))
descriptionText$sold = eBay$sold

descriptionText$biddable = eBay$biddable

descriptionText$startprice = eBay$startprice

descriptionText$condition = eBay$condition

descriptionText$heel = eBay$heel

descriptionText$style = eBay$style

descriptionText$color = eBay$color

descriptionText$material = eBay$material

trainText = subset(descriptionText, spl==TRUE)
testText = subset(descriptionText, spl==FALSE)

ncol(testText)

# 21
glmText = glm(sold~., data=trainText, family = "binomial")
summary(glmText)


# 22
library("ROCR")

pred.trainText = predict(glmText, type="response")

ROCRpred.trainText = prediction(pred.trainText, trainText$sold)
auc.train = as.numeric(performance(ROCRpred.trainText, "auc")@y.values)
auc.train

pred.testText = predict(glmText, newdata=testText, type="response")

ROCRpred.testText = prediction(pred.testText, testText$sold)
auc.test = as.numeric(performance(ROCRpred.testText, "auc")@y.values)
auc.test


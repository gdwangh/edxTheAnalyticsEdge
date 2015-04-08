setwd('D:/workspace/The Analytics Edge/unit5')
setwd('D:/doc/study/TheAnalyticsEdge/unit5')
# 1.1
emails = read.csv("emails.csv",stringsAsFactors=FALSE)
str(emails)

# 1.2
table(emails$spam)

# 1.3
emails$text[1]

# 1.5
max(nchar(emails$text))

# 1.6
min(nchar(emails$text))

# 2.1
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm

# 2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# 2.3
emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

# 2.4
emailsSparse$spam = emails$spam
colnames(emailsSparse)[colSums(emailsSparse[emailsSparse$spam==0, ])>=5000]

# 2.5
colnames(emailsSparse)[colSums(emailsSparse[emailsSparse$spam==1, ])>=1000]


# 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl==TRUE)
test = subset(emailsSparse, spl==FALSE)

spamLog = glm(spam~., data=train, family="binomial")
spamLog.pred = predict(spamLog, type="response")

library(rpart)
spamCART = rpart(spam~., data=train, method = "class")
spamCART.pred = predict(spamCART)

library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)
spamRF.pred = predict(spamRF, type="prob")


table(spamLog.pred<0.00001)  

table(spamLog.pred>0.99999) 

table(spamLog.pred>=0.00001 & spamLog.pred<=0.99999) 

# 3.2
summary(spamLog)

# 3.3
library(rpart.plot)
prp(spamCART)

# 3.4
table(train$spam, spamLog.pred>0.5)
(3052+954)/nrow(train)

# 3.5
library(ROCR)
spamLog.predROCR = prediction(spamLog.pred, train$spam)

auc = (performance(spamLog.predROCR, "auc"))@y.values
auc

# 3.6
table(train$spam, spamCART.pred[,2]>0.5)
(2885+894)/nrow(train)

# 3.7
spamCART.predROCR = prediction(spamCART.pred[,2], train$spam)

auc = (performance(spamCART.predROCR, "auc"))@y.values
auc

# 3.8
table(train$spam, spamRF.pred[,2]>0.5)
(3013+914)/nrow(train)

# 3.9
spamRF.predROCR = prediction(spamRF.pred[,2], train$spam)

auc = (performance(spamRF.predROCR, "auc"))@y.values
auc

# 4.1
spamLog.predTest = predict(spamLog, newdata=test, type="response")
table(test$spam, spamLog.predTest>0.5)
(1257+376)/nrow(test)

# 4.2
spamLog.predTestROCR = prediction(spamLog.predTest, test$spam)

auc = (performance(spamLog.predTestROCR, "auc"))@y.values
auc

# 4.3
spamCART.predTest = predict(spamCART, newdata=test)
table(test$spam, spamCART.predTest[,2]>0.5)
(1228+386)/nrow(test)

# 4.4
spamCART.predTestROCR = prediction(spamCART.predTest[,2], test$spam)

auc = (performance(spamCART.predTestROCR, "auc"))@y.values
auc

# 4.5
spamRF.predTest = predict(spamRF, newdata=test, type="prob")
table(test$spam, spamRF.predTest[,2]>0.5)
(1290+385)/nrow(test)

# 4.6
spamRF.predTestROCR = prediction(spamRF.predTest[,2], test$spam)

auc = (performance(spamRF.predTestROCR, "auc"))@y.values
auc

# 6.1
wordCount = rowSums(as.matrix(dtm))

# 6.2
hist(wordCount)

# 6.3
hist(log(wordCount))

# 6.4
emailsSparse$logWordCount = log(wordCount)
boxplot(logWordCount~spam, data=emailsSparse)

# 6.5
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl==FALSE)

spam2CART = rpart(spam~., data=train2, method="class")

set.seed(123)
spam2RF = randomForest(spam~., data=train2)

prp(spam2CART)

# 6.6
spam2CART.pred = predict(spam2CART, newdata=test2)
table(test2$spam, spam2CART.pred[,2]>0.5)
(1214+384)/nrow(test2)

# 6.7
spam2CART.predROCR = prediction(spam2CART.pred[,2], test2$spam)

auc = (performance(spam2CART.predROCR, "auc"))@y.values
auc

# 6.8
spam2RF.pred = predict(spam2RF, newdata=test2, type="prob")
table(test2$spam, spam2RF.pred[,2]>0.5)
(1298+382)/nrow(test2)

# 6.9
spam2RF.predROCR = prediction(spam2RF.pred[,2], test2$spam)

auc = (performance(spam2RF.predROCR, "auc"))@y.values
auc

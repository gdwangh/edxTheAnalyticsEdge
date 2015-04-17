setwd('D:/workspace/The Analytics Edge/unit5')

# 1.1
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE,encoding="latin1") 
str(trials)

trials[[1]]
trials$abstract

max(nchar(trials$abstract))
summary(nchar(trials$abstract)) 

# 1.2
sum(nchar(trials$abstract)==0)
table(nchar(trials$abstract) == 0)

# 1.3
which.min(nchar(trials$title))
trials[1258, "title"]

# 1.4
library(tm)

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

ncol(dtmTitle)
ncol(dtmAbstract)

#2.3
which.max(colSums(dtmAbstract))


# 3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# 3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trials = trials$trial

# 3.3
library(caTools)
set.seed(144)
spl = sample.split(dtm$trials, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trials)
730/nrow(train)

# 3.4
library(rpart)
library(rpart.plot)

trialCART = rpart(trials~., data=train, method = "class")
prp(trialCART)

# 3.5
trialCART.pred = predict(trialCART)
max(trialCART.pred[,2])

## 3.7
table(train$trials, trialCART.pred[,2]>=0.5)

(631+441)/nrow(train)
441/(131+441)
631/(631+99)

# 4.1
predTest = predict(trialCART, newdata=test)
table(test$trials, predTest[,2]>=0.5)
(261+162)/nrow(test)


# 4.2
library(ROCR)
predROCR = prediction(predTest[,2], test$trials)

auc = (performance(predROCR, "auc"))@y.values
auc

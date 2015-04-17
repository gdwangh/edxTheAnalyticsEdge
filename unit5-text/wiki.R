setwd('D:/workspace/The Analytics Edge/unit5')

# 模型总结：正文单词 + 是否有http地址 + 增加单数数 + 删除单词数
#   + 用户修改是是否登录+ 用户是否标记小修改

# read file
wiki = read.csv("wiki.csv",stringsAsFactors=FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)

# 1.1
table(wiki$Vandal)

# 1.2 preprocess data
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, PlainTextDocument)

corpusAdded
corpusAdded[[1]]

# Remove stopwords
length(stopwords("english"))

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded[[1]]

# Stem document 

corpusAdded = tm_map(corpusAdded, stemDocument)

corpusAdded[[1]]

# Build the DocumentTermMatrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# 1.3
# Remove sparse terms
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# 1.4 
# Convert to a data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
wordsAdded

# Make all variable names R-friendly
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# handle removed words with all of upper steps
# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

corpusRemoved
corpusRemoved[[2]]

# Remove stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved[[2]]

# Stem document 

corpusRemoved = tm_map(corpusRemoved, stemDocument)

corpusRemoved[[2]]

# Build the DocumentTermMatrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

# Remove sparse terms
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Convert to a data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

# Make all variable names R-friendly
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
wordsRemoved

# 1.5
wikiWords = cbind(wordsAdded, wordsRemoved) 
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
wikiWordsTrain = subset(wikiWords, spl==TRUE)
wikiWordsTest = subset(wikiWords, spl==FALSE)

table(wikiWordsTrain$Vandal)  # 0 for baseline
table(wikiWordsTest$Vandal)  
618/(618+545)

# 1.6
library(rpart)
library(rpart.plot)
wikiWordsTree = rpart(Vandal~., data = wikiWordsTrain)
wikiWordsTree.pred = predict(wikiWordsTree, newdata=wikiWordsTest)

pred.prob = wikiWordsTree.pred[,2]
table(wikiWordsTest$Vandal, pred.prob>=0.5)

(618+12)/nrow(wikiWordsTest)

# 1.7
prp(wikiWordsTree)

# 1.8
# ROC curve
library(ROCR)
predROCR = prediction(pred.prob, wikiWordsTest$Vandal)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# 2.1
wikiWords2 = wikiWords 
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# 2.2
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiWords2Tree = rpart(Vandal~., data = wikiTrain2)
wikiWords2Tree.pred = predict(wikiWords2Tree, newdata=wikiTest2)

pred2.prob = wikiWords2Tree.pred[,2]
table(wikiTest2$Vandal, pred2.prob>=0.5)
(609+57)/nrow(wikiTest2)

# 2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

# 2.4
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiWords2Tree2 = rpart(Vandal~., data = wikiTrain2)
wikiWords2Tree2.pred = predict(wikiWords2Tree2, newdata=wikiTest2)

pred2.2.prob = wikiWords2Tree2.pred[,2]
table(wikiTest2$Vandal, pred2.2.prob>=0.5)
(514+248)/nrow(wikiTest2)

# 3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)

wikiWords3Tree = rpart(Vandal~., data = wikiTrain3)
wikiWords3Tree.pred = predict(wikiWords3Tree, newdata=wikiTest3)

pred3.prob = wikiWords3Tree.pred[,2]
table(wikiTest3$Vandal, pred3.prob>=0.5)
(595+241)/nrow(wikiTest3)

# 3.2
prp(wikiWords3Tree)

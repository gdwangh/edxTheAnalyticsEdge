# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
setwd( "C:\\Neeraj\\backup\\tech\\coursera\\AnalyticEdge\\kaggle")

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

####
library(tm) # Load tm package
corpus = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))  # Create corpus
corpus = tm_map(corpus, tolower) # Pre-process data
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm1 = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm1, 0.99)  # create sparse terms
combo = as.data.frame(as.matrix(dtm)) # Create data frame
colnames(combo) = make.names(colnames(combo)) # Let's make sure our variable names are okay for R:
colnames(combo) <- paste0("H", colnames(combo))

#####
comboCol <- rbind(NewsTrain[,c(1,2,3,4,6,7,8)], NewsTest[,c(1,2,3,4,6,7,8)])

comboCol$SectionName[which(NewsTrain$SectionName=="Sports")] <- ""
combo$WordCount = comboCol$WordCount
combo$SubsectionName <- as.factor(comboCol$SubsectionName)
combo$SectionName <- as.factor(comboCol$SectionName)
combo$NewsDesk <- as.factor(comboCol$NewsDesk)
combo$comment <- as.factor(grepl("Open for Comments", comboCol$Headline, ignore.case=T))
combo$nocomment <- as.factor(grepl("No Comment Necessary", comboCol$Headline, ignore.case=T))
combo$york <- as.factor(grepl("new york", comboCol$Headline, ignore.case=T))
combo$obama <- as.factor(grepl("obama", comboCol$Headline, ignore.case=T))
combo$ebola <- as.factor(grepl("ebola", comboCol$Headline, ignore.case=T))
combo$qa  <- grepl("does |is |has |how |what |why |who |where |when |which |\\?", comboCol$Headline, ignore.case=T)
combo$Aqa  <- grepl("does |is |has |how |what |why |who |where |when |which |\\?", comboCol$Abstract, ignore.case=T)

# To convert the date/time to something R will understand, you can use the following commands:
comboCol$PubDate = strptime(comboCol$PubDate, "%Y-%m-%d %H:%M:%S")
combo$Weekday = as.factor(comboCol$PubDate$wday)
combo$Hour = as.factor(comboCol$PubDate$hour)

Train = head(combo, nrow(NewsTrain))
Train$Popular = as.factor(ifelse( NewsTrain$Popular==1, 'yes', 'no'))
Test = tail(combo, nrow(NewsTest))

################### training/testing
library(caTools)
library(caret)
library(ROCR)

set.seed(144)

## train part of Train
###   interaction.depth=c(5,9), n.trees = (1:3)*500, shrinkage = c(0.05,0.01))
####  n.trees = 1500, interaction.depth =9 and shrinkage = 0.01
###  interaction.depth = c(7, 11,15), n.trees = c(1500,2000), shrinkage = c(0.001,0.01))
#### n.trees = 1500, interaction.depth = 7 and shrinkage = 0.01
### interaction.depth = c(9, 11), n.trees = c(1700, 2000, 2300), shrinkage = c(0.002, 0.005, 0.006))
#### n.trees = 2300, interaction.depth = 11 and shrinkage = 0.005
gbmGrid8 <-  expand.grid(interaction.depth = c(11, 13), n.trees = c(2300, 2600), shrinkage = c(0.005))
nf  <- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)
gbmtr <- train(as.factor(Popular) ~. ,data = Train, method = "gbm",trControl = nf, tuneGrid=gbmGrid, metric ="ROC",verbose = T)
varImp(gbmtr, scale=F)
gbmtr
preds <- predict(object=gbmtr, Train, type="prob")[,2]
performance(prediction(preds, Train$Popular), "auc")@y.values

PredTest <- predict(object=gbmtr, Test, type="prob")[,2]

# Now we can prepare our submission file for Kaggle:
logfit = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(logfit, "gbm-006.csv", row.names=FALSE)

library(tm)
library(caret)
library(caretEnsemble)
library(doParallel)
library(randomForest)
library(gbm)
library(kernlab)
library(timeDate)
library(chron)
library(ROCR)
setwd("/home/branden/Documents/edxAnalyticsEdge/competition")
newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
# set.seed(99)
# split <- createDataPartition(newsTrain$Popular, p=0.8, list=FALSE)
# newsTest <- newsTrain[-split,]
# newsTrain <- newsTrain[split,]


## PREPROCESSING
# Combine Train and Test sets to reduce preprocessing lines, then split again
all <- rbind(newsTrain[,!(colnames(newsTrain) %in% "Popular")], newsTest)
#all <- rbind(newsTrain, newsTest)
#Convert characters to date variables
all$PubDate = strptime(all$PubDate, "%Y-%m-%d %H:%M:%S")

#Create a weekday variable
all$Weekday = as.factor(all$PubDate$wday)

# #Create hour variable
all$Hour <- as.factor(all$PubDate$hour)
# #Bin the hours
# all$Hour <- all$PubDate$hour
# all$Hour <- as.factor(cut(all$Hour, 8, labels=FALSE))

# Convert WordCount to log(WordCount)
all$WordCount <- log(all$WordCount + 1)
#all$WordCount <- as.factor(cut(log(all$WordCount + 1),8, labels=FALSE))

#all$Popular <- as.factor(ifelse(all$Popular==1,"Yes","No"))
all$NewsDesk <- as.factor(all$NewsDesk)
all$SectionName <- as.factor(all$SectionName)
all$SubsectionName <- as.factor(all$SubsectionName)

##### BAG OF WORDS
CorpusHeadline = Corpus(VectorSource(c(all$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!
dtm = DocumentTermMatrix(CorpusHeadline, control=list(weighting=function(x) weightTfIdf(x, normalize=FALSE)))
sparse = removeSparseTerms(dtm, 0.988)
HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) <- paste("H", colnames(HeadlineWords), sep = "_")

#Snippet and Abstract seem to be correlated, remvoing Snippet for now.
all <- cbind(all, HeadlineWords)

#####

# Convert to lower case for grep
all$Headline <- tolower(all$Headline)
all$Snippet <- tolower(all$Snippet)
all$Abstract <- tolower(all$Abstract)

# Is the headline a question?
questionWords <- c("\\?","^why","should","^when","can","^if","^is")
all$Question <- as.factor(ifelse(grepl(paste(questionWords, collapse="|"), all$Headline)==TRUE,"Yes","No"))

whatReadWatch <- c("what we're")
all$whatReadWatch <- as.factor(ifelse(grepl(paste(whatReadWatch, collapse="|"), all$Headline)==TRUE,"Yes","No"))
all$beginYear <- as.factor(ifelse(grepl("^[0-9][0-9][0-9][0-9]", all$Headline)==TRUE, "Yes","No"))
all$QsAboutNews <- as.factor(ifelse(grepl("6 q's about the news", all$Headline)==TRUE, "Yes","No"))
all$askWell <- as.factor(ifelse(grepl("ask well", all$Headline)==TRUE, "Yes","No"))
all$dailyClip <- as.factor(ifelse(grepl("daily clip report", all$Headline)==TRUE, "Yes","No"))
all$dailyReport <- as.factor(ifelse(grepl("daily report", all$Headline)==TRUE, "Yes","No"))
all$dont <- as.factor(ifelse(grepl("don't", all$Headline)==TRUE,"Yes","No"))
all$firstDraft <- as.factor(ifelse(grepl("first draft", all$Headline)==TRUE, "Yes","No"))
all$mornAgenda <- as.factor(ifelse(grepl("morning agenda", all$Headline)==TRUE, "Yes","No"))
all$playlist <- as.factor(ifelse(grepl("playlist", all$Headline)==TRUE, "Yes","No"))
all$podcast <- as.factor(ifelse(grepl("podcast", all$Headline)==TRUE,"Yes","No"))
all$readRespond <- as.factor(ifelse(grepl("readers respond", all$Headline)==TRUE, "Yes","No"))
all$testYourself <- as.factor(ifelse(grepl("test yourself", all$Headline)==TRUE, "Yes","No"))
all$theDailyGift <- as.factor(ifelse(grepl("the daily gift", all$Headline)==TRUE, "Yes","No"))
all$todayIn <- as.factor(ifelse(grepl("today in", all$Headline)==TRUE, "Yes","No"))
all$verbatim <- as.factor(ifelse(grepl("verbatim", all$Headline)==TRUE, "Yes","No"))
all$wordOfDay <- as.factor(ifelse(grepl("word of the day", all$Headline)==TRUE, "Yes","No"))
all$recap <- as.factor(ifelse(grepl("recap", all$Headline)==TRUE, "Yes","No"))

brit <- c("britain", "british","england")
all$brit <- as.factor(ifelse(grepl(paste(brit, collapse="|"), all$Headline)==TRUE,"Yes","No"))

how <- c("\\<how\\>")
all$how <- as.factor(ifelse(grepl(paste(how, collapse="|"), all$Headline)==TRUE,"Yes","No"))

where <- c("\\<where\\>")
all$where <- as.factor(ifelse(grepl(paste(where, collapse="|"), all$Headline)==TRUE,"Yes","No"))

mymei <- c("my","me","i")
all$mymei <- as.factor(ifelse(grepl(paste(mymei, collapse="|"), all$Headline)==TRUE,"Yes","No"))


# all$Question <- grepl(paste(questionWords, collapse="|"), all$Headline)
# newsTest$Question <- grepl(paste(questionWords, collapse="|"), newsTest$Headline)

# Words related to religion -- people usually comment a lot on these types of articles
religionWords <- c("secular","humanist","humanism","secularist","god","religion","atheist","atheism","islam","islamic","islamist",
              "islamists","church","atheists","jesus","christ","christian","catholic","pope","imam", "\\<isis\\>","muslim","gay","marriage","israel","jewish","extremist",
              "fundamentalism","terror","terrorist","terrorism")
all$religionWords <- ifelse(grepl(paste(religionWords, collapse="|"), all$Headline)==TRUE,1,0)

techWords <- c("apple","\\<ios\\>","ipod", "ipad","iphone","amazon","facebook")
all$tech <- ifelse(grepl(paste(techWords, collapse="|"), all$Headline)==TRUE,1,0)

techUnpop <- c("twitter","google")
all$techUnpop <- ifelse(grepl(paste(techUnpop, collapse="|"), all$Headline)==TRUE,1,0)

healthWords <- c("cancer","weight","fat","heart","disease","brain","sex","sexual","love","hate","doctor","doctors","medical","medicine","hospital","hospitals")
all$health <- ifelse(grepl(paste(healthWords, collapse="|"), all$Headline)==TRUE,1,0)

sciWords <- c("climate","warming","global","science","scientists")
all$sci <- ifelse(grepl(paste(sciWords, collapse="|"), all$Headline)==TRUE,1,0)

busWords <- c("jobs","employment","work","working","economy")
all$business <- ifelse(grepl(paste(busWords, collapse="|"), all$Headline)==TRUE,1,0)

# #Already in Bag of Words
# all$york <- ifelse(grepl("york", all$Headline)==TRUE,1,0)

# #Already in Bag of Words
#all$ebola <- ifelse(grepl("ebola", all$Headline)==TRUE,1,0)

# #Already in Bag of Words
#all$today <- ifelse(grepl("today", all$Headline)==TRUE,1,0)
# #Already in Bag of Words
#all$fashion <- ifelse(grepl("fashion", all$Headline)==TRUE,1,0)
# Already in Bag of Words
#all$report <- ifelse(grepl("report", all$Headline)==TRUE,1,0)

# political words correlated w/ high popularity
poliHiWords <- c("republican", "conservative")
all$poliHi <- ifelse(grepl(paste(poliHiWords, collapse="|"), all$Headline)==TRUE,1,0)

poliLoWords <- c("politics")
all$poliLo <- ifelse(grepl(paste(poliLoWords, collapse="|"), all$Headline),1,0)

all$noComment <- ifelse(grepl("no comment necessary", all$Headline),1,0)
all$comments <- ifelse(grepl("open for comments", all$Headline),1,0)

#Headline WordCount
all$headWordCount <- sapply(gregexpr("\\W+", all$Headline), length)
all$headWordCount <- as.factor(ifelse(all$headWordCount>3,"3+", all$headWordCount))

levels(all$NewsDesk) <- c(levels(all$NewsDesk), c("Multimedia"))
levels(all$SubsectionName) <- c(levels(all$SubsectionName), c("Multimedia"))
all$NewsDesk[all$SectionName=="Opinion"] <- "OpEd" 
all$NewsDesk[all$SectionName=="Multimedia"] <- "Multimedia"
all$SubsectionName[all$SectionName=="Multimedia"] <- "Multimedia"
all$SubsectionName[all$NewsDesk=="Styles"] <- "Fashion & Style"
all$SectionName[all$Subsection=="Fashion & Style"] <- "Style"
all$SectionName[all$NewsDesk=="Foreign"] <- "World"

# Remove unneccessary columns
all$Headline <- NULL
all$Snippet <- NULL
all$Abstract <- NULL
all$PubDate <- NULL

# Resplit the dataset
train <- head(all, nrow(newsTrain))
train$Popular <- as.factor(ifelse(newsTrain$Popular==1,"Yes","No"))
train$UniqueID <- NULL
test <- tail(all, nrow(newsTest))


## ENSEMBLE
ensCtrl <- trainControl(method="cv",
                        number=5,
                        savePredictions=TRUE,
                        allowParallel=TRUE,
                        classProbs=TRUE,
                        index=createResample(train$Popular, 25),
                        selectionFunction="best",
                        summaryFunction=fiveStats)
glmGrid <- expand.grid(alpha=c(.4), lambda=2^-8)
rfGrid <- expand.grid(mtry=c(17))
gbmGrid <- expand.grid(n.trees=c(3500), interaction.depth=c(27), shrinkage=c(.001))
svmGrid <- expand.grid(.sigma=c(.0007),.C=c(16,32))

cl <- makeCluster(7)
registerDoParallel(cl)
tme <- Sys.time()
model_list <- caretList(
  Popular ~ .,
  data=train,
  trControl=ensCtrl,
  metric="ROC",
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=rfGrid, nodesize=1, ntree=3000),
    glmnet=caretModelSpec(method="glmnet", tuneGrid=glmGrid, preProcess=c("center","scale")),
    gbm=caretModelSpec(method="gbm", tuneGrid=gbmGrid),
    svm=caretModelSpec(method="svmRadial", tuneGrid=svmGrid, preProcess=c("center","scale"))
  )
)
stopCluster(cl)
Sys.time() - tme

save(model_list, file="model_listFinal.rda")

xyplot(resamples(model_list))
modelCor(resamples(model_list))
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

library('caTools')
model_preds <- lapply(model_list, predict, newdata=test, type='prob')
model_preds <- lapply(model_preds, function(x) x[,'Yes'])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=test)
model_preds$ensemble <- ens_preds


# Ensemble Stack
cl <- makeCluster(7)
registerDoParallel(cl)
tme <- Sys.time()
gbm_stack <- caretStack(
  model_list, 
  method='gbm',
  verbose=FALSE,
  tuneGrid=expand.grid(n.trees=c(2500), interaction.depth=c(24), shrinkage=c(.001)),
  metric='ROC',
  trControl=trainControl(
    method='cv',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    allowParallel=TRUE,
    summaryFunction=twoClassSummary
  )
)
stopCluster(cl)
Sys.time() - tme
gbm_stack

model_preds2 <- model_preds
model_preds2$ensemble <- predict(gbm_stack, newdata=test, type='prob')$Yes
#CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
#colAUC(model_preds2, testing$Class)


## Creat Submission File
EnsSubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = model_preds2$ensemble)

write.csv(EnsSubmission, "SubmissionGBMStackFinal-rf-glmnet-gbm-svm.csv", row.names=FALSE)



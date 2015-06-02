# setwd("D:/workspace/TheAnalyticsEdge/kaggleCompetition")
setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")

newsTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newsTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

newsTrain$logWordCount = log(1+newsTrain$WordCount)
newsTest$logWordCount = log(1+newsTest$WordCount)

# get info from pubdate
newsTrain$PubDate = strptime(newsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
newsTest$PubDate = strptime(newsTest$PubDate, "%Y-%m-%d %H:%M:%S")

newsTrain$Weekday = newsTrain$PubDate$wday
newsTest$Weekday = newsTest$PubDate$wday

newsTrain$Hour = newsTrain$PubDate$hour
newsTest$Hour = newsTest$PubDate$hour

# newsTrain: mon=8~10: 9~11月
# 2014.9~11 holiday: 
# 9月1日 （9月第1个星期一 ） 劳工节;
# 9月11日 911袭击September 11 attacks
# 10月13日  哥伦布日Columbus Day
# 10月31日  万圣夜Halloween
# 11月1日  万圣节
# 11月11日 退伍军人节
# 11月27日 感恩节: 2014年11月27日(星期四)至2014年11月30日(星期日)放假4天
mon = newsTrain$PubDate$mon
dd = newsTrain$PubDate$mday
newsTrain$isHoliday = (mon == 8 & dd == 1) | (mon == 8 & dd == 11) | (mon==9 & dd==13 ) | 
  (mon==9 & dd == 31) | (mon == 10 & dd == 1) | (mon==10 & dd==11) |
  (mon==10 & dd == 27)

# newsTest: mon=11: 12月
# 12月24日 平安夜
mon = newsTest$PubDate$mon
dd = newsTest$PubDate$mday
newsTest$isHoliday = (mon==11 & dd == 24) | (mon==11 & dd == 25)



# # trans to factor
newsTrain$PopularFactor = as.factor(ifelse(newsTrain$Popular,"Yes", "No"))

newsTrain$NewsDeskFactor = as.factor(newsTrain$NewsDesk)
newsTest$NewsDeskFactor = factor(newsTest$NewsDesk, levels = levels(newsTrain$NewsDeskFactor))

newsTrain$SectionNameFactor = as.factor(newsTrain$SectionName)
newsTest$SectionNameFactor = factor(newsTest$SectionName, levels = levels(newsTrain$SectionNameFactor))


newsTrain$SubsectionNameFactor = as.factor(newsTrain$SubsectionName)
newsTest$SubsectionNameFactor = factor(newsTest$SubsectionName, levels = levels(newsTrain$SubsectionNameFactor))

# check question words or ? in the headline
# not significant
newsTrain$HeadlineIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Headline, ignore.case = TRUE) ))
newsTest$HeadlineIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Headline, ignore.case = TRUE)), levels = levels(newsTrain$HeadlineIsQuestion))

# table(newsTrain$HeadlineIsQuestion, newsTrain$Popular)
# tapply(newsTrain$Popular,newsTrain$HeadlineIsQuestion,mean)
# 
# newsTrain$AbstractIsQuestion = as.factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTrain$Abstract, ignore.case = TRUE) ))
# newsTest$AbstractIsQuestion = factor(as.numeric(grepl("[\\? | ^(How|Why|When|What|Where|Who|Should|Can|Is|Was) ]", newsTest$Abstract, ignore.case = TRUE)), levels = levels(newsTrain$AbstractIsQuestion))



#############################################################################
# text conpus
library(tm)
require(RWeka)
library(rJava)
library(wordcloud)

TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))  

cal_text_corpus = function(text, throld) {
  Corpus = Corpus(VectorSource(c(text)))
  
  # You can go through all of the standard pre-processing steps like we did in Unit 5:
  Corpus = tm_map(Corpus, tolower)
  Corpus = tm_map(Corpus, PlainTextDocument)
  Corpus = tm_map(Corpus, removePunctuation)
  Corpus = tm_map(Corpus, removeWords, stopwords("english"))
  Corpus = tm_map(Corpus, stemDocument)
  
  dtm <- DocumentTermMatrix(Corpus, control = list(tokenize = TrigramTokenizer, weighting=function(x) weightTfIdf(x, normalize=FALSE)))
  
  sparse = removeSparseTerms(dtm, throld)
  
  textWords = as.data.frame(as.matrix(sparse))
  
  #   wordcloud(colnames(textWords), colSums(textWords))
  #   sort(colSums(textWords))
  
  as.factor(ifelse( rowSums(textWords)==0, "No", "Yes"))
}

# popular words
HeadlineIsPop = cal_text_corpus(c(newsTrain$Headline, newsTest$Headline),0.985)
SnippetIsPop = cal_text_corpus(c(newsTrain$Snippet, newsTest$Snippet),0.965)
AbstractIsPop = cal_text_corpus(c(newsTrain$Abstract, newsTest$Abstract),0.965)

newsTrain$headlineIsPopWord = head(HeadlineIsPop, nrow(newsTrain))
newsTest$headlineIsPopWord = tail(HeadlineIsPop, nrow(newsTest))

# not significant
newsTrain$SnippetIsPop = head(SnippetIsPop, nrow(newsTrain))
newsTest$SnippetIsPop = tail(SnippetIsPop, nrow(newsTest))

newsTrain$AbstractIsPop = head(AbstractIsPop, nrow(newsTrain))
newsTest$AbstractIsPop = tail(AbstractIsPop, nrow(newsTest))

rm(HeadlineIsPop, SnippetIsPop, AbstractIsPop)

# emotion
library(qdap)
# pol<- polarity(paste(newsTrain$Headline, newsTrain$Snippet,".") )
pol<- polarity(newsTrain$Headline)
newsTrain$HSpolarity = pol[[1]]$polarity
newsTrain[is.na(newsTrain$HSpolarity), ]$HSpolarity = 0

pol<- polarity(newsTest$Headline)
newsTest$HSpolarity = pol[[1]]$polarity
newsTest[is.na(newsTest$HSpolarity), ]$HSpolarity = 0

pol<- polarity(newsTrain$Abstract)
newsTrain$AbstractPolarity = pol[[1]]$polarity
newsTrain[is.na(newsTrain$AbstractPolarity), ]$AbstractPolarity = 0

pol<- polarity(newsTest$Abstract)
newsTest$AbstractPolarity = pol[[1]]$polarity
newsTest[is.na(newsTest$AbstractPolarity), ]$AbstractPolarity = 0

# cal the count of every day
newsTrain$yday = newsTrain$PubDate$yday
newsTest$yday = newsTest$PubDate$yday

library(dplyr)
newsTrain_state<- tbl_df(newsTrain) %>% select(yday) %>% group_by(yday) %>% 
  summarise(count = n())

tmp = merge(newsTrain[,c("UniqueID","yday")], newsTrain_state[,c(1,2)], by="yday")
newsTrain$everydayCount = tmp$count

newsTest_state<- tbl_df(newsTest) %>% select(yday) %>% group_by(yday) %>% 
  summarise(count = n())
tmp = merge(newsTest[,c("UniqueID","yday")], newsTest_state, by="yday")
newsTest$everydayCount = tmp$count

rm(tmp)

## last - how much time has passed till publication of the last article
## the idea is that the bigger the number of articles published 
## the lesser the average popularity as attention is spread to more articles
library(zoo)
newsAll = rbind(newsTrain[,c("UniqueID","PubDate", "Weekday", "Hour")], 
                newsTest[,c("UniqueID","PubDate", "Weekday", "Hour")])
newsAll = newsAll[order(newsAll$PubDate),]

pd = as.POSIXlt( newsAll$PubDate )
z = zoo(as.numeric(pd))   # create time zoo list by pubdate
n = nrow(newsAll)
b = zoo(, seq(n))

# cal the time zoo interval units between a item and other item lag 1/3/5/10/20/50 element
newsAll$last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
newsAll$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
newsAll$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
newsAll$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
newsAll$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
newsAll$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))

# cal the average of last1/3/5/10/20/50
last.avg = newsAll[, c("Weekday", "Hour", "last1", "last3", "last5", "last10", "last20", "last50")] %>% 
  group_by(Weekday, Hour) %>% dplyr::summarise(
    last1.avg=mean(last1, na.rm=TRUE),
    last3.avg=mean(last3, na.rm=TRUE),
    last5.avg=mean(last5, na.rm=TRUE),
    last10.avg=mean(last10, na.rm=TRUE),
    last20.avg=mean(last20, na.rm=TRUE),
    last50.avg=mean(last50, na.rm=TRUE)
  )

# replace na with average 
na.merge = merge(newsAll, last.avg, by=c("Weekday","Hour"))
na.merge = na.merge[order(na.merge$UniqueID),]

for(col in c("last1", "last3", "last5", "last10", "last20", "last50")) {
  y = paste0(col, ".avg")
  idx = is.na(newsAll[[col]])   # get the column named col and check NA
  newsAll[idx,][[col]] <- na.merge[idx,][[y]]   # set the NA with average
}

newsTrain = merge(newsTrain, newsAll[,c("UniqueID","last1", "last3", "last5", "last10", "last20", "last50")], by="UniqueID")
newsTest = merge(newsTest, newsAll[,c("UniqueID","last1", "last3", "last5", "last10", "last20", "last50")], by="UniqueID")

cor(newsTrain[,c("Popular","last1", "last3", "last5", "last10", "last20", "last50")])

rm(last.avg, na.merge, b, i, idx, n, pd, y, z)

## ENSEMBLE

# rf
library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)

library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

# rfGrid<- expand.grid(mtry=c(1:20))
# 
# set.seed(1000)
# rfFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
#                 logWordCount+Weekday+Hour+headlineIsPopWord+
#                 AbstractPolarity+HSpolarity+everydayCount+isHoliday+
#                 last1+last3+last5+last10+last20+last50,
#               data = newsTrain,
#               method="rf",
#               importance=TRUE,
#               trControl=ensCtrl,
#               tuneGrid=rfGrid,
#               metric="ROC")

rfGrid<- expand.grid(mtry=c(9))

stopCluster(cl)

# # glmnet
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

library(glmnet)
x = newsTrain[,c("NewsDeskFactor","SectionNameFactor","SubsectionNameFactor","logWordCount",
                  "Weekday","Hour","headlineIsPopWord","AbstractPolarity","HSpolarity",
                  "everydayCount","isHoliday","last1","last3","last5","last10","last20","last50")]
# set.seed(1000)
# 
# gla1 <- cv.glmnet(data.matrix(x), y=newsTrain$PopularFactor, nfolds = 10, alpha=1, type.measure="auc", family="binomial")
# gla1$lambda.1se   # 0.02580755
# gla1$lambda.min   # 0.00119788
# 
# pred1 = predict(gla1$glmnet.fit, newx=data.matrix(x), s = gla1$lambda.min)
# 
# gla0 <- cv.glmnet(data.matrix(x), y=newsTrain$PopularFactor, nfolds = 10, alpha=0, type.measure="auc", family="binomial")
# gla0$lambda.1se  # 0.1170341
# gla0$lambda.min  # 0.01041855
# pred0 = predict(gla0$glmnet.fit, newx=data.matrix(x), s = gla0$lambda.min)
# 
# library("ROCR")
# ROCR.Pred1 = prediction(pred1, newsTrain$PopularFactor)
# auc1 = as.numeric(performance(ROCR.Pred1, "auc")@y.values)
# auc1  # 0.8392424
# 
# ROCR.Pred0 = prediction(pred0, newsTrain$PopularFactor)
# auc0 = as.numeric(performance(ROCR.Pred0, "auc")@y.values)
# auc0  # 0.8381913
# 
# 0.0008256524
# glmGrid<- expand.grid(alpha=seq(0, 1, by=0.1), lambda=c(1:100)*0.001)  # 0.001~0.1
# 
# set.seed(1000)
# glmnetFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
#                                     logWordCount+Weekday+Hour+headlineIsPopWord+
#                                     AbstractPolarity+HSpolarity+everydayCount+isHoliday+
#                                     last1+last3+last5+last10+last20+last50,
#                   data = newsTrain,
#                   method="glmnet", 
#                   trControl=ensCtrl,
#                   tuneGrid=glmGrid,
#                   metric="ROC")

# alpha=seq(0, 1, by=0.1), lambda=c(1:100)*0.001)=0.001~0.1: alpha = 1 and lambda = 0.003

glmGrid<- expand.grid(alpha=c(1), lambda=0.003)

stopCluster(cl)


# gbm
modelLookup("gbm")

library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

# gbmGrid<- expand.grid(n.trees=c(10:30)*100, interaction.depth=c(10:20), shrinkage=c(0.005),n.minobsinnode = c(10))

# ptm <- proc.time();ptm
# 
# set.seed(1000)
# gbmFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
#                              logWordCount+Weekday+Hour+headlineIsPopWord+
#                              AbstractPolarity+HSpolarity+everydayCount+isHoliday+
#                              last1+last3+last5+last10+last20+last50,
#                data = newsTrain,
#                method="gbm", 
#                trControl=ensCtrl,
#                tuneGrid=gbmGrid,
#                metric="ROC")

proc.time()-ptm
用户     系统     流逝 
366.17    14.36 69955.69 --- 19hours

用户     系统     流逝 
483.44    16.96 90940.41 --- 25.5

用户     系统     流逝 
222.17     1.67 40409.19 --- 11.2hour


gbmGrid<- expand.grid(n.trees=c(1800), interaction.depth=c(19), shrinkage=c(.005),n.minobsinnode = 10)

stopCluster(cl)


pred.gbm = predict(gbmFit, type="prob")
library("ROCR")
ROCR.Pred.gbm = prediction( newsTrain$Popular, pred.gbm[,2]>0.5)
auc1 = as.numeric(performance(ROCR.Pred.gbm, "auc")@y.values)
auc1  # 0.9163987


# svm
modelLookup("svmRadial")

library(e1071)
library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

# svmGrid<- expand.grid(.sigma=c(.0007),.C=c(16,32))
svmGrid<- expand.grid(.sigma=c(1:10)*.0001,.C=c(1:100)*0.1) #  sigma = 2e-04 and C = 9.2

set.seed(1000)
svmFit = train(PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
                             logWordCount+Weekday+Hour+headlineIsPopWord+
                             AbstractPolarity+HSpolarity+everydayCount+isHoliday+
                             last1+last3+last5+last10+last20+last50,
               data = newsTrain,
                method="svmRadial", 
                trControl=ensCtrl,
                tuneGrid=svmGrid,
                 metric="ROC")

svmGrid<- expand.grid(.sigma=c(0.0002),.C=c(9.2))

library(caretEnsemble)
set.seed(1000)
model_list<- caretList(
  PopularFactor~NewsDeskFactor+SectionNameFactor+SubsectionNameFactor+
    logWordCount+Weekday+Hour+headlineIsPopWord+
    AbstractPolarity+HSpolarity+everydayCount+isHoliday+
    last1+last3+last5+last10+last20+last50,
  data=newsTrain,
  trControl=ensCtrl,
  metric="ROC",
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=rfGrid, nodesize=1, ntree=3000),
    glmnet=caretModelSpec(method="glmnet",tuneGrid=glmGrid, preProcess=c("center","scale")),
    gbm=caretModelSpec(method="gbm", tuneGrid=gbmGrid),
    svm=caretModelSpec(method="svmRadial",tuneGrid=svmGrid, preProcess=c("center","scale"))
  )
)

stopCluster(cl)

greedy_ensemble<- caretEnsemble(model_list)

library('caTools')
model_preds<- lapply(model_list, predict, newdata=newsTrain, type='prob')
model_preds<- lapply(model_preds, function(x) x[,'Yes'])
model_preds<- data.frame(model_preds)

ens_preds<- predict(greedy_ensemble, newdata=newsTrain)
model_preds$ensemble<- ens_preds

colAUC(model_preds, newsTrain$PopularFactor)


library("ROCR")
ROCR.Pred = prediction( newsTrain$Popular, ens_preds>0.5)
auc = as.numeric(performance(ROCR.Pred, "auc")@y.values)
auc  # 0.918668

pred.test = predict(greedy_ensemble, newdata=newsTest)
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test)
write.csv(MySubmission, "post-study/caretEnsemble.csv", row.names=FALSE)

# other method
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

gbm_stack<- caretStack(
  model_list,
  method='gbm',
  verbose=FALSE,
  # tuneGrid=expand.grid(n.trees=c(2500), interaction.depth=c(24), shrinkage=c(.001),n.minobsinnode=c(10)),
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

model_preds2 <- model_preds
model_preds2$ensemble <- predict(gbm_stack, newdata=newsTrain, type='prob')$Yes
colAUC(model_preds2, newsTrain$PopularFactor)

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, model_preds2$ensemble>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.964333


pred.test = predict(gbm_stack, newdata=newsTest, type='prob')$Yes
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test)
write.csv(MySubmission, "post-study/ensembleModel_gbm.csv", row.names=FALSE)

setwd("D:/workspace/The Analytics Edge/kaggleCompetition")
# setwd("D:/doc/study/TheAnalyticsEdge/kaggleCompetition")


#read data
train <- read.csv('NYTimesBlogTrain.csv',stringsAsFactors=F)
test <- read.csv('NYTimesBlogTest.csv',stringsAsFactors=F)

#load libraries
library(tm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(chron)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(randomForest)
library(kknn)
library(glmnet)
library(gbm)
library(caretEnsemble)
# library(doMC)  # library for parrel which is not available for windows
library(tau)
library(e1071)

#I am going to combine the data to do the processing together, then I can split that
#it seems like duplicate qwork to do it at once and besides when I select the less sparse data who knows who I will do that

train$group = 1
test$group= 0 
data <- bind_rows(train,test)
table(data$group)

#let's do some basic processing
data$date = as.Date(data$PubDate,'%Y-%m-%d ')

# wanghui: run error so change it 
# data$time = sapply(strsplit(data$PubDate,' '), function(x) times(x[2]))
data$time = strptime(data$PubDate, "%Y-%m-%d %H:%M:%S")

data$weekday = weekdays(data$date)
data$hour = as.character((hours(data$time)))

#I need to create a corpus for the headline, for the snippet and for the abstract
corpus_hl <- Corpus(VectorSource(data$Headline))
corpus_hl <- tm_map(corpus_hl,tolower)
corpus_hl <- tm_map(corpus_hl,PlainTextDocument)
corpus_hl <- tm_map(corpus_hl,removePunctuation)
corpus_hl <- tm_map(corpus_hl,removeWords, stopwords("english"))
corpus_hl <- tm_map(corpus_hl,stemDocument)
dtm_hl <- DocumentTermMatrix(corpus_hl)
dtm_hl_dense = removeSparseTerms(dtm_hl, 0.995)
headline_words <- as.data.frame(as.matrix(dtm_hl_dense))
names(headline_words) <- paste0('head_',names(headline_words))

corpus_sn <- Corpus(VectorSource(data$Snippet))
corpus_sn <- tm_map(corpus_sn,tolower)
corpus_sn <- tm_map(corpus_sn,PlainTextDocument)
corpus_sn <- tm_map(corpus_sn,removePunctuation)
corpus_sn <- tm_map(corpus_sn,removeWords, stopwords("english"))
corpus_sn <- tm_map(corpus_sn,stemDocument)
dtm_sn <- DocumentTermMatrix(corpus_sn)
dtm_sn_dense = removeSparseTerms(dtm_sn, 0.99)
snippet_words <- as.data.frame(as.matrix(dtm_sn_dense))
names(snippet_words) <- paste0('snip_',names(snippet_words))


corpus_abs <- Corpus(VectorSource(data$Abstract))
corpus_abs <- tm_map(corpus_abs,tolower)
corpus_abs <- tm_map(corpus_abs,PlainTextDocument)
corpus_abs <- tm_map(corpus_abs,removePunctuation)
corpus_abs <- tm_map(corpus_abs,removeWords, stopwords("english"))
corpus_abs <- tm_map(corpus_abs,stemDocument)
dtm_abs <- DocumentTermMatrix(corpus_abs)
dtm_abs_dense = removeSparseTerms(dtm_abs, 0.99)
abstract_words <- as.data.frame(as.matrix(dtm_abs_dense))
names(abstract_words) <- paste0('abs_',names(abstract_words))

#merge back to create a modeling set
modeling <- cbind(data,headline_words,snippet_words,abstract_words)
row.names(modeling) <- NULL
modeling$NewsDesk <- factor(modeling$NewsDesk)
modeling$SectionName <- factor(modeling$SectionName)
modeling$SubsectionName <- factor(modeling$SubsectionName)
modeling$weekday <- factor(modeling$weekday)

#for some reason, caret does some preprocessing to the factors and messes them up for prediction, the best way is to vreate my own categorical variables for them
#on modleing data and be done with it
#for that I will use dplyr

extra <- modeling %>% select(NewsDesk,SectionName,SubsectionName,weekday,UniqueID) %>% gather(var,level,-UniqueID) %>% 
  mutate(level=ifelse(level=="",'blank',level),n=1) %>%  unite(name,var,level,sep="_") %>% spread(name,n,fill=0)

modeling2 <- inner_join(modeling,extra)
modeling2 <- modeling2 %>% select(-c(NewsDesk,SectionName,SubsectionName,weekday))
which(sapply(modeling2,function(x) sum(is.na(x))>0))
names(modeling2) <- make.names(names(modeling2))
names(modeling2) <- gsub("\\.+","",names(modeling2))
modeling2$hour <- as.numeric(modeling2$hour)

#split into train and test
train_new <- subset(modeling2,group==1,-group)
train_new$Popular <- as.factor(train_new$Popular)
test_new <- subset(modeling2,group==0,-c(group,Popular))
which(sapply(test_new,function(x) sum(is.na(x))>0))
which(sapply(train_new,function(x) sum(is.na(x))>0))

#############################################
#Models
# Now that I have somedata, lest establish a a baseline, guessnot
prop.table(table(train_new$Popular))
confusionMatrix(factor(rep(0,dim(train_new)[1]),c(0,1)),factor(train_new$Popular,c(0,1)),positive='1')
#83.27%

drop1 = which((names(train_new) %in% c('PubDate','date','time','UniqueID','Headline','Snippet','Abstract')))
#start with a logistic

model1 <- glm(Popular~. ,train_new[,-drop1],family='binomial')
pred1 <- predict(model1,newdata=test_new,type='response')
write.csv(data.frame(UniqueID=test_new$UniqueID,Probability1=pred1),'submission1.csv',row.names=F)
confusionMatrix(factor(as.numeric(predict(model1,type='response')>=0.5,c(0,1))),
                factor(train_new$Popular,c(0,1)),positive='1')


#this was not so good, but it was just a test of how well I can do, mostly test the sumission
#it will take forever to clean the variable by nae o n logistic

#try an rpart cv on caret
#from a Kuhn presentation I found, he explain how to tune ROC, whihc is how we are being measured


#summaryFunction = twoClassSummary),metric="ROC"
train2 = train_new[,setdiff(1:dim(train_new)[2],drop1)]


train2$Popular = factor(train2$Popular,c(0,1),labels=c('N',"Y"))
cvctrl <- trainControl(method = "cv", number = 10,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

cpgrid = expand.grid(.cp=seq(0.01,.1,by=0.01))
set.seed(1)
model2 <- train(Popular ~ ., data = train2, method = "rpart",preProcess=NULL,
                metric = "ROC",
                trControl = cvctrl,tuneGrid=cpgrid)
model2
model2$finalModel
fancyRpartPlot(model2$finalModel)
confusionMatrix(factor(as.numeric(predict(model2$finalModel)[,2]>=0.5),c(0,1),labels=c('N',"Y")),
                train2$Popular,positive='Y')

pred2 <- predict(model2$finalModel,newdata=test_new)
write.csv(data.frame(UniqueID=test_new$UniqueID,Probability1=pred2[,2]),'submission2.csv',row.names=F)



#Random Forest model
model3 <- randomForest(Popular~.,data=train2)
model3
pred3 <- predict(model3,newdata=test_new,type='prob')
confusionMatrix(predict(model3),
                train2$Popular,positive='Y')
write.csv(data.frame(UniqueID=test_new$UniqueID,Probability1=pred3[,2]),'submission3.csv',row.names=F)

#the RF was like number 19 so far, this makes me feel good

#I will calculate n-grams for 2 and 3 workds from the headline and the snippet, 
#I think those should be enough, but I may revisit that later
#I worked that codee on the hw for chapetr 5
library(tau)
#1. extract the processed text from teh corpora
snippets <- data.frame(text=sapply(1:dim(data)[1],function (x) unlist(corpus_sn[[x]][[1]])),
                       stringsAsFactors=F)
headlines <- data.frame(text=sapply(1:dim(data)[1],function (x) unlist(corpus_hl[[x]][[1]])),
                        stringsAsFactors=F)

#calculateth n-gram combos
ngrams2_hl <- sapply(headlines$text,textcnt,method='string',n=2)
ngrams3_hl <- sapply(headlines$text,textcnt,method='string',n=3)
ngrams2_sn <- sapply(snippets$text,textcnt,method='string',n=2)
ngrams3_sn <- sapply(snippets$text,textcnt,method='string',n=3)

#define a function to extract the data from the ngrams, there may be better ways
#this was mine
#define  a helper function
mydf <- function(x) {
  y=datax
  if (length(y[[x]])>0) {
    data.frame(ngram=names(y[[x]]),
               freq=as.numeric(y[[x]]),index=x,
               stringsAsFactors=F)
  }
}

library(dplyr)
library(tidyr)
datax = ngrams2_hl
tmp2_hl <- bind_rows(Map(mydf,1:dim(headlines)[1]))
datax = ngrams3_hl
tmp3_hl <- bind_rows(Map(mydf,1:dim(headlines)[1]))

datax = ngrams2_sn
tmp2_sn <- bind_rows(Map(mydf,1:dim(snippets)[1]))
datax = ngrams3_sn
tmp3_sn <- bind_rows(Map(mydf,1:dim(snippets)[1]))


#combine and modify to have columns for each n-gram
ngrams_hl <- bind_rows(tmp2_hl,tmp3_hl) %>% mutate(ngram=paste0('hl_',gsub(' ','_',ngram))) %>%
  group_by(index,ngram) %>% summarise(freq=sum(freq)) 

ngrams_sn <- bind_rows(tmp2_sn,tmp3_sn) %>% mutate(ngram=paste0('sn_',gsub(' ','_',ngram))) %>%
  group_by(index,ngram) %>% summarise(freq=sum(freq)) 

#this yields way too many n-grams, lets identify the top ones so we can filter those 
#and only crerate variables for those
top_hl <- ngrams_hl %>% group_by(ngram) %>% summarise(P=n()/dim(headlines)[1]) %>% ungroup() %>% arrange(desc(P))
top_sn <- ngrams_sn %>% group_by(ngram) %>% summarise(P=n()/dim(snippets)[1]) %>% ungroup() %>% arrange(desc(P))

sum(top_sn$P >=0.01)
sum(top_hl$P >=0.005)
#if I chose the ones that are on 0.5% or more of dcuments then I get 27 variabls for headline and over 1% I get 19 for snippets
#let's go with this, as it is new yoprk is the top comb as expected for the NY Times

ngrams_hl1 <- ngrams_hl %>% filter(ngram %in% top_hl$ngram[top_hl$P >=0.005]) %>% spread(ngram,freq,fill=0)
ngrams_sn1 <- ngrams_sn %>% filter(ngram %in% top_sn$ngram[top_hl$P >=0.01]) %>% spread(ngram,freq,fill=0)

#Now I have to merge this by the index somehow, which is the row number
#for that add an index variable, whihc will be the row number
#I do not want to mess anything, so i will do a copy of modeling2 for that

modeling3 <- modeling2
modeling3$index <- as.numeric(row.names(modeling3))
modeling3 <- left_join(modeling3,ngrams_hl1)
modeling3 <- left_join(modeling3,ngrams_sn1)
#when the index was not found that created NAs, in this case they mean 0 on the ngram columns

modeling3[644:682][is.na(modeling3[644:682])] <- 0
which(sapply(modeling3,function(x) sum(is.na(x))>0))  #this shows only popular, and this is fine those are the test
table(modeling3$Popular,modeling3$group,useNA='ifany')

#create a new test and train from modeling3
train_new1 <- subset(modeling3,group==1,-c(group,index))
train_new1$Popular <- as.factor(train_new1$Popular)
test_new1 <- subset(modeling3,group==0,-c(group,Popular,index))

dim(train_new1)[2]- dim(train_new)[2]
dim(test_new1)[2]- dim(test_new)[2]
#I added 39 variables to both
setdiff(names(train_new1),names(test_new1))  #as expected the only difference is popular
setdiff(names(test_new1),names(train_new1))  #a nothing is in test that is not in train

#finally drop the variables not needed
train3 = train_new1[,setdiff(1:dim(train_new1)[2],drop1)]
train3$Popular = factor(train3$Popular,c(0,1),labels=c('N',"Y"))


#since the random forest is my best so far
#try a rf with the ngrams
model4 <- randomForest(Popular~.,data=train3)
model4
pred4 <- predict(model4,newdata=test_new1,type='prob')
confusionMatrix(predict(model4),
                train3$Popular,positive='Y')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred4[,2]),'submission4.csv',row.names=F)

#try a boosted tree using caret, wil use C5.0
#library('gbm')
cvCtrl <- trainControl(method = "cv", number = 10,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
model5 <- train(Popular~.,train3,
                method = "C5.0",
                metric = "ROC",trControl= cvCtrl)
model5
pred5 <- predict(model5,newdata=test_new1,type='prob')
confusionMatrix(predict(model5),
                train3$Popular,positive='Y')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred5[,2]),'submission5.csv',row.names=F)
#this moved it to 20th, it was a small improvement though

#try GBM
model6 <- train(Popular~.,train3,
                method = "gbm",
                metric = "ROC",trControl= cvCtrl)

pred6 <- predict(model6,newdata=test_new1,type='prob')
confusionMatrix(predict(model6),
                train3$Popular,positive='Y')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred6[,2]),'submission6.csv',row.names=F)



#I am running into issues with variables that are too sparse,
#in addition in researching that. I will take out soem highly correlated variables
#using findCorrelation
#Iwant to understand the data better to see what to do

prop.table(table(data$NewsDesk,useNA='ifany'))*100
prop.table(table(data$SectionName,useNA='ifany'))*100
prop.table(table(data$SubsectionName,useNA='ifany'))*100

#I need to further fix my data to clean some sparse data, o the section, subsection anc news desk variables
#for which i created my own cat vars, modeling is when i did that befor

modeling4 <- modeling 
levels(modeling4$NewsDesk )[1]<- 'blank'
levels(modeling4$SectionName )[1]<- 'blank'
levels(modeling4$SubsectionName )[1]<- 'blank'

#now several are very small, so they have really no information, or I will assume that
#I will need to have at least 5%
thresh = 250

names(which(table(modeling4$NewsDesk)<thresh))
levels(modeling4$NewsDesk )[(which(table(modeling4$NewsDesk)<thresh))] <- 'Other'

names(which(table(modeling4$SectionName)<thresh))
levels(modeling4$SectionName )[(which(table(modeling4$SectionName)<thresh))] <- 'Other'

names(which(table(modeling4$SubsectionName)<thresh))
levels(modeling4$SubsectionName )[(which(table(modeling4$SubsectionName)<thresh))] <- 'Other'


extra1 <- modeling4 %>% select(NewsDesk,SectionName,SubsectionName,weekday,UniqueID) %>% gather(var,level,-UniqueID) %>% 
  mutate(level=ifelse(level=="",'blank',level),n=1) %>%  unite(name,var,level,sep="_") %>% spread(name,n,fill=0)


modeling5 <- inner_join(modeling4,extra1)
modeling5 <- modeling5 %>% select(-c(NewsDesk,SectionName,SubsectionName,weekday))
which(sapply(modeling5,function(x) sum(is.na(x))>0))
names(modeling5) <- make.names(names(modeling5))
names(modeling5) <- gsub("\\.+","",names(modeling5))
modeling5$hour <- as.numeric(modeling5$hour)

#now add the ngrams
modeling6 <- modeling5
modeling6$index <- as.numeric(row.names(modeling6))
modeling6 <- left_join(modeling6,ngrams_hl1)
modeling6 <- left_join(modeling6,ngrams_sn1)
#when the index was not founf that created NAs, in this case they mean 0 on the ngram columns

modeling6[630:668][is.na(modeling6[630:668])] <- 0
which(sapply(modeling6,function(x) sum(is.na(x))>0))  #this shows only popular, and this is fine those are the test
table(modeling6$Popular,modeling6$group,useNA='ifany')


#as I should have assumed, the same work tends ot be in snippet and abstract and 
#hence the correlation is very high
#I need to take these out

correlated_vars <- findCorrelation(cor(modeling6[,-c(1:3,5,6,7,8,9,10)]))
names(modeling6[,-c(1:3,5,6,7,8,9,10)])[correlated_vars]

modeling7 <- modeling6[,-c(1:3,5,7,9,10)][,-correlated_vars]
#modeling7$UniqueID = modeling6$UniqueID

nzv <- nearZeroVar(modeling7,freqCut=99/1)
sapply(modeling7[nzv],mean)
names(modeling7[nzv])

modeling8 <- modeling7[,-nzv]
modeling8$UniqueID = modeling6$UniqueID  #I had dropped the uique IDs

#create a new test and train from modeling8
train_new2 <- subset(modeling8,group==1,-c(group,UniqueID))
train_new2$Popular <- as.factor(train_new2$Popular)
test_new2 <- subset(modeling8,group==0,-c(group,Popular))
sum(row.names(train_new2) != row.names(train_new1))  #sanity check
sum(row.names(test_new2) != row.names(test_new1))  #sanity check
levels(train_new2$Popular) <- c('N','Y')

#I think this set is good enough to try the ensemble, 
#first try the lasso


#try to regularize with lasso
library(glmnet)
model8 <- train(Popular~.,train_new2,
                method = "glmnet",family='binomial')

confusionMatrix(predict(model8),
                train_new2$Popular,positive='1')
model8$finalModel

#Next step is to try an ensemble
library(caretEnsemble)

#I will try an ensemble of rf, the boosted tree (gbm) and the log - all cv'd by caret consstently


ctrl1 <- trainControl(method='cv',number=2,savePredictions=T,classProbs=T, 
                      summaryFunction=twoClassSummary,
                      index=createFolds(train3$Popular, 5))
list1 = list(lasso=caretModelSpec(method='glmnet',family='binomial'),
             gbm = caretModelSpec(method='gbm'),
             rf = caretModelSpec(method='rf'))

list2 = list(log=caretModelSpec(method='glm',family='binomial'),
             rf = caretModelSpec(method='rf',tuneGrid=data.frame(.mtry=10)))
set.seed(234) #just in case
model_list <- caretList(Popular~.,data=train3[-drop2],
                        trControl=ctrl1,metric='ROC',tuneList=list1)

#try them not in tuneList but in methodList (not sure how to do binomial)
#try svm alone, then svm, gbm.c5.0 and rf
#it seems  data is so sparse in some vars that it is creating issues as well
#so what if gbm fails, maybe c5.0 works on ensemble

#the emsemble is not working like that - lets tune my models one by one, and then 
#try to use caretEnsemble, if that fails try caretStack, and if not then just build your own
#ensemble and your own stack model

ctrl2 <- trainControl(method='repeatedcv',number=10,repeats = 3,savePredictions=T,
                      classProbs=T, verboseIter = TRUE,
                      summaryFunction=twoClassSummary,
                      index=createMultiFolds(train_new2$Popular, 10,times=3))
library(doMC)
registerDoMC(cores = 3)

ptm = proc.time()
ensemble1 <- train(Popular~.,train_new2,
                   method = "glmnet",family='binomial',trControl=ctrl2)
proc.time()-ptm

ensemble1

confusionMatrix(predict(ensemble1),
                train_new2$Popular,positive='Y')
pred_ens1 <- predict(ensemble1,newdata=test_new2,type='prob')
write.csv(data.frame(UniqueID=test_new2$UniqueID,Probability1=pred_ens1[,2]),'submission_ens1.csv',row.names=F)



ptm = proc.time()
ensemble2 <- train(Popular~.,train_new2,
                   method = "rf",trControl=ctrl2,metric='ROC',tuneLength=3)
proc.time()-ptm

ensemble2
confusionMatrix(predict(ensemble2),
                train_new2$Popular,positive='Y')


pred_ens2 <- predict(ensemble2,newdata=test_new2,type='prob')
write.csv(data.frame(UniqueID=test_new2$UniqueID,Probability1=pred_ens2[,2]),'submission_ens2.csv',row.names=F)


ptm = proc.time()
ensemble3 <- train(Popular~.,train_new2,
                   method = "gbm",trControl=ctrl2)
proc.time()-ptm
ensemble3

#why would the better tuned model for gb do worse,
#I wangt to rerun thid
#it can be the data
ensemble3 <- train(Popular~.,train_new2,
                   method = "gbm",trControl=ctrl2)

ptm = proc.time()

confusionMatrix(predict(ensemble3),
                train_new2$Popular,positive='Y')
pred_ens3 <- predict(ensemble3,newdata=test_new2,type='prob')
write.csv(data.frame(UniqueID=test_new2$UniqueID,Probability1=pred_ens3[,2]),'submission_ens3.csv',row.names=F)




ptm = proc.time()
ensemble4 <- train(Popular~.,train_new2,
                   method = "svmPoly",trControl=ctrl2)
proc.time()-ptm

confusionMatrix(predict(ensemble4),
                train_new2$Popular,positive='Y')
pred_ens4 <- predict(ensemble4,newdata=test_new2,type='prob')
write.csv(data.frame(UniqueID=test_new2$UniqueID,Probability1=pred_ens4[,2]),'submission_ens4.csv',row.names=F)





#since I have not been able to get caretList to work, I will create my own list
#seems like it should work
model_list=list(ensemble1,ensemble2,ensemble3,ensemble4)

ensemble <- caretEnsemble(model_list)



#try to stacj my predictions using a gbm model
ensemble_data <- data.frame(model1=predict(ensemble1,type='prob')[,2],
                            model2=predict(ensemble2,type='prob')[,2],
                            model3=predict(ensemble3,type='prob')[,2],
                            model4=predict(ensemble4,type='prob')[,2],
                            Popular=train_new2$Popular)

registerDoMC(cores = 3)
ensemble_mix1 = train(Popular~.,ensemble_data,method='gbm',trControl=ctrl2)
registerDoMC(cores = 1)
#weird this selected model2, only

#what if I just average the Ps
preds_mix <- data.frame(UniqueID=test_new2$UniqueID,m1=pred_ens1[,2],m2=pred_ens2[,2],m3=pred_ens3[,2],m4=pred_ens4[,2])
preds_mix$Probability1 = apply(preds_mix[-1], 1,mean)
write.csv(preds_mix[c(1,6)],'submission_ens.avg.csv',row.names=F)

pred_aux <- as.factor(ifelse(apply(ensemble_data[1:4],1,mean)>=0.5,'Y','N'))


#What if I try my ensemble with the train3 set, and bootstrapping
#I will have to set the seeds as before - it iw worth a try
registerDoMC(cores = 3)
getDoParWorkers()

ctrl3 <- trainControl(method='repeatedcv',number=10,repeats=5,savePredictions=T,
                      classProbs=T, verboseIter = TRUE,
                      summaryFunction=twoClassSummary,
                      index=createFolds(train3$Popular, 10,times=5))
set.seed(2345)
seeds1 <- vector(mode='list',length=(10*5 +1))
for (i in 1:(10*5)) seeds1[[i]] = sample.int(1000,15)
seeds1[[10*5 +1 ]] <- sample.int(1000,1)

ensemble3b <- train(Popular~.,train_new2,
                    trControl = ctrl3,tuneLength=5,
                    method = "gbm",metric="ROC")

confusionMatrix(predict(ensemble3b),
                train3$Popular,positive='Y')
pred_ens3b <- predict(ensemble3b,newdata=train_new2,type='prob')
write.csv(data.frame(UniqueID=test_new2$UniqueID,Probability1=pred_ens4[,2]),'submission_ens3b.csv',row.names=F)

#you already have models for gbm (winner), rf (ngrams) and - so why no try that on caretEnsemble

list2 = list(log=caretModelSpec(method='glm',family='binomial'),
             rf = caretModelSpec(method='rf'),
             gbm=caretModelSpec(method='gbm',distribution='bernoulli'))

n=25


ctrl4 <- trainControl(method = "boot", number = n, savePredictions = T,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE,verboseIter =T,
                      index=createResample(train3$Popular, n))

model_list <- caretList(Popular~.,data=train3[,-drop2],
                        trControl=ctrl4,metric='ROC',tuneList=list2)

xyplot(resamples(model_list$gbm),resamples(model_list$rf))
modelCor(resamples(model_list))

rocs <- data.frame(gbm=model_list$gbm$resample$ROC,rf=model_list$rf$resample$ROC,log=model_list$log$resample$ROC)
ggplot(rocs,aes(x=gbm,y=rf))+geom_point()+coord_cartesian(ylim=c(0,1),xlim=c(0,1))
ggplot(rocs,aes(x=gbm,y=log))+geom_point()+coord_cartesian(ylim=c(0,1),xlim=c(0,1))
ggplot(rocs,aes(x=rf,y=log))+geom_point()+coord_cartesian(ylim=c(0,1),xlim=c(0,1))

#gbm and rf are fairly correlated
#rf took 9 hours, and  that seems to much to me, it is also highly correlated to gbm
#I think I will run some other types of models insted - 
#lest try knn and nnet
#but I i will try them first

nnet1 <- train(Popular~.,data=train3[,-drop2],trControl=ctrl4,metric='ROC',method='nnet',)
knn1 <- train(Popular~.,data=train3[,-drop2],trControl=ctrl4,metric='ROC',method='kknn')
glmnet1 = train(Popular~.,data=train3[,-drop2],trControl=ctrl4,metric='ROC',method='glmnet',family='bernoulli',tuneLength=5)


#Now that I figured (I hope) caretEnsemble and the seeds and all that, I can let my thing run
#I will use gbm, glmnet/binomial (regularized), nnet
load('train_test_full.rdata')
n=25
set.seed(1969)
ctrl5<- trainControl(method = "boot", number = n, savePredictions = T,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,verboseIter =T,
                     index=createResample(train4$Popular, n))

list3 = list(glm=caretModelSpec(method='glmnet',family='binomial',tuneLength=5),
             gbm=caretModelSpec(method='gbm',distribution='bernoulli',tuneLength=5),
             nnet=caretModelSpec(method='nnet',tuneLength=5))

registerDoMC(cores = 3)
set.seed(1969)
model_list1 <- caretList(Popular~.,data=train3[,-drop2],
                         trControl=ctrl5,metric='ROC',tuneList=list3)

save(model_list1,file='model_list1.rdata')

#add a emseble ist with log(wordCount)
train4 <- train3[-which(names(train3)=='WordCount')]
train4$logwordcount <- log(train3$WordCount+1)

registerDoMC(cores = 3)
set.seed(1971)
model_list2 <- caretList(Popular~.,data=train4[,-drop2],
                         trControl=ctrl5,metric='ROC',tuneList=list3)

save(model_list2,file='model_list2.rdata')


greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

drop2 <- which(sapply(train3s,sd)==0)

qplot(model_list1$glm$resample$ROC,model_list1$gbm$resample$ROC,geom="point")+coord_cartesian(ylim=c(0,1),xlim=c(0,1))
qplot(model_list1$glm$resample$ROC,model_list1$nnet$resample$ROC,geom="point")+coord_cartesian(ylim=c(0,1),xlim=c(0,1))
qplot(model_list1$gbm$resample$ROC,model_list1$nnet$resample$ROC,geom="point")+coord_cartesian(ylim=c(0,1),xlim=c(0,1))

modelCor(resamples(model_list1))


#NOw lest do the ensemble
registerDoMC(cores = 3)
greedy_ensemble <- caretEnsemble(model_list1)
summary(greedy_ensemble)

greedy_ensemble2 <- caretEnsemble(model_list2)

pred_greedy1<- predict(greedy_ensemble,newdata=test_new1)
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_greedy1),
          'submission_ens_greedy.csv',row.names=F)

test_new1a <- test_new1
test_new1a$logwordcount <- log(test_new1$WordCount+1)

pred_greedy2<- predict(greedy_ensemble2,newdata=test_new1a)
write.csv(data.frame(UniqueID=test_new1a$UniqueID,Probability1=pred_greedy2),
          'submission_ens_greedy2.csv',row.names=F)



ens_preds1 <- predict(greedy_ensemble, newdata=test_new1)
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=ens_preds1),'submission_ens1.csv',row.names=F)



gbm_ensemble <- caretStack(
  model_list1, 
  method='gbm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_stack1 <- predict(gbm_ensemble,test_new1,type='prob')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_stack1[,2]),'submission_ens_gbm.csv',row.names=F)

glm_ensemble <- caretStack(
  model_list1, 
  method='glm',family='binomial',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_ens_glm1 <- predict(glm_ensemble,test_new1,type='prob')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_ens_glm1[,2]),
          'submission_ens_glm.csv',row.names=F)


#### ebs 2
gbm_ensemble2 <- caretStack(
  model_list2, 
  method='gbm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_stack2 <- predict(gbm_ensemble2,test_new1a,type='prob')
write.csv(data.frame(UniqueID=test_new1a$UniqueID,Probability1=pred_stack2[,2]),'submission_ens_gbm2.csv',row.names=F)

glm_ensemble2 <- caretStack(
  model_list2, 
  method='glm',family='binomial',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_ens_glm2 <- predict(glm_ensemble2,test_new1a,type='prob')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_ens_glm2[,2]),
          'submission_ens_glm2.csv',row.names=F)


#what If I try rf with this

set.seed(123)
rf_ensemble2 <- caretStack(
  model_list2, 
  method='rf',
  metric='ROC', tuneLength=3,
  trControl=trainControl(
    method='boot',
    number=25,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_ens_rf2 <- predict(rf_ensemble2,test_new1a,type='prob')
write.csv(data.frame(UniqueID=test_new1a$UniqueID,Probability1=pred_ens_rf2[,2]),
          'submission_ens_rf2.csv',row.names=F)
###
#the log helped a lot
#I also would like to try t impite the missing owrkd count, I think that 
#could help the log model, as 0 may be makig it not idal

#other idea is to try PCA preprocess (or ICA prep-process)
#with list 3 since that is my best one so far
#then move forward

drop2 <- which(sapply(train4,sd)==0)
set.seed(85)
ctrl_pca = trainControl(method='boot',number=5,savePredictions = T,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,verboseIter =T)
registerDoMC(1)
nzv1 = nearZeroVar(train4[])
set.seed(67)
gbm_pca <- train(Popular~.,data=train4[,-nzv1],preProcess=c("pca"),
                 trControl = ctrl_pca,metric="ROC",tune_length=3,
                 method='gbm',distribution='bernoulli')


save(model_list,file='model_list.rdata')
save.image('kaggle.rdata')
save(train_new2,drop2,test_new2,file='train_test_new2')

save.image('ensemble_models1_image.rdata')
save.image('ensemble_models2_image.rdata')

#At this point I am <1% from the top, I am sure I can tune this and get some more
#try svm and other stuff for example, but what if there is a better approach
#what if the classification first makes sense
#also by definition I am in the land of diniishig returns
#if we were prediting cancer 1% is a lor of lives,
#blogs are much less important

#one idea is to use the log of word count for the glm  model, 
#that one should improve hopefully
#and that should improve the ensemble
#in fact since it makes nt difference for trees (as a cut in 7 is  the same)
#as a cut in log(7), lets just do it for all
#what is with wordCount = 0, should I impute?
#did it and I made it to #6


#I want to try somrandom models, see if I get something that can improve
#lda, svm

set.seed(567)
svm <- train(Popular~.,data=train4[,-drop2],method='svmRadial',metric='ROC',trControl = ctrl5)
qplot(svm$resample$ROC,model_list2$glm$resample$ROC)+coord_cartesian(ylim=c(0,1),xlim=c(0,1))
qplot(svm$resample$ROC,model_list2$nnet$resample$ROC)+coord_cartesian(ylim=c(0,1),xlim=c(0,1))

ctrl6<- trainControl(method = "none", number = n, savePredictions = T,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,verboseIter =T,
                     index=createResample(train3$Popular, n))
set.seed(567)
drop4 <- nearZeroVar(train4[-which(names(train4)=='Popular')],freq=99/1)
drop5 <- findCorrelation(cor(train4[-c(which(names(train4)=='Popular'),drop4)]))
lda1 <- train(Popular~.,data=train4[,-c(drop2,drop4)],method='lda2',metric='ROC',trControl = ctrl6)


#I want to estimate the AUC on the 2 best models 
set.seed(234)
my_boot <- createResample(train4$Popular,times = 25)
library(ROCR)
getROC <- function(model,x) {
  pred = predict(model,newdata=train4[x,],type='prob')
  roc = prediction(pred[,2],train4$Popular[x])
  as.numeric(performance(roc, "auc")@y.values)
}
glm_ens2_stats <- sapply(my_boot, function(x) getROC(glm_ensemble2,x))
gbm_ens2_stats <- sapply(my_boot, function(x) getROC(gbm_ensemble2,x))

perf_data <- data.frame(glm=glm_ens2_stats,gbm=gbm_ens2_stats,index=1:25)
perf_data %>% gather(model, ROC, -index) %>% 
  ggplot(aes(x=index,y=ROC,color=model,group=model))+
  geom_line()+coord_cartesian(ylim=c(0.95,0.975))+
  scale_y_continuous(labels=percent)+
  geom_line(stat='hline',yintercept='mean')

mean( sapply(my_boot, function(x) getROC(svm,x)))

pred_svm <- predict(svm,test_new1a,type='prob')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_svm[,2]),
          'submission_svmrad.csv',row.names=F)
confusionMatrix(predict(svm),train4$Popular,positive='Y')

#svm is quite uncorrelated to the others
#so lest add it to the list

#it took forever runnign all four, so I will come back later and add it manually
#Zach Meyer helpoed e figure out how

set.seed(2006)
ctrl6<- trainControl(method = "boot", number = n, savePredictions = T,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,verboseIter =T,
                     index=createResample(train4$Popular, n))


list4 = list(svm=caretModelSpec(method='svmPoly',tuneLength=5),
             glm=caretModelSpec(method='glmnet',family='binomial',tuneLength=5),
             gbm=caretModelSpec(method='gbm',distribution='bernoulli',tuneLength=5),
             nnet=caretModelSpec(method='nnet',tuneLength=5))


registerDoMC(cores = 3)
model_list4 <- caretList(Popular~.,data=train4[,-drop2],
                         trControl=ctrl6,metric='ROC',tuneList=list4)

save(model_list4,file='model_list4.rdata')


#running all 4 took forever, but that let me to lern that I can 
#run one seprately with train and add it
#lets add svm

ctrlx<- trainControl(method = "boot", number=25, savePredictions = T,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,verboseIter =T)

registerDoMC(cores = 4)
svm_extra <- train(Popular~.,data=train4[,-drop2],
                   trControl=ctrl5,metric='ROC',method='svmPoly',
                   tuneLength=3)
registerDoMC(cores = 1)
save(svm_extra,file='svm_extra.rdata')

#combine the list

model_list3 <- model_list2 
model_list3[['svm']] <- svm_extra


#somehow now it is telling me that the model_list2 and model_list3 do not have the same
#resampling, somehow it got messed up
#I need to run the other models again tonight

glm_ensemble3 <- caretStack(
  model_list3, 
  method='glm',family='binomial',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=25,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

pred_ens_glm3 <- predict(glm_ensemble3,test_new1a,type='prob')
write.csv(data.frame(UniqueID=test_new1$UniqueID,Probability1=pred_ens_glm3[,2]),
          'submission_ens_glm3.csv',row.names=F)





#I am going to try to cluster the data, in the lesson it says as i underatand it
#you cluster on train only, and use that , so I will do that
rm(list=ls())
load("~/Documents/Analytics_edge/train_test_full.rdata")

#first I need to scale the data, it is mostly for wordCount, as all others are 0/1 
#or very close
#and even before impute the word count for those that are 0 as that is
#illogical to me

load('modeling.rdata')

train_predictors <- modeling[modeling$group==1,c(10,1:3,7,14)]
#save mean for imputation
meanx = mean(train_predictors$WordCount[train_predictors$WordCount !=0])
train_predictors$WordCount[train_predictors$WordCount ==0] <- meanx

train_predictors$logwc <- log(train_predictors$WordCount)
train_predictors <- train_predictors[-5]
extra2 <-  train_predictors %>% select(-logwc)  %>%
  gather(variable,level,-UniqueID) %>% 
  group_by(UniqueID,variable,level) %>% summarise(N=n()) %>% 
  unite(key,variable,level,sep="_") %>% spread(key,N,fill=0)
train_predictors1 <- left_join(train_predictors[c('UniqueID','logwc')],extra2)

scale1 <- preProcess(train_predictors1)
scaled_preds <- predict(scale1,newdata=train_predictors1)

dist = dist(scaled_preds)

hclust1 <- hclust(dist,method='ward.D')
plot(hclust1)
clusters=cutree(hclust1,k=4)

kmeans1 <- kmeans(scaled_preds,4)
table(kmeans1$cluster,train_new1$Popular)
kclusters <- kmeans1$cluster
#lest see if they make sense
#modeling has the original data that came, so I can use that for a quick profile

aux <- modeling[modeling$group==1,c(9,1,2,3)]
aux$cluster =  kclusters

table(kclusters,train3$Popular)


profile = aux %>% group_by(cluster) %>% gather(variable,level,-cluster) %>% 
  group_by(cluster,variable,level) %>% summarise(N=n()) %>%
  group_by(cluster,variable) %>% mutate(P=N/sum(N)) %>% select(-N) %>%
  spread(cluster,P,fill=0)

View(profile)


#Because gbm is my best model, I will keep using gbm
#and build 4 models, one for each cluster
load('train4_files.rdata')

n=25
registerDoMC(cores = 3)
getDoParWorkers()

for (i in 1:4) {
  trainx <- subset(train4,clusters==i) #get the data for that cluster
  #I need to take out the vars with no variability, or gbm fails
  dropx <- nearZeroVar(trainx,freq=100/0)
  set.seed(2006) #set a seed for reproductibility
  #define control parameters
  ctrlx<- trainControl(method = "boot", number = n, savePredictions = T,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,verboseIter =T,
                       index=createResample(trainx$Popular, n))
  #run model
  modelx <- train(Popular~.,trainx[,-c(drop2,dropx)],method='gbm',
                  distribution='bernoulli',trControl = ctrlx,metric='ROC')
  #save results
  assign(paste0('gbm_clust_',i),modelx)
}

save(list=ls(pattern='gbm_clust'),file='cluster_models.rdata')
#they are worth trying, oI just need to combined them

#first  need to cluster the test data
#for that I imputd first the missing word to mean, then did the log,  then scaled

test_new1b <- test_new1
test_predictors <- modeling[modeling$group==0,c(10,1:3,7,14)]
test_predictors$WordCount[test_predictors$WordCount ==0] <- meanx

test_predictors$logwc <- log(test_predictors$WordCount)
test_predictors <- test_predictors[-5]
extra2test <-  test_predictors %>% select(-logwc)  %>%
  gather(variable,level,-UniqueID) %>% 
  group_by(UniqueID,variable,level) %>% summarise(N=n()) %>% 
  unite(key,variable,level,sep="_") %>% spread(key,N,fill=0)
#I am missing some variables, I need to add them
missing <- setdiff(names(extra2),names(extra2test))
missing_df = data.frame(matrix(rep(0,dim(extra2test)[1]*length(missing)),dim(extra2test)[1],length(missing)))
names(missing_df) <- missing
missing_df$UniqueID = extra2test$UniqueID
extra2test <- inner_join(extra2test,missing_df)
test_predictors1 <- left_join(test_predictors[c('UniqueID','logwc')],extra2test)



scaled_test_preds <- predict(scale1,newdata=test_predictors1)

library(flexclust)
clust.kcca = as.kcca(kmeans1,train_predictors1)
test_clusters = predict(clust.kcca,newdata=test_predictors1)
table(test_clusters)
library(dplyr)
library(plyr)
library(zoo)
library(tm)
library(arm)
library(ROCR)
library(randomForest)
library(dynamicTreeCut)

# train0 - original train set
# test0 - original test set
# train - training set
# test - test set
# train1 - first 5000 rows of train - used for CV
# test1 - rest of train set - used as CV set
# all - original train + test set
# all2 - new train + test set

## kfold function:
## http://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/forums/t/13752/do-you-want-a-better-model


# load & join data
train0 = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
test0 = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
train0 = train0[order(train0$PubDate),]
test0 = test0[order(test0$PubDate),]
all = rbind(dplyr::select(train0,-Popular), test0)


#### PubDate ####

pd = as.POSIXlt( all$PubDate )
# mon, mday, hour, min, wday, yday

all2 = data.frame(id = all$UniqueID)
all2$NewsDesk = all$NewsDesk
# state holidays: 1.9., 13.10., 11.11., 27.11., 25.12.
sh <- (pd$mon==8 & pd$mday==1) | (pd$mon==9 & pd$mday==13) | (pd$mon==10 & pd$mday==11) | 
    (pd$mon==10 & pd$mday==27) | (pd$mon==11 & pd$mday==25)
all2$weekend <- pd$wday==0 | pd$wday==6 | sh
all2$hour <- pd$hour
all2$min <- pd$min
all2$ts <- 60*all2$hour + all2$min


## last - how much time has passed till publication of the last article
## the idea is that the bigger the number of articles published 
## the lesser the average popularity as attention is spread to more articles
pd = as.POSIXlt( all$PubDate )
z = zoo(as.numeric(pd))
n = nrow(all2)
b = zoo(, seq(n))

all2$last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))


# order table
all2 = all2[order(all2$id),]

## fill in NAs
# count averages
na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
    last1=mean(last1, na.rm=TRUE),
    last3=mean(last3, na.rm=TRUE),
    last5=mean(last5, na.rm=TRUE),
    last10=mean(last10, na.rm=TRUE),
    last20=mean(last20, na.rm=TRUE),
    last50=mean(last50, na.rm=TRUE)
)

# fill in averages
na.merge = merge(all2, na.avg, by=c("weekend","hour"))
na.merge = na.merge[order(na.merge$id),]
for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
    y = paste0(i, ".y")
    idx = is.na(all2[[i]])
    all2[idx,][[i]] <- na.merge[idx,][[y]]
}


rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)



#### Section ####

## paste NewsDesk + SectionName + SubsectionName together to produce $section
## then merge some sections together to produce cat. variable with 20 levels

all = all[order(all$UniqueID),]
all2$section = paste0(all$NewsDesk, "#", all$SectionName, "#", all$SubsectionName)

t = table(paste0(train0$NewsDesk, "#", train0$SectionName, "#", train0$SubsectionName), train0$Popular)
t1 = round( prop.table(t,1), 3)
cbind(t,t1)

## merge some sections 
all2$section <-
    plyr::revalue(all2$section, c(
        "Business##" = "Business#Business Day#Dealbook",
        "#Business Day#Small Business" = "Business#Business Day#Small Business",
        "#Crosswords/Games#" = "Business#Crosswords/Games#",
        "Culture##" = "Culture#Arts#",
        "Foreign#World#" = "Foreign##",
        "#Health#" = "Science#Health#",
        "National##" = "Other",
        "National#U.S.#Politics" = "Other",
        "OpEd##" = "OpEd#Opinion#",
        "#Open#" = "Other",
        "#Opinion#" = "#Opinion#Room For Debate",
        "#Opinion#The Public Editor" = "OpEd#Opinion#",
        "Science##" = "Science#Health#",
        "Sports##" = "Other",
        "Sports#Sports#" = "Other",
        "Styles#Health#" = "Styles##",
        "Styles#Style#Fashion & Style" = "Styles##",
        "#Travel#" = "Travel#Travel#",
        "#U.S.#" = "#U.S.#Education",
        "#Arts#" = "Culture#Arts#",
        "#Business Day#Dealbook" = "Business#Business Day#Dealbook",
        "#N.Y. / Region#" = "Metro#N.Y. / Region#",
        "#Technology#" = "Business#Technology#",
        "#World#Asia Pacific" = "Foreign#World#Asia Pacific"
    ))



#### Headline ####

# count the 'c' character
count.c = function(col, c) {
    sapply(sapply(gregexpr(paste0("[",c,"]"), col), function(x) { as.integer(x>=0) * length(x) } ), max)
}

all2$head.nchar = nchar(all$Headline)
all2$head.nwords = sapply(strsplit(all$Headline, ' '), length)
all2$head.nupper = count.c(all$Headline, "A-Z")
all2$head.number = count.c(all$Headline, "0-9")
all2$head.dollar = count.c(all$Headline, "$")
all2$head.exclam = count.c(all$Headline, "!")
all2$head.question = count.c(all$Headline, "?")
all2$head.comma = count.c(all$Headline, ",")
all2$head.dot = count.c(all$Headline, ".")
all2$head.quote = count.c(all$Headline, "'")
all2$head.colon = count.c(all$Headline, ":")
all2$head.pipe = count.c(all$Headline, "|")
all2$head.year = as.integer(attributes( regexpr( "[0-9]{4}:", strsplit(all$Headline, ' ') ))$match.length == 5)

all2$snip.nchar = nchar(all$Snippet)
all2$snip.nwords = sapply(strsplit(all$Snippet, ' '), length)
all2$snip.nupper = count.c(all$Snippet, "A-Z")
all2$snip.number = count.c(all$Snippet, "0-9")
all2$snip.dollar = count.c(all$Snippet, "$")
all2$snip.exclam = count.c(all$Snippet, "!")
all2$snip.question = count.c(all$Snippet, "?")
all2$snip.comma = count.c(all$Snippet, ",")
all2$snip.dot = count.c(all$Snippet, ".")
all2$snip.quote = count.c(all$Snippet, "'")
all2$snip.colon = count.c(all$Snippet, ":")
all2$snip.pipe = count.c(all$Snippet, "|")
all2$snip.year = as.integer(attributes( regexpr( "[0-9]{4}:", strsplit(all$Snippet, ' ') ))$match.length == 5)

# like top400 head words + top500 snip words

# corpus - HeadLine
corpus = Corpus(VectorSource(all$Headline))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.9975)
headSparse = as.data.frame(as.matrix(spdtm))
colnames(headSparse) = make.names(colnames(headSparse))
names(headSparse) = paste0("head_", names(headSparse))
all2 = cbind(all2, headSparse)


# corpus - Snippet
corpus = Corpus(VectorSource(all$Snippet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.995)
snipSparse = as.data.frame(as.matrix(spdtm))
colnames(snipSparse) = make.names(colnames(snipSparse))
names(snipSparse) = paste0("snip_", names(snipSparse))
all2 = cbind(all2, snipSparse)


## the same but with lower numbers - like 40 head words & 60 snip words
## it worked pretty well in my earlier submission so I decided to add it to my final model

# corpus - HeadLine
corpus = Corpus(VectorSource(all$Headline))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.99)
headSparse = as.data.frame(as.matrix(spdtm))
colnames(headSparse) = make.names(colnames(headSparse))
names(headSparse) = paste0("head2_", names(headSparse))
all2 = cbind(all2, headSparse)

# corpus - Snippet
corpus = Corpus(VectorSource(all$Snippet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.98)
snipSparse = as.data.frame(as.matrix(spdtm))
colnames(snipSparse) = make.names(colnames(snipSparse))
names(snipSparse) = paste0("snip2_", names(snipSparse))
all2 = cbind(all2, snipSparse)

rm(headSparse, snipSparse, corpus, dtm, spdtm)


#### split data.frame ####
all2$hour = as.factor(all2$hour)
all2$WordCount = all$WordCount

all2$NewsDesk = as.factor(all2$NewsDesk)
all2$section = as.factor(all2$section)
n_train = nrow(train0)
n_all = nrow(all)
train = all2[1:n_train,]
test = all2[(n_train+1):n_all,]
train0 = train0[order(train0$UniqueID),]
train$Popular = train0$Popular

#train$hour = as.factor(train$hour %/% 3)

train1 = train[1:5000,]
test1 = train[5001:6532,]


## auc for glm
auc = function(model, test1) {
    PredictROC = predict(model, test1, type="response")
    pred = prediction(PredictROC, test1$Popular)
    perf = performance(pred, "tpr", "fpr")
    as.numeric(performance(pred, "auc")@y.values)
}

# auc for randomForest
aucRF = function(model, test1) {
    PredictROC = predict(model, test1, type="prob")[,2]
    pred = prediction(PredictROC, test1$Popular)
    perf = performance(pred, "tpr", "fpr")
    as.numeric(performance(pred, "auc")@y.values)
}


#### Clustering ####

## the idea is that within each section there are different groups of articles
## each group with different popularity

## probably, this can be done better, but I came up with this idea on the very last day
## I used minClusterSize=20 to avoid overfitting
## I used chi-square test to decide whether clustering does make sense

head_names = names(train)
head_names = head_names[grep( "head_",  head_names )]
snip_names = names(train)
snip_names = snip_names[grep( "snip_",  snip_names )]

t = table(train$section, train$Popular)
t1 = round( prop.table(t,1), 3)
cbind(t,t1)

# ##                                   1169 115 0.910 0.090   OK min48 x12
# Business#Business Day#Dealbook        868  89 0.907 0.093   OK min21 x9
# Business#Business Day#Small Business  135   5 0.964 0.036   OK min25 x4
# Business#Crosswords/Games#             20 103 0.163 0.837   KO
# Business#Technology#                  280  50 0.848 0.152   OK min21 x6
# Culture#Arts#                         626  50 0.926 0.074   OK min27 x9
# Foreign##                             172   0 1.000 0.000   -
# Foreign#World#Asia Pacific            200   3 0.985 0.015   -
# Magazine#Magazine#                     31   0 1.000 0.000   -
# Metro#N.Y. / Region#                  181  17 0.914 0.086   OK min20 x5 ???
# #Multimedia#                          139   2 0.986 0.014   -
# OpEd#Opinion#                         117 424 0.216 0.784   OK min 25 x5
# #Opinion#Room For Debate               65   2 0.970 0.030   -
# Other                                  10   0 1.000 0.000   -
# Science#Health#                        73 122 0.374 0.626   KO  + podla dlzky
# Styles##                              119   1 0.992 0.008   -
# Styles#U.S.#                           77 100 0.435 0.565   KO - toto netusim
# Travel#Travel#                        116   1 0.991 0.009   -
# TStyle##                              715   9 0.988 0.012   ????
# #U.S.#Education                       326   0 1.000 0.000   -

all2$clusterid = 1

for (section in c("##", "Business#Business Day#Dealbook", "Business#Business Day#Small Business",
                  "Business#Technology#", "Culture#Arts#", "Metro#N.Y. / Region#",
                  "OpEd#Opinion#")) {
    
    distances = dist(all2[all2$section==section,c(head_names,snip_names)], method = "binary")
    clusters = hclust(distances, method = "ward.D2") 
    #clusterGroups = cutree(clusters, k=7)
    clusterGroups = cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
    clusterGroups[clusterGroups==0] = 1
    #summary(factor(clusterGroups))
    
    # add to all2 - then split the data again
    all2[all2$section==section,]$clusterid = clusterGroups
}

# split the again
all2$clusterid = factor(all2$clusterid)
train = all2[1:n_train,]
test = all2[(n_train+1):n_all,]
train0 = train0[order(train0$UniqueID),]
train$Popular = train0$Popular
train1 = train[1:5000,]
test1 = train[5001:6532,]


#t = table(factor(train[train$section==section,]$clusterid), train[train$section==section,]$Popular)
#t1 = round( prop.table(t,1), 3)
#cbind(t,t1)

#tab = cbind(t,t1)[,1:2]
#chisq.test(tab)

rm(distances, clusters, clusterGroups)


#### my best models ####

head2_names = names(train)
head2_names = head2_names[grep( "head2_",  head2_names )]
snip2_names = names(train)
snip2_names = snip2_names[grep( "snip2_",  snip2_names )]



## my best glm; features added based on cross-validation
## in my previous submissions I used weekend::poly(ts,7) as weekend has different curve,
## but I think it overfitted the weekend curve
## and after futher examination I found out that some sections are more dependent on time than others
## so I changed it to section:weekend + section:poly(ts,5)
## 

## CV (test1):      0.9540409
## 5-fold:          0.9466
## Kaggle-public:   0.93034
## Kaggle-private:  0.91107 (38th)

modelBayesGLM <- bayesglm(data=train, family="binomial", Popular ~ 
    section*I(log(WordCount + 1)) + section:clusterid + section:weekend + section:poly(ts,5) +
    (head.question>0) + section:(snip.pipe>0)
)
#auc(modelBayesGLM, test1)

# kfold(data=train, model=bayesglm, family="binomial", pargs=list(type="response"), formula= Popular ~
#     section*I(log(WordCount + 1)) + section:clusterid + section:weekend + section:poly(ts,5) + 
#     (head.question>0) + section:(snip.pipe>0)
# )


## the same as above, but with top40 headline words and top60 snippet words
## my earlier submissions performed well with this so I decided to add it to the final model

## CV (test1):      0.9535202
## 5-fold:          0.9429186
## Kaggle-public:   0.93273
## Kaggle-private:  0.91508 (12th)

formula = formula(paste("Popular ~ ",
    "section*I(log(WordCount + 1)) + section:clusterid + section:weekend + section:poly(ts,5) + 
    (head.question>0) + section:(snip.pipe>0) + ",
    paste(head2_names, collapse=" + "), " + ",
    paste(snip2_names, collapse=" + ")
))
modelWords099 <- bayesglm(data=train, family="binomial", formula=formula)
#auc(modelWords099, test1)

#kfold(data=train, model=bayesglm, family="binomial", pargs=list(type="response"), formula= formula)


## then I mixed it with some crappy RF, and the ensemble finished 9th
## sorry, but don't remember exact formula, it was 5min before deadline
## 

## after the deadline I tried some RFs, most of them were crap, except this one:
    
## CV (test1):      0.9504049
## 5-fold:          0.9431
## Kaggle-public:   ????
## Kaggle-private:  0.90875 (59th)

# I had some RFs with 5-fold 0.9451, but they didn't perform that well in the private rankings
    
formula = formula(paste("factor(Popular) ~ ",
    "section + WordCount + clusterid + weekend + ts +
    head.question + last20 + head.nchar + snip.nupper +
    head_agenda + head_today"
))
modelWords099RF <- randomForest(data=train, formula, ntrees=5000)
#aucRF(modelWords099RF, test1)

#kfold(data=train, model=randomForest, pargs=list(type="prob"), ntrees=5000, formula = formula)


# make predictions
prdBayesGLM = predict(modelBayesGLM, test, type="response")
prdWords099 = predict(modelWords099, test, type="response")
prd099RF = predict(modelWords099RF, test, type="prob")[,2]

# make ensemble
# this was just blind guess, I didn't had time to try various combinations
prdMix = 0.8*prdWords099 + 0.2*prd099RF


## my final ensemble (with lost formula):
## Kaggle-private:  0.91640 (9th)

## if i had used this RF it would be:
## Kaggle-private:  0.91794 (7th)


# write the results
MyData = data.frame(UniqueID = test$id)
MyData$Probability1 = prdMix
write.csv(MyData, file = "submission_BBBMix.csv", row.names = FALSE)




#### after the competition ####

## I added last20 to bayes:
## Kaggle-private:  0.91380 (15th)

## then I added words (head2_names+snip2_names):
## Kaggle-private:  0.91758 (7th)

## then ensembled with RF:
## Kaggle-private:  0.92031 (2nd)

formula = formula(paste("Popular ~ ",
    "section*I(log(WordCount + 1)) + section:clusterid + section:weekend + section:poly(ts,5) + 
    (head.question>0) + section:(snip.pipe>0) + section:I(log(last20 + 1)) + ",
    paste(head2_names, collapse=" + "), " + ",
    paste(snip2_names, collapse=" + ")
))
modelBayesGLM2 <- bayesglm(data=train, family="binomial", formula=formula)
prdBayesGLM = predict(modelBayesGLM2, test, type="response")
MyData$Probability1 = prdBayesGLM
write.csv(MyData, file = "submission_BBBBayes2.csv", row.names = FALSE)

# ensemble
prdMix = 0.6*prdWords099 + 0.4*prd099RF
MyData$Probability1 = prdMix
write.csv(MyData, file = "submission_BBBMix60-40.csv", row.names = FALSE)

# Author: David O'Connor
# April, 2015
#
#The data provided for this competition is split into two files: 
#  NYTimesBlogTrain.csv = the training data set. It consists of 6532 articles.
#  NYTimesBlogTest.csv = the testing data set. It consists of 1870 articles.  
#We have also provided a sample submission file, SampleSubmission.csv. 
#This file gives an example of the format of submission files 
#(see the Evaluation page for more information). The data for this 
#competition comes from the New York Times website.

#Variable Descriptions
#The dependent variable in this problem is the variable Popular, 
#which labels if an article had 25 or more comments in its online 
#comment section (equal to 1 if it did, and 0 if it did not). The 
#dependent variable is provided in the training data set, but not the 
#testing dataset.
#The independent variables consist of pieces of article data available 
#at the time of publication, and a unique identifier:  
#  NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
#  SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
#  SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
#  Headline = the title of the article
#  Snippet = a small portion of the article text
#  Abstract = a summary of the blog article, written by the New York Times
#  WordCount = the number of words in the article
#  PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
#  UniqueID = a unique identifier for each article

readData = function() {
  dfTrain <<- read.csv("~/Documents/Kaggle/NYTimesBlogTrain.csv",stringsAsFactors=FALSE)
  dfTest <<- read.csv("~/Documents/Kaggle/NYTimesBlogTest.csv",stringsAsFactors=FALSE)
}

buildData = function() {
  require(tm)
  require(caret)
  require(openNLP)
  readData()
  dfAll = rbind(dfTrain[c(1:8,10)],dfTest)
  
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Business"] = "Business Day"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Culture"] = "Arts"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Foreign"] = "World"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "National"] = "U.S."
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "OpEd"] = "Opinion"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Science"] = "Health"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Sports"] = "Sports"
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "Styles"] = "U.S."
  dfAll$SectionName[dfAll$SectionName=="" & dfAll$NewsDesk == "TStyle"] = "TStyleSec"
  
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Arts"] = "Culture"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Business Day"] = "Business"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Crosswords/Games"] = "Business"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Health"] = "Science"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "N.Y. / Region"] = "Metro"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Opinion"] = "OpEd"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Technology"] = "Business"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Travel"] = "Travel"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "U.S."] = "National"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "World"] = "Foreign"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Multimedia"] = "MultimediaDesk"
  dfAll$NewsDesk[dfAll$NewsDesk=="" & dfAll$SectionName == "Open"] = "Open"
  dfAll$SubsectionName[dfAll$SubsectionName==""] = dfAll$SectionName[dfAll$SubsectionName==""]
  
  hits = grepl("white house|first draft|today in politics|obama|clinton|kerry|Boehner|congress|senate|republican|democrat|McConnell|Biden|verbatim|the upshot|Supreme Court",dfAll$Headline,ignore.case=TRUE)
  dfAll$NewsDesk[dfAll$NewsDesk=="" & hits] = "National"
  dfAll$SectionName[dfAll$SectionName=="" & hits] = "U.S."
  dfAll$SubsectionName[dfAll$SubsectionName=="" & hits] = "Politics"
  
  dfAll$NewsDesk = as.factor(dfAll$NewsDesk)
  dfAll$SectionName = as.factor(dfAll$SectionName)
  dfAll$SubsectionName = as.factor(dfAll$SubsectionName)
  
  # day of the week post was published
  dfAll$dow = as.factor(weekdays(as.Date(dfAll$PubDate)))
  dfAll$PubDate = as.POSIXct(dfAll$PubDate)
  
  # hour of the day post was published
  times = as.POSIXlt(dfAll$PubDate)
  dfAll$tod = times$hour
  dfAll$isQ = as.factor(ifelse(grepl("\\?",dfAll$Headline),"yes","no"))
  
  # interesting (and not) words and phrases in headline and abstract
  notBoring.hWords = "can |wall | work|kids|^how|^what [^w]|no |torture|climate|police|obama|&|recap|uber|not |cuba|you"
  boring.hWords = "design|sale|first draft|london|british|Daily Clip Report|gift:|fashion|agenda|report:|verbatim| art |2014|business| day|Morning Agenda|What We're|buy|Q\\. and A\\.|\\$|q's|today in politics|daily|today|[[:digit:]]{4}:|Pictures of the Day"
  notBoring.aWords = "find|families|think| change| may |become|study|energy|parents|america|child|school|puzzle|ISIS|your|decision"
  boring.aWords = "shares|release|review|breakingviews|music|editors | art |herald tribune|^photos|market|Metropolitan Diary|film|Daily Clip Report|highlights|china|white house|articles"
  isBoring = grepl(paste0("\\||",boring.hWords),dfAll$Headline,ignore.case=TRUE) | grepl(boring.aWords,dfAll$Abstract,ignore.case=TRUE)
  isNotBoring = grepl(notBoring.aWords,dfAll$Abstract,ignore.case=TRUE) | grepl(notBoring.hWords,dfAll$Headline,ignore.case=TRUE)
  dfAll$isBoring = as.factor(ifelse(isBoring,"yes","no"))
  dfAll$isNotBoring = as.factor(ifelse(isNotBoring,"yes","no"))
  
  # numeric measure of popularity of posts containing this phrase
  dfAll$wordP = 0
  words = strsplit(notBoring.hWords,"\\|")[[1]]
  for (i in 1:length(words)) {
    hits = grepl(words[i],dfAll$Headline,ignore.case=TRUE)
    dfAll$wordP[hits] = dfAll$wordP[hits] + 1 + pword(words[i],"Headline")[2]
  }
  words = strsplit(boring.hWords,"\\|")[[1]]
  words[length(words)+1] = "\\|"
  for (i in 1:length(words)) {
    hits = grepl(words[i],dfAll$Headline,ignore.case=TRUE)
    dfAll$wordP[hits] = dfAll$wordP[hits] - 1 + pword(words[i],"Headline")[2]
  }
  words = strsplit(notBoring.aWords,"\\|")[[1]]
  for (i in 1:length(words)) {
    hits = grepl(words[i],dfAll$Headline,ignore.case=TRUE)
    dfAll$wordP[hits] = dfAll$wordP[hits] + 1 + pword(words[i],"Abstract")[2]
  }
  words = strsplit(boring.aWords,"\\|")[[1]]
  for (i in 1:length(words)) {
    hits = grepl(words[i],dfAll$Headline,ignore.case=TRUE)
    dfAll$wordP[hits] = dfAll$wordP[hits] - 1 + pword(words[i],"Abstract")[2]
  }
  
  # impute WordCount
  sWordCount = wordCount(dfAll$Snippet)
  dfAll$WordCount[dfAll$WordCount==0] = sWordCount[dfAll$WordCount==0]
  
  # blog posts per day
  tppd = table(as.Date(dfAll$PubDate))
  dfAll$ppd = tppd[as.character(as.Date(dfAll$PubDate))]
  
  # blog posts per hour
  tDate = as.POSIXlt(dfAll$PubDate)
  tpph = table(tDate$yday*24 + tDate$hour)
  #dfAll$pph = tpph[as.character(tDate$yday*24 + tDate$hour)]
  
  dfAll$Headline = NULL
  dfAll$Abstract = NULL
  dfAll$Snippet = NULL
  dfAll$PubDate = NULL
  
  #training and testing data sets
  dfIV <<- head(dfAll,nrow(dfTrain))
  dfIV$UniqueID <<- NULL
  dfIV$Popular <<- as.factor(dfTrain$Popular)
  dfIVTest <<- tail(dfAll,nrow(dfTest))
  
  # oddly enough gbm does not like Popular as factor
  dfIVgbm <<- dfIV
  dfIVgbm$Popular <<- as.numeric(as.character(dfIVgbm$Popular))
  
  # normalize data for knn since it is distance based
  fac.cols = sapply(dfIV,is.factor)
  dfIVp = dfIV[,!fac.cols]
  pp = preProcess(dfIVp)
  dfIVpp = predict(pp,dfIVp)
  dfIVknn <<- cbind(dfIVpp,dfIV[,fac.cols])
  fac.cols = sapply(dfIVTest,is.factor)
  fac.cols[colnames(dfIVTest)=="UniqueID"] = TRUE
  dfIVp = dfIVTest[,!fac.cols]
  dfIVpp = predict(pp,dfIVp)
  dfIVTestknn <<- cbind(dfIVpp,dfIVTest[,fac.cols]) 
}

wordCount = function(text) {
  cp = Corpus(VectorSource(text))
  cp = tm_map(cp,removePunctuation)
  dtm = DocumentTermMatrix(cp,control=list(wordLengths=c(1,Inf)))
  rowSums(as.matrix(dtm))
}

makeCorpus = function(text,sparsity,prefix) {
  cp = Corpus(VectorSource(text))
  cp = tm_map(cp,tolower)
  cp = tm_map(cp,PlainTextDocument)
  cp = tm_map(cp,removePunctuation)
  cp = tm_map(cp,removeWords,stopwords("english"))
  cp = tm_map(cp,stemDocument)
  dtm = DocumentTermMatrix(cp)
  dtm = removeSparseTerms(dtm,sparsity)
  df = as.data.frame(as.matrix(dtm))
  colnames(df) = paste0(prefix,colnames(df))
  df
}

makeModels = function() {
  require(randomForest)
  require(rpart)
  require(caret)
  require(e1071)
  require(nnet)
  require(gbm)
  #m1 <<- glm(Popular~SectionName+WordCount+hotButton+yawn+tod+dow+isQ+isBoringA+isNotBoringA+isBoringH+isNotBoringH+hWordCount+sWordCount,data=dfIV,family="binomial")
  m2 <<- randomForest(Popular~.,data=dfIV,nodesize=1,ntree=6000,mtry=3)
  m3 <<- rpart(Popular~.,data=dfIV,method="class",cp=0.0003)
  m4 <<- knn3(Popular~.,data=dfIVknn,k=20)
  #m5 <<- svm(Popular~.,data=dfIV,probability=TRUE,gamma=0.5,cost=4)
  m6 <<- nnet(Popular~.,data=dfIVknn,size=3,maxit=300,decay=0.007,trace=FALSE)
  m7 <<- gbm(Popular~.,data=dfIVgbm,n.trees=15000,interaction.depth=3,shrinkage=0.001,distribution="bernoulli")
}

makePredictions = function() {
  #p1 <<- predict(m1, type="response")
  p2 <<- predict(m2, type="prob")[,2]
  p3 <<- predict(m3)[,2]
  p4 <<- predict(m4,dfIVknn,type="prob")[,2]
  #p5 <<- attr(predict(m5,dfIV,probability=TRUE),"probabilities")[,"1"]
  p6 <<- predict(m6,dfIVknn,type="raw")
  p7 <<- predict(m7,n.trees=15000,type="response")
  ps = (0.7*p2+0.3*p7)
  print(conf.matrix(dfIV$Popular,ps,0.5)$metrics["auc"])
  print(c(#glm=conf.matrix(dfIV$Popular,p1,0.5)$metrics["auc"],
    rf=conf.matrix(dfIV$Popular,p2,0.5)$metrics["auc"],
    cart=conf.matrix(dfIV$Popular,p3,0.5)$metrics["auc"],
    knn=conf.matrix(dfIV$Popular,p4,0.5)$metrics["auc"],
    #svm=conf.matrix(dfIV$Popular,p5,0.5)$metrics["auc"],
    nnet=conf.matrix(dfIV$Popular,p6,0.5)$metrics["auc"],
    gbm=conf.matrix(dfIVgbm$Popular,p7,0.5)$metrics["auc"]))
}

makeSubmission = function() {
  #t1 <<- predict(m1, newdata=dfIVTest, type="response")
  t2 <<- predict(m2, newdata=dfIVTest,type="prob")[,2]
  #t3 <<- predict(m3, newdata=dfIVTest)[,2]
  #t4 <<- predict(m4, dfIVTestknn,type="prob")[,2]
  #t5 <<- attr(predict(m5,dfIVTest,probability=TRUE),"probabilities")[,"1"]
  #t6 <<- predict(m6,dfIVTestknn,type="raw")
  t7 <<- predict(m7,newdata=dfIVTest,n.trees=15000,type="response")
  ts2 <<- (0.7*t2+0.3*t7)
  submission = data.frame(UniqueID=dfIVTest$UniqueID, Probability1=ts2)
  write.csv(submission, "~/Documents/Kaggle/submission.csv", row.names=FALSE)
}

# train AUC 0.9505891
# test AUC  0.94036
# CV AUC    0.9465515

# fitControl <- trainControl(method="cv",number=5,classProbs = TRUE, summaryFunction = twoClassSummary)
# tuneGrid = expand.grid(interaction.depth = 3,n.trees = 10000,shrinkage = c(0.002,0.001,0.0008))
# tm = train(Popular~.,dfIV,method="gbm",metric="ROC",trControl=fitControl,tuneGrid=tuneGrid)

# kfold(dfIV,glm,pargs=list(type="response"),family="binomial")
# kfold(dfIVknn,nnet,pargs=list(type="raw"),size=3,maxit=300,decay=0.007,trace=FALSE)
# kfold(dfIV,rpart,method="class",cp=0.0003)
# kfold(dfIVknn,knn3,pargs=list(type="prob"),k=20)
# kfold(dfIV,randomForest,pargs=list(type="prob"),nodesize=1,ntree=2000,mtry=3)
# kfold(dfIVgbm,gbm,pargs=list(n.trees=15000,type="response"),n.trees=15000,interaction.depth=3,shrinkage=0.001,distribution="bernoulli")

ekfold = function() {
  require(caret)
  t = Sys.time()
  k = 10
  metrics = list(k)
  means = numeric(5)
  folds = createFolds(dfIV$Popular,k=k,list=TRUE,returnTrain=TRUE)
  for (i in 1:length(folds)) {
    dfT = dfIV[folds[[i]],]
    dfP = dfIV[folds[[i]]*-1,]
    dfTgbm = dfIVgbm[folds[[i]],]
    dfPgbm = dfIVgbm[folds[[i]]*-1,]
    dfTknn = dfIVknn[folds[[i]],]
    dfPknn = dfIVknn[folds[[i]]*-1,]
    m2 = randomForest(Popular~.,data=dfT,nodesize=1,ntree=6000,mtry=3)
    #m3 = rpart(Popular~.,data=dfT,method="class",cp=0.0003)
    #m4 = knn3(Popular~.,data=dfTknn,k=20)
    #m6 = nnet(Popular~.,data=dfTknn,size=3,maxit=300,decay=0.007,trace=FALSE)
    m7 = gbm(Popular~.,data=dfTgbm,n.trees=15000,interaction.depth=3,shrinkage=0.001,distribution="bernoulli")
    #p1 = predict(m1, type="response")
    p2 = predict(m2, newdata=dfP,type="prob")[,2]
    #p3 = predict(m3,newdata=dfP)[,2]
    #p4 = predict(m4,dfPknn,type="prob")[,2]
    #p6 = predict(m6,newdata=dfPknn,type="raw")
    p7 = predict(m7,newdata=dfPgbm,n.trees=15000,type="response")
    ps = (0.7*p2+0.3*p7)
    mk = conf.matrix(dfP$Popular,ps,0.5)$metrics
    #print(mk)
    metrics[[i]] = mk
  }
  for (i in 1:length(metrics)) {
    means = means + metrics[[i]]
  }
  means = means / k
  names(means) = names(metrics[[1]])
  print(Sys.time() - t)
  list(means=means)
}

# Parameters:
#   data  - data frame containing your training data
#   model - the function to call to make the model (e.g. rpart, gbm, knn3 etc.)
#   pargs - a list of arguments for the predict function
#   ...   - parameters for the model
# Examples:
#   kfold(TrainData,randomForest,pargs=list(type="prob"),nodesize=1,ntree=500)
#   kfold(TrainData,rpart,method="class",cp=0.01)
kfold = function(data,model,pargs=NULL,...) {
  require(caret)
  t = Sys.time()
  # k is the number of folds, 5 or 10 are common choices
  k = 10
  metrics = list(k)
  means = numeric(5)
  folds = createFolds(data$Popular,k=k,list=TRUE,returnTrain=TRUE)
  for (i in 1:length(folds)) {
    dfT = data[folds[[i]],]
    dfP = data[folds[[i]]*-1,]
    # change the formula (Popular~.) in the line below if you need to
    m = model(Popular~.,data=dfT,...)
    p = do.call(predict,c(list(object=m,newdata=dfP),pargs))
    if (!is.vector(p))
      if (ncol(p)>1)
        p = p[,2]
    mk = conf.matrix(dfP$Popular,p,0.5)$metrics
    # remove the # in front of the print statement if you want to see 
    # data for each iteration
    #print(mk)
    metrics[[i]] = mk
  }
  for (i in 1:length(metrics)) {
    means = means + metrics[[i]]
  }
  means = means / k
  names(means) = names(metrics[[1]])
  delta = Sys.time() - t
  cat(paste(as.integer(delta),attr(delta,"units"),"\n"))
  list(means=means)
}

conf.matrix = function (outcomes,predictions,cutoff) {
  require(ROCR)
  if (class(predictions) == "factor") {
    auc=NA
    t = table(outcomes,predictions)
  }
  else {
    auc = performance(prediction(predictions,outcomes),"auc")@y.values[[1]]
    t = table(outcomes,predictions=predictions >= cutoff)
    if (cutoff > max(predictions)) {
      t = cbind(t,"TRUE"=c(0,0))
    }
    else if (cutoff < min(predictions)) {
      t = cbind("FALSE"=c(0,0),t)
    }
  }
  acc = (t[1,1] + t[2,2]) / length(predictions)
  sen = t[2,2] / (t[2,1] + t[2,2])
  spec = t[1,1] / (t[1,1] + t[1,2])
  baseline.accuracy = max(t[1,1]+t[1,2],t[2,1]+t[2,2])/length(predictions)
  list(confusion.matrix=t,
       metrics=c(sensitivity=sen,specificity=spec,accuracy=acc,baseline.acc=baseline.accuracy,auc=auc))
}

pword = function(w,s,show=FALSE) {
  i = grep(w,dfTrain[,s],ignore.case=TRUE)
  m = mean(dfIVgbm[i,"Popular"])
  t = grep(w,dfTest[,s],ignore.case=TRUE)
  if (show) {
    print(dfTest[t,s])
  }
  c(length(i),m,length(t))
}

wcount = function(n) {
  a = length(dfIVgbm$Popular[wc==n])
  b = mean(dfIVgbm$Popular[wc==n]);
  c = length(dfIVTest$tod[wct==n])
  print(paste(a,b,c))
}

makeMatrix = function(text,sparsity) {
  cp = Corpus(VectorSource(text))
  cp = tm_map(cp,tolower)
  cp = tm_map(cp,PlainTextDocument)
  cp = tm_map(cp,removePunctuation)
  cp = tm_map(cp,removeWords,stopwords("english"))
  dtm = DocumentTermMatrix(cp)
  dtm = removeSparseTerms(dtm,sparsity)
  dtm
}

# generate table of frequent words in the test data
#   word - word found in test data
#   nTrain - how manys times word appears in training data
#   Pop - average popularity of posts containing this word
#   nTest - how manys times word appears in test data
sw = function(column,sparsity) {
  dtm = makeMatrix(dfTest[,column],sparsity)
  n = nTerms(dtm)
  wl = data.frame(word=character(n),nTrain=numeric(n),Pop=numeric(n),nTest=numeric(n),stringsAsFactors=FALSE)
  for (i in 1:n) {
    wl$word[i] = Terms(dtm)[i]
    t = grep(Terms(dtm)[i],dfTrain[,column],ignore.case=TRUE)
    wl$nTrain[i] = length(t)
    wl$Pop[i] = mean(dfIVgbm[t,"Popular"])
    t = grep(Terms(dtm)[i],dfTest[,column],ignore.case=TRUE)
    wl$nTest[i] = length(t)
  }
  wl[order(-wl$Pop),]
}
plots = function() {
  plot(tapply(dfIVgbm$Popular,dfIV$sWordCount,mean))
  plot(tapply(dfIVgbm$Popular,dfIV$hWordCount,mean))
  t = tapply(dfIVgbm$Popular,as.Date(dfTrain$PubDate),mean)
  plot(as.Date(names(t)),t,type="l",ylim=c(0,1))
  
  t = tapply(dfIVgbm$Popular,as.Date(dfTrain$PubDate),mean)
  t2 = table(as.Date(dfTrain$PubDate))
  tdf2 = cbind(names(t),t,t2)
  tdf2 = as.data.frame(tdf)
  tdf2$V1 = as.Date(tdf$V1)
  tdf2$t = as.numeric(as.character(tdf$t))
  tdf2$t2 = as.numeric(as.character(tdf$t2))
  lines(tdf2$V1,tdf2$t2/max(tdf$t2),type="l",col="red")
}
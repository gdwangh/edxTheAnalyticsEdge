library(RWeka)
library(tm)
setwd("D:/backupppp/Downloads")
nyTrain = read.csv("nyTrain.csv" , stringsAsFactors = F)
nyTest = read.csv("nyTest.csv" , stringsAsFactors = F)
split = c(rep(1,nrow(nyTrain)) , rep(0 , nrow(nyTest)))
setwd("C:/Users/Pulkit/Desktop/")

ny = rbind(nyTrain[,c(-4,-5,-6,-9)] , nyTest[,c(-4,-5,-6)])
text = rbind(nyTrain[,c(4,5,6)] , nyTest[,c(4,5,6)]) 
library(car)
popular = recode(nyTrain[,c(9)] , " '1' = 'Yes' ; else = 'No' ")
popular = as.factor(x = popular)

PreProcess = function(corpus){
  x = Corpus(VectorSource(corpus))
  x = tm_map(x , tolower)
  x = tm_map(x , PlainTextDocument)
  x = tm_map(x , removePunctuation)
  x = tm_map(x , removeNumbers)
  x = tm_map(x , removeWords , stopwords("english"))
  x = tm_map(x , stemDocument)
  x
}
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
corpus_list = lapply(list(text[,1], text[,2],text[,3]) , PreProcess)

dtm_list <- lapply(corpus_list,function(a)DocumentTermMatrix(a, control = list(tokenize = BigramTokenizer)))
dtm_list_backup = dtm_list

duplicated = colnames(dtm_list[[3]]) %in% colnames(dtm_list[[1]])
duplicated = which(duplicated == T)
dtm_list[[3]] = dtm_list[[3]][,-duplicated]

col = list("H" , "S" , "A")
for(i in 1:3){
  colnames(dtm_list[[i]]) = paste(col[[i]],colnames(dtm_list[[i]]),sep="_")
  colnames(dtm_list[[i]]) = make.names(colnames(dtm_list[[i]]) , unique = T)
}

dtm_h = dtm_list[[1]]
dtm_s = dtm_list[[2]]
dtm_a = dtm_list[[3]]

s_h = removeSparseTerms(dtm_h , sparse = .9992)      #182
s_a = removeSparseTerms(dtm_a , sparse = .99783)      #086

#...............................................................................
##     ACCURACY
acc = function(o , p){
  require(ROCR)
  if(class(p) == "factor") 
    return('NA')  
  else 
    z = table(o , p > 0.5)
  z = (z[1]+z[4])/sum(z) 
  auc = as.numeric(performance(prediction(p , o) , "auc")@y.values)
  print(format(paste('auc = ' , auc)))
  print(format(paste('Acc = ' , z))) 
}

#....................................................................................
## DATE PROCESSING
PubDate = ny$PubDate
PubDate = strptime(PubDate , format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
ny$WeekDay = weekdays(PubDate)
ny$Hour = PubDate$hour
ny$WeekDay = as.factor(ny$WeekDay)
ny$Hour = as.factor(ny$Hour)
ny$PubDate = NULL

for(i in 1:3){
  ny[,i] = recode(ny[,i] , " '' = paste0('M' ,  i);")
}

for(i in 1:3){
  ny[,i] = as.factor(ny[,i])
}

library(caret)
wc = ny$WordCount
ny$WordCount = log(wc + 1)
ny = ny[,c(5, 1:4 , 6 , 7)]
ny$WordCount = (ny$WordCount-mean(ny$WordCount))/sd(ny$WordCount)

## _______________________________________________________

#################################################################

ny_all = cbind(ny , as.data.frame.matrix(s_h) , as.data.frame.matrix(s_a))

# NY DATA FRAME IS CLEANED 

NYTRAIN = ny_all[split == 1,]
NYTEST = ny_all[split == 0 , ]
NYTRAIN$Popular = as.factor(nyTrain$Popular)

# TEST AND TRAIN IS PREPARED
# ......................................................................................

# GLM MODEL __________________________________________________________________

NYTRAIN$Popular = as.factor(nyTrain$Popular)
GLM = glm(Popular~.-UniqueID , data = NYTRAIN , family = "binomial"  )
pred_glm = predict(GLM , NYTEST , type = "response")
acc(NYTRAIN$Popular ,  GLM$fitted.values)


## GBM ----------------------------------------------------------------------
library(gbm)
library(doParallel)
library(foreach)
library(caret)
cl = makeCluster(4)
registerDoParallel(cl)
NYTRAIN$Popular = as.factor(popular)
set.seed(144) 
control = trainControl(method = "cv" , allowParallel = T , number = 8  )
grid = expand.grid(n.trees = c(400,450,500) , interaction.depth = c(7,8,9,10) , shrinkage = c(0.06,0.08,0.04))
cv.gbm = train(form = Popular~.-UniqueID , data = NYTRAIN , method = "gbm", maximize = T , metric = "Accuracy" , trControl = control , tuneGrid = grid  )
pred_gbm = predict.train(object = cv.gbm , newdata = NYTEST , type = "prob" )[,2]                                                                                                                                                                                                     
acc(NYTRAIN$Popular , predict.train(object = cv.gbm , newdata = NYTRAIN , type = "prob" )[,2]   )


# output file ......................................................................................
predF =  pred_glm*0.4 + pred_gbm*0.6 
output = data.frame(UniqueID = NYTEST$UniqueID , Probability1 = predF )
colnames(output) = c("UniqueID","Probability1")
setwd("C:/Users/Pulkit/Desktop/")
write.csv( output , "test.csv" , row.names = F)








  
  
  
  
  
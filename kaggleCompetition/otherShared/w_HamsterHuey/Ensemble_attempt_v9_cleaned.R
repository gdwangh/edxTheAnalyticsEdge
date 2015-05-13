# ==============================================================================
# Kaggle Competition - Analytics Edge, Spring 2015
#
# Author: Sudeep Mandal / HamsterHuey
#
# Notes: This script performs model stacking to create an Ensemble. GLMNET model
# was used to select down features which were then used to train component models
# for the Ensemble. Finally GBM and GLMNET ensembles were created. RWeka package
# used to create bag of words consisting of 1-, 2- and 3-grams.
#
# GBM Ensemble would have scored 150 on private LB. GLMNET ensemble didn't do as
# well
#
# A word of caution - You do not want to train GLMNET on entire Training dataset
# for feature selection as I have done here. This pollutes CV results making them
# meaningless. Should have created separate data split to train GLMNET on for
# feature selection. In general, a lot of the Regexp features were designed after
# analyzing the entire training set. This was a bad idea and resulted in overly
# optimistic AUC values even when cross-validating.
#
# Reference: http://amunategui.github.io/blending-models/
# ===============================================================================

# Read Data
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(tm)
library(RWeka)

accuracy = function(vec1, vec2){
  t = table(vec1, vec2)
  (t[1,1] + t[2,2])/length(vec1)
}

# Function to do all the work of creating the final DataFrames
parseTextFields = function(NewsTrain, NewsTest, sparseValue){
  require(tm)
  
  corpusH = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
  corpusH = tm_map(corpusH, tolower)
  corpusH = tm_map(corpusH, PlainTextDocument)
  corpusH = tm_map(corpusH, removePunctuation)
  corpusH = tm_map(corpusH, removeWords, stopwords('english'))
  corpusH = tm_map(corpusH, stemDocument)
  ngramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3)) # Select 1, 2 or 3-grams
  dtmH = DocumentTermMatrix(corpusH, control=list(tokenize=ngramTokenizer))
  dtmH = removeSparseTerms(dtmH, sparseValue)
  dtmH = as.data.frame(as.matrix(dtmH))
  
  corpusA = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
  corpusA = tm_map(corpusA, tolower)
  corpusA = tm_map(corpusA, PlainTextDocument)
  corpusA = tm_map(corpusA, removePunctuation)
  corpusA = tm_map(corpusA, removeWords, stopwords('english'))
  corpusA = tm_map(corpusA, stemDocument)
  ngramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3)) # Select 1, 2 or 3-grams
  dtmA = DocumentTermMatrix(corpusA, control=list(tokenize=ngramTokenizer))
  dtmA = removeSparseTerms(dtmA, sparseValue)
  dtmA = as.data.frame(as.matrix(dtmA))
  
  colnames(dtmH) = make.names(colnames(dtmH))
  colnames(dtmH) = paste0("H", colnames(dtmH))
  
  colnames(dtmA) = make.names(colnames(dtmA))
  colnames(dtmA) = paste0("A", colnames(dtmA))
  
  # Combine all DTMs into one DataFrame
  dtmFinal = cbind(dtmH, dtmA)
  dtm_names = names(dtmFinal)
  
  dtmFinal$WordCount = c(log(50+NewsTrain$WordCount), log(50+NewsTest$WordCount))
  dtmFinal$NewsDesk = c(NewsTrain$NewsDesk, NewsTest$NewsDesk)
  dtmFinal$NewsDesk = as.factor(dtmFinal$NewsDesk)
  dtmFinal$SectionName = c(NewsTrain$SectionName, NewsTest$SectionName)
  dtmFinal$SectionName = as.factor(dtmFinal$SectionName)
  dtmFinal$SubsectionName = c(NewsTrain$SubsectionName, NewsTest$SubsectionName)
  dtmFinal$SubsectionName = as.factor(dtmFinal$SubsectionName)
  
  # Add new feature regarding Questions in Headline or Abstract
  dtmFinal$QuestionH = as.factor(c(grepl('^.+\\?.*$', NewsTrain$Headline), grepl('^.+\\?.*$', NewsTest$Headline)))
  dtmFinal$QuestionA = as.factor(c(grepl('^.+\\?.*$', NewsTrain$Abstract), grepl('^.+\\?.*$', NewsTest$Abstract)))
  
  # Does Headline start with Question Words?
  dtmFinal$QWord = as.factor(c(grepl('^(How|Why|Should|What|Can|Is|When|Was) .*$', NewsTrain$Headline), grepl('^(How|Why|Should|Is|What|Can|When|Was) .*$', NewsTest$Headline)))
  
  # First word ending in -ing?
  dtmFinal$EndIng = as.factor(c(grepl('^[a-zA-Z]*ing ', NewsTrain$Headline), grepl('^[a-zA-Z]*ing ', NewsTest$Headline)))
  
  # Add Headline length
  # dtmFinal$HeadlineLength = sapply(regmatches(c(NewsTrain$Headline, NewsTest$Headline), gregexpr(" ", c(NewsTrain$Headline, NewsTest$Headline))), length) + 1
  dtmFinal$HeadlineLength = nchar(c(NewsTrain$Headline, NewsTest$Headline)) # Initially was counting words. Counting total characters seemed to give a slighlty better result
  
  # Zero Wordcount articles should have Popular = 0 (from analyzing Training set)
  dtmFinal$zeroWordCount = as.factor(c(NewsTrain$WordCount==0, NewsTest$WordCount==0))
  
  # Add colon???
  dtmFinal$Colon = as.factor(c(grepl('^.+:.+$', NewsTrain$Headline), grepl('^.+:.+$', NewsTest$Headline)))
  
  dtmFinal$historyColon = as.factor(c(grepl('^([0-9]+:).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^([0-9]+:).*$', NewsTest$Headline, ignore.case=TRUE)))
  dtmFinal$pipe = as.factor(c(grepl('^.*(\\|).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(\\|).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  # To convert the date/time to something R will understand, you can use the following commands:
  NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
  NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
  
  # Incorporate HOUR into model
  # **********************************
  dtmFinal$Weekday = as.factor(c(NewsTrain$PubDate$wday, NewsTest$PubDate$wday))
  dtmFinal$Hour = as.factor(c(NewsTrain$PubDate$hour, NewsTest$PubDate$hour))
  
  FinalTrain = head(dtmFinal, nrow(NewsTrain))
  FinalTest = tail(dtmFinal, nrow(NewsTest))
  FinalTrain$Popular = as.factor(NewsTrain$Popular)	
  
  return(list('FTrain'=FinalTrain, 'FTest'=FinalTest, 'dtmH'=dtmH, 'dtmA'=dtmA, dtm_names, 'dtmFinal'=dtmFinal))
}

t = parseTextFields(NewsTrain, NewsTest, 0.998)
FinalTrain = t[1]$FTrain
FinalTest = t[2]$FTest
dtmFinal = t[6]$dtmFinal
noDtmVars = names(FinalTrain)[!(names(FinalTrain) %in% t[[5]])]
noDtmVars = noDtmVars[noDtmVars != 'Popular']

# =================================================
# Begin fitting models
# =================================================
library(doParallel)
registerDoParallel(cores=4)
library(caret)

# Outcome should be factor with valid R variable names so 0,1 won't work
Yvals = ifelse(FinalTrain$Popular == 1, 'yes', 'no')
Yvals = as.factor(Yvals)

# Set up FinalTrainNoDtm with only non-DTM variables
FinalTrainNoDtm = FinalTrain[noDtmVars]
FinalTestNoDtm = FinalTest[noDtmVars]

# Set up FinalTrainDtm and FinalTestDtm with only DTM variables
FinalTrainDtm = FinalTrain
FinalTestDtm = FinalTest
FinalTrainDtm = FinalTrain[, -which(names(FinalTrain) %in% noDtmVars)]  # Remove bag of words features
FinalTestDtm = FinalTest[, -which(names(FinalTest) %in% noDtmVars)]  # Remove bag of words features
FinalTrain$Popular = NULL
FinalTestDtm$Popular=NULL


# ===========================================
# Feature Selection using GLMNET
# ===========================================
require(caret)
library(glmnet)

# GLMNET needs family=binomial for classification. Also, only accepts model.matrix, not x, y values or dataframes
FinalGlmNet = cv.glmnet(model.matrix(~ ., FinalTrain), Yvals, family='binomial', type.measure='auc', parallel=TRUE, intercept=TRUE, alpha=1)

# Analyze model and extract Important variables
plot(FinalGlmNet)
FinalGlmNet$lambda.min  # 0.004815186
coeffs = coef(FinalGlmNet, s = "lambda.1se")  # Could be lambda.min for larger number of features and absolute best AUC performance
coeffs = as.data.frame(as.table(as.matrix(coeffs)))
filtered_coeffs = coeffs[abs(coeffs$Freq) > 0,]  # Change 0 to higher number to filter out very low importance features
filtered_coeffs = filtered_coeffs[order(abs(filtered_coeffs$Freq), decreasing = TRUE),]
View(filtered_coeffs)  # NOTE: View is an RStudio specific command

# Accuracy on training set - Meaningless metric. Just for kicks.
accuracy(Yvals, predict(FinalGlmNet, newx=model.matrix(~ ., FinalTrain) , type='class'))

# Select out features based on GLMNET Lasso model and create new Training and Test data
dtm_features = names(FinalTrain)[names(FinalTrainDtm) %in% filtered_coeffs$Var1]
filtTrain = cbind(FinalTrainNoDtm, FinalTrainDtm[,dtm_features])
filtTest = cbind(FinalTestNoDtm, FinalTestDtm[,dtm_features])

# ======================================
# PreProcess Data and Split into 3 parts
# ======================================
set.seed(825)
# set.seed(420)
# sample_indx = sample(length(Yvals), replace=FALSE)

# Did not randomly split data. Was going by the assumption that trends in data evolve
# over time. In such situations, one recommendation is to split train and test data
# in chronological sequence. Train data is earliest set of data and then CV'd against
# future dataset for hyperparameter tuning. 
# Not sure if this approach was actually useful
split = floor(length(Yvals)/2)
ensembleYvals = Yvals[(split+1):length(Yvals)]
ensembleFiltTrain = filtTrain[(split+1):length(Yvals),]
ensembleNoDtm = FinalTrainNoDtm[(split+1):length(Yvals),]
ensembleDtm = FinalTrainDtm[(split+1):length(Yvals),]

blenderYvals = Yvals[1:split]
blenderFiltTrain = filtTrain[1:split,]
blenderNoDtm = FinalTrainNoDtm[1:split,]
blenderDtm = FinalTrainDtm[1:split,]

# sample_indx = createDataPartition(Yvals, p=0.5, list=FALSE)
# Yvals2 = Yvals[sample_indx]
# filtTrain2 = filtTrain[sample_indx,]
# FinalTrainNoDtm2 = FinalTrainNoDtm[sample_indx,]
# FinalTrainDtm2 = FinalTrainDtm[sample_indx,]

# ensembleYvals = Yvals[sample_indx]
# ensembleFiltTrain = filtTrain[sample_indx,]
# ensembleNoDtm = FinalTrainNoDtm[sample_indx,]
# ensembleDtm = FinalTrainDtm[sample_indx,]

# blenderYvals = Yvals[-sample_indx]
# blenderFiltTrain = filtTrain[-sample_indx,]
# blenderNoDtm = FinalTrainNoDtm[-sample_indx,]
# blenderDtm = FinalTrainDtm[-sample_indx,]


# ===================================================
# Train Models with selected Features on BlenderData
# ===================================================
fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
# fitControl <- trainControl(method = "repeatedcv",
# number = 5,
# repeats = 1,
# # Estimate class probabilities
# classProbs = TRUE,
# # Evaluate performance using 
# # the following function
# summaryFunction = twoClassSummary)
RFGrid = expand.grid(mtry = c(23, 24))

## ============= RF ====================
set.seed(825)
FinalRFCaretDtmNs7 = train(blenderFiltTrain, blenderYvals, method='rf', trControl = fitControl, tuneGrid = RFGrid, metric='ROC', ntree=2000, nodesize=7)
# ROC        Sens       Spec       ROC SD      Sens SD     Spec SD   
# 0.9392169  0.9604086  0.6412773  0.01037878  0.01352627  0.04798717

# Tuning parameter 'mtry' was held constant at a value of 11


# ============= RF NoDtm ===================
fitControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalRFCaretNoDtmNs7 = train(blenderNoDtm, blenderYvals, method='rf', trControl = fitControl, tuneGrid = expand.grid(mtry = c(3, 4, 5, 6)), metric='ROC', ntree=2000, nodesize=7)
# Resampling results across tuning parameters:

# mtry  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD   
# 3     0.9379767  0.9595620  0.6333916  0.01726985  0.01347193  0.07316047
# 4     0.9382010  0.9580963  0.6482530  0.01724141  0.01394904  0.06407702
# 5     0.9379305  0.9555277  0.6588050  0.01770172  0.01386001  0.06505418

# ROC was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 4. 


# ============= GBM ====================
fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(3, 5, 7), n.trees = c(500, 1000, 1500, 2000, 3000), shrinkage = c(0.004, 0.01))
FinalGBMCaretDtm = train(blenderFiltTrain, blenderYvals, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid) # 
# shrinkage  interaction.depth  n.trees  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 0.004      3                   500     0.9248429  0.9648110  0.5744202  0.010096240  0.011978951  0.05863281
# 0.004      3                  1000     0.9314930  0.9629775  0.5873659  0.009233244  0.009648544  0.06383134
# 0.004      3                  1500     0.9337667  0.9596787  0.5965905  0.009664144  0.010123402  0.05427852
# 0.004      3                  2000     0.9349766  0.9589461  0.6059017  0.009412063  0.006797780  0.06120467
# 0.004      3                  3000     0.9356528  0.9593111  0.6207511  0.010501378  0.005997099  0.05375683
# 0.004      5                   500     0.9286421  0.9662782  0.5613707  0.010035437  0.012196490  0.05350361
# 0.004      5                  1000     0.9344009  0.9618772  0.5929214  0.010245304  0.007929514  0.06655791
# 0.004      5                  1500     0.9360297  0.9585771  0.6151783  0.010858978  0.007954567  0.04549461
# 0.004      5                  2000     0.9369436  0.9563793  0.6263067  0.011536760  0.008530977  0.05354776
# 0.004      5                  3000     0.9367463  0.9582115  0.6430426  0.012714692  0.007259456  0.05272241
# 0.004      7                   500     0.9309472  0.9673764  0.5502077  0.010543216  0.011109365  0.04442348
# 0.004      7                  1000     0.9354841  0.9611433  0.5947040  0.011298703  0.008545062  0.05069115
# 0.004      7                  1500     0.9367014  0.9585792  0.6170647  0.012559167  0.008644916  0.05221022
# 0.004      7                  2000     0.9371614  0.9567463  0.6263240  0.013351856  0.007274913  0.05163894 ****
# 0.004      7                  3000     0.9368899  0.9582115  0.6374697  0.014352069  0.006529663  0.04129973
# 0.010      3                   500     0.9330331  0.9604107  0.5947733  0.009743298  0.009926354  0.06471768
# 0.010      3                  1000     0.9354744  0.9589455  0.6207684  0.010635150  0.007164463  0.05403416
# 0.010      3                  1500     0.9359542  0.9589455  0.6374524  0.012402627  0.006549370  0.04793291
# 0.010      3                  2000     0.9359815  0.9589441  0.6374351  0.013473350  0.005590168  0.04625788
# 0.010      3                  3000     0.9355627  0.9585778  0.6393216  0.014268624  0.007398651  0.04892978
# 0.010      5                   500     0.9353630  0.9600450  0.6114919  0.011063029  0.008824634  0.04926535
# 0.010      5                  1000     0.9370943  0.9563800  0.6299931  0.012472008  0.008910466  0.04915669
# 0.010      5                  1500     0.9367494  0.9571133  0.6467117  0.014090863  0.007826431  0.04819508
# 0.010      5                  2000     0.9360176  0.9593111  0.6467117  0.015112795  0.005993502  0.04774829
# 0.010      5                  3000     0.9345705  0.9582102  0.6485981  0.015929948  0.006275993  0.04725759
# 0.010      7                   500     0.9366442  0.9589468  0.6096227  0.011565137  0.011979830  0.04908329
# 0.010      7                  1000     0.9367769  0.9571119  0.6374524  0.013889815  0.007167011  0.04602224
# 0.010      7                  1500     0.9363877  0.9582102  0.6411734  0.015738852  0.006005885  0.04575040
# 0.010      7                  2000     0.9355904  0.9582108  0.6430426  0.015969744  0.006663838  0.03616195
# 0.010      7                  3000     0.9336171  0.9582115  0.6449463  0.015714363  0.007259456  0.03486904

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 2000, interaction.depth = 7 and shrinkage = 0.004.

fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(2, 3, 4, 5), n.trees = c(500, 1000, 1500, 2000, 3000), shrinkage = c(0.004, 0.01))
FinalGBMCaretNoDtm = train(blenderNoDtm, blenderYvals, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid) #
# shrinkage  interaction.depth  n.trees  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 0.004      2                   500     0.9200766  0.9670088  0.5706992  0.008750921  0.010930532  0.05608091
# 0.004      2                  1000     0.9278496  0.9633458  0.5854794  0.006352815  0.011590934  0.06981955
# 0.004      2                  1500     0.9306080  0.9626118  0.5947906  0.007365414  0.010415683  0.07094408
# 0.004      2                  2000     0.9321537  0.9596781  0.5984943  0.008180852  0.009787546  0.05839081
# 0.004      2                  3000     0.9330008  0.9589468  0.6096400  0.009080344  0.009570636  0.04813921
# 0.004      3                   500     0.9254113  0.9651779  0.5744202  0.008983235  0.012634022  0.05863281
# 0.004      3                  1000     0.9316954  0.9626112  0.5966597  0.008627509  0.010005730  0.07151109
# 0.004      3                  1500     0.9337150  0.9589455  0.6077882  0.008999690  0.008555391  0.05947818
# 0.004      3                  2000     0.9344137  0.9593124  0.6133437  0.009464404  0.009288500  0.04619892
# 0.004      3                  3000     0.9349701  0.9596794  0.6337833  0.010598535  0.007873582  0.03969011
# 0.004      4                   500     0.9273814  0.9651786  0.5706646  0.010348031  0.012087280  0.05351068
# 0.004      4                  1000     0.9331655  0.9611439  0.5911215  0.009249068  0.009910901  0.06755806
# 0.004      4                  1500     0.9352021  0.9593104  0.6133437  0.009261591  0.009655071  0.05476978
# 0.004      4                  2000     0.9357891  0.9600457  0.6356698  0.010056977  0.009640571  0.04540038
# 0.004      4                  3000     0.9360481  0.9604127  0.6449637  0.011469316  0.009567314  0.03807061
# 0.004      5                   500     0.9288494  0.9659119  0.5650917  0.009991908  0.012671741  0.05222786
# 0.004      5                  1000     0.9341910  0.9604120  0.5947733  0.010099379  0.009487929  0.05574809
# 0.004      5                  1500     0.9356147  0.9596794  0.6226895  0.010540201  0.010197298  0.04798080
# 0.004      5                  2000     0.9362382  0.9585805  0.6375736  0.011155692  0.010402322  0.04285090
# 0.004      5                  3000     0.9361827  0.9589455  0.6412426  0.012787735  0.009392971  0.03347469
# 0.010      2                   500     0.9297054  0.9637114  0.5910869  0.006988915  0.010952247  0.06819431
# 0.010      2                  1000     0.9327929  0.9593124  0.6040672  0.008642646  0.010399995  0.05225309
# 0.010      2                  1500     0.9334494  0.9589468  0.6152302  0.009050791  0.006795377  0.03761007
# 0.010      2                  2000     0.9337900  0.9593118  0.6300796  0.010802652  0.008027337  0.03600763
# 0.010      2                  3000     0.9336194  0.9600444  0.6319142  0.012403967  0.009639358  0.03985864
# 0.010      3                   500     0.9331892  0.9611439  0.6003808  0.009171732  0.008737407  0.06330614
# 0.010      3                  1000     0.9354703  0.9596787  0.6282105  0.009508169  0.010683800  0.04301069
# 0.010      3                  1500     0.9353232  0.9582129  0.6338179  0.011000477  0.009636967  0.03989864
# 0.010      3                  2000     0.9354691  0.9600457  0.6412426  0.012054538  0.009007697  0.02938288
# 0.010      3                  3000     0.9355922  0.9593124  0.6393735  0.013205710  0.010145196  0.04458834
# 0.010      4                   500     0.9344602  0.9596781  0.6078055  0.008999695  0.010770733  0.05892974
# 0.010      4                  1000     0.9364217  0.9593118  0.6375216  0.010742036  0.009286643  0.04713645 ****
# 0.010      4                  1500     0.9362215  0.9578466  0.6375043  0.012199956  0.011286939  0.03928400
# 0.010      4                  2000     0.9357519  0.9589468  0.6375216  0.013112313  0.011255342  0.03560200
# 0.010      4                  3000     0.9351363  0.9574803  0.6449463  0.013796480  0.009188700  0.04553228
# 0.010      5                   500     0.9344212  0.9607783  0.6115092  0.010726354  0.010001765  0.04904739
# 0.010      5                  1000     0.9359583  0.9585785  0.6449637  0.012599413  0.008653179  0.03813399
# 0.010      5                  1500     0.9359188  0.9585785  0.6468155  0.014207098  0.008354069  0.03798979
# 0.010      5                  2000     0.9356429  0.9578452  0.6430945  0.015048732  0.007983829  0.04829818
# 0.010      5                  3000     0.9336520  0.9567450  0.6431464  0.015534696  0.008844228  0.04350878

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 1000, interaction.depth = 4 and shrinkage = 0.01. 

# ============ NNET ===================
fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalNNETCaretDtm = train(blenderFiltTrain, blenderYvals, method = "nnet", trControl = fitControl, tuneGrid = expand.grid(size = c(3, 5, 10, 15), decay = c(1e-1, 6e-1, 8e-1, 1.4, 2)), metric='ROC', MaxNWts = 5000)
# size  decay  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 3    0.1    0.9324157  0.9512431  0.6783489  0.011842116  0.017052946  0.05875560
# 3    0.6    0.9438772  0.9571086  0.6523191  0.010340533  0.013019338  0.06399881 ****
# 3    0.8    0.9428204  0.9607763  0.6579266  0.010112462  0.012414050  0.05040521
# 3    1.4    0.9401588  0.9633431  0.6393735  0.009802454  0.010455677  0.05430380
# 3    2.0    0.9372822  0.9655402  0.6115092  0.009552820  0.010892075  0.03880001
# 5    0.1    0.9288309  0.9439124  0.6839391  0.014231426  0.011590247  0.05043275
# 5    0.6    0.9408279  0.9600457  0.6449117  0.012692932  0.009644404  0.05651399
# 5    0.8    0.9429685  0.9626105  0.6542402  0.012044006  0.011851914  0.05630820
# 5    1.4    0.9398141  0.9655422  0.6263586  0.010487779  0.009023461  0.03610021
# 5    2.0    0.9377476  0.9673751  0.6115784  0.009763030  0.008343460  0.05930923
# 10    0.1    0.9281230  0.9472171  0.6746279  0.017881698  0.014455105  0.05878698
# 10    0.6    0.9438547  0.9585778  0.6504673  0.010396518  0.012276402  0.06041774
# 10    0.8    0.9433689  0.9629781  0.6337833  0.009339740  0.011546965  0.04903332
# 10    1.4    0.9404263  0.9651753  0.6301488  0.009483092  0.009356065  0.05201291
# 10    2.0    0.9382007  0.9673744  0.6003981  0.010212766  0.008547378  0.05012325
# 15    0.1    0.9304194  0.9497799  0.6523018  0.011546646  0.009932266  0.05510080
# 15    0.6    0.9412860  0.9593091  0.6449290  0.009465270  0.013000993  0.04192801
# 15    0.8    0.9423860  0.9607776  0.6375389  0.009096156  0.013450482  0.06176692
# 15    1.4    0.9405663  0.9655409  0.6226895  0.009974800  0.009304779  0.05751015
# 15    2.0    0.9382370  0.9677414  0.6003808  0.010232937  0.007953749  0.05378780

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were size = 3 and decay = 0.6. 

fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalNNETCaretNoDtm = train(blenderNoDtm, blenderYvals, method = "nnet", trControl = fitControl, tuneGrid = expand.grid(size = c(3, 5, 10, 15), decay = c(1e-1, 6e-1, 8e-1, 1.4, 2)), metric='ROC', MaxNWts = 5000)
# size  decay  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 3    0.1    0.9307039  0.9516141  0.6522845  0.012127284  0.009214857  0.04607451
# 3    0.6    0.9339371  0.9563800  0.6300450  0.009122899  0.013678699  0.06189765
# 3    0.8    0.9342290  0.9567456  0.6115092  0.006979077  0.014470484  0.04541708
# 3    1.4    0.9314206  0.9629775  0.6003634  0.007476155  0.011100003  0.06107500
# 3    2.0    0.9279185  0.9651759  0.5910696  0.007695301  0.009350795  0.05228715
# 5    0.1    0.9231793  0.9461162  0.6615438  0.013958346  0.008744339  0.04502345
# 5    0.6    0.9347079  0.9582115  0.6263586  0.006412420  0.013184373  0.03437833
# 5    0.8    0.9347245  0.9582122  0.6226376  0.007601306  0.011175810  0.06007176
# 5    1.4    0.9310452  0.9622442  0.6041018  0.007941259  0.011560898  0.05233630
# 5    2.0    0.9280944  0.9644420  0.5929733  0.007467499  0.010179022  0.06531496
# 10    0.1    0.9237010  0.9519844  0.6374351  0.013541172  0.010454804  0.03103328
# 10    0.6    0.9337095  0.9596801  0.6189512  0.005153761  0.011658052  0.04128640
# 10    0.8    0.9338194  0.9593124  0.6133437  0.006651046  0.011026219  0.04720301
# 10    1.4    0.9311150  0.9622442  0.5985635  0.007459092  0.011560898  0.06355518
# 10    2.0    0.9285322  0.9651753  0.5874178  0.007661751  0.009262010  0.06288177
# 15    0.1    0.9246023  0.9512464  0.6412426  0.013552980  0.004782410  0.05680379
# 15    0.6    0.9330696  0.9582115  0.6170474  0.007434372  0.010642542  0.05442343
# 15    0.8    0.9345580  0.9600444  0.6114919  0.005979421  0.012120051  0.05805297
# 15    1.4    0.9315451  0.9622442  0.5985462  0.007421235  0.011560898  0.05238641
# 15    2.0    0.9283954  0.9644420  0.5892350  0.007672081  0.009408384  0.05536157

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were size = 5 and decay = 0.8. 

# ============== SVM =====================
## Prep data for glmnet
FinalTrainSVM = filtTrain
FinalTrainDummy = dummyVars('~ .', data=FinalTrainSVM, fullRank=TRUE)
FinalTrainSVM = as.data.frame(predict(FinalTrainDummy, newdata=FinalTrainSVM))
# YvalsGlmnet = ifelse(Yvals == 'yes', 1, 0)

FinalTestSVM = filtTest
FinalTestDummy = dummyVars('~ .', data=FinalTestSVM, fullRank=TRUE)
FinalTestSVM = as.data.frame(predict(FinalTestDummy, newdata=FinalTestSVM))

FinalTrainSVMNoDtm = FinalTrainNoDtm
FinalTrainSVMNoDtmDummy = dummyVars('~ .', data=FinalTrainSVMNoDtm, fullRank=TRUE)
FinalTrainSVMNoDtm = as.data.frame(predict(FinalTrainSVMNoDtmDummy, newdata=FinalTrainSVMNoDtm))

FinalTestSVMNoDtm = FinalTrainNoDtm
FinalTestSVMNoDtmDummy = dummyVars('~ .', data=FinalTestSVMNoDtm, fullRank=TRUE)
FinalTestSVMNoDtm = as.data.frame(predict(FinalTestSVMNoDtmDummy, newdata=FinalTestSVMNoDtm))

ensembleFiltTrainSVM = FinalTrainSVM[(split+1):length(Yvals),]
ensembleTrainSVMNoDtm = FinalTrainSVMNoDtm[(split+1):length(Yvals),]
blenderFiltTrainSVM = FinalTrainSVM[1:split,]
blenderTrainSVMNoDtm = FinalTrainSVMNoDtm[1:split,]

fitControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalSVMCaretDtm = train(blenderFiltTrainSVM, blenderYvals, method='svmRadial',  trControl=fitControl, tuneGrid=expand.grid(C=c(0.25, 0.5, 1, 2, 4, 8), sigma=c(0.01, 0.02, 0.04, 0.08)), metric='ROC')

# C     sigma  ROC        Sens       Spec       ROC SD      Sens SD      Spec SD   
# 0.25  0.01   0.9050841  0.9577084  0.5054712  0.01514226  0.008895491  0.01040926
# 0.25  0.02   0.9009842  0.9599136  0.4780484  0.01782526  0.007155260  0.01721540
# 0.25  0.04   0.8932669  0.9628534  0.4670559  0.02082311  0.004763750  0.02003405
# 0.25  0.08   0.8884740  0.9617498  0.4542118  0.02268964  0.007739088  0.03008057
# 0.50  0.01   0.9051281  0.9577077  0.5091410  0.01497418  0.007674553  0.01586646
# 0.50  0.02   0.9018677  0.9621195  0.4725438  0.01912001  0.006439664  0.01445379
# 0.50  0.04   0.8959752  0.9624871  0.4706922  0.02276114  0.003801083  0.02857747
# 0.50  0.08   0.8926585  0.9669003  0.4761968  0.02311583  0.005354455  0.03950784
# 1.00  0.01   0.9061524  0.9606503  0.5073061  0.01620888  0.010190411  0.01820854
# 1.00  0.02   0.9053149  0.9680045  0.4944954  0.01891880  0.007856602  0.01391353
# 1.00  0.04   0.9024040  0.9713147  0.4981985  0.02233470  0.008069202  0.02780241
# 1.00  0.08   0.8985711  0.9698441  0.4853545  0.02325168  0.009771983  0.01956180
# 2.00  0.01   0.9106557  0.9702125  0.5036364  0.01578526  0.007388171  0.01308914
# 2.00  0.02   0.9100825  0.9720507  0.5109258  0.01772485  0.008254626  0.02917669
# 2.00  0.04   0.9066590  0.9705801  0.5238198  0.02117988  0.011835709  0.03218278
# 2.00  0.08   0.9025588  0.9683742  0.5440367  0.02345379  0.013650384  0.03663986
# 4.00  0.01   0.9134780  0.9698441  0.5255880  0.01491099  0.009509100  0.02221495
# 4.00  0.02   0.9126491  0.9680066  0.5457715  0.01690854  0.011142258  0.03039083
# 4.00  0.04   0.9078898  0.9624919  0.5586989  0.02095728  0.013781528  0.03285449
# 4.00  0.08   0.9019335  0.9639604  0.5477064  0.02135311  0.013029006  0.03886921
# 8.00  0.01   0.9153845  0.9665346  0.5751126  0.01425244  0.009752352  0.01832336 ****
# 8.00  0.02   0.9113730  0.9613862  0.5677898  0.01704346  0.009971729  0.02568607
# 8.00  0.04   0.9051685  0.9617539  0.5586656  0.01875986  0.013335691  0.02650489
# 8.00  0.08   0.8973168  0.9591803  0.5165304  0.02029378  0.012346159  0.02963926

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.01 and C = 8. 

# ============== GLMNET ==================???
fitControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalGlmNetDtm = train(model.matrix(~ ., blenderFiltTrain), blenderYvals, method='glmnet',  trControl=fitControl, tuneGrid=expand.grid(alpha=c(0.005, 0.01, 0.05), lambda=c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5)), family='binomial', metric="ROC", intercept=TRUE)
# alpha  lambda  ROC        Sens       Spec         ROC SD      Sens SD      Spec SD   
# 0.005  0.000   0.9393725  0.9596854  0.639692523  0.01491289  0.012786348  0.09022133
# 0.005  0.001   0.9393725  0.9596854  0.639692523  0.01491289  0.012786348  0.09022133
# 0.005  0.005   0.9395616  0.9618832  0.634102027  0.01473425  0.011331902  0.08888518
# 0.005  0.010   0.9402428  0.9637147  0.626694619  0.01481523  0.008343342  0.08980893 ****
# 0.005  0.050   0.9396086  0.9710448  0.587561146  0.01685292  0.008877205  0.10054211
# 0.005  0.100   0.9381508  0.9765406  0.561495458  0.01766227  0.006952679  0.09131530
# 0.005  0.500   0.9299364  0.9992661  0.081865828  0.02074018  0.001547300  0.03318031
# 0.010  0.000   0.9381453  0.9578526  0.652690426  0.01527603  0.013605247  0.08545372
# 0.010  0.001   0.9381453  0.9578526  0.652690426  0.01527603  0.013605247  0.08545372
# 0.010  0.005   0.9396227  0.9618832  0.634102027  0.01474941  0.011331902  0.08888518
# 0.010  0.010   0.9403106  0.9637147  0.626694619  0.01486380  0.008863206  0.08980893
# 0.010  0.050   0.9397312  0.9710448  0.587561146  0.01691541  0.008877205  0.10054211
# 0.010  0.100   0.9380970  0.9769083  0.555870021  0.01787801  0.006695396  0.09245374
# 0.010  0.500   0.9282623  0.9992661  0.063312369  0.02141455  0.001547300  0.04321782
# 0.050  0.000   0.9359016  0.9567496  0.661949686  0.01673334  0.012047279  0.08679147
# 0.050  0.001   0.9376011  0.9567523  0.656394130  0.01567529  0.013555088  0.08287746
# 0.050  0.005   0.9401331  0.9633498  0.630398323  0.01478615  0.010904480  0.08639963
# 0.050  0.010   0.9405090  0.9640824  0.624842767  0.01503301  0.010600983  0.09035523
# 0.050  0.050   0.9395637  0.9725086  0.580083857  0.01780173  0.009184509  0.09701537
# 0.050  0.100   0.9357237  0.9787384  0.524283718  0.01896146  0.005935769  0.08169665
# 0.050  0.500   0.9076470  1.0000000  0.001851852  0.02814761  0.000000000  0.00585607

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were alpha = 0.05 and lambda = 0.01

fitControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalGlmNetNoDtm = train(model.matrix(~ ., blenderNoDtm), blenderYvals, method='glmnet',  trControl=fitControl, tuneGrid=expand.grid(alpha=c(0.005, 0.01, 0.05), lambda=c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5)), family='binomial', metric="ROC", intercept=TRUE)
# alpha  lambda  ROC        Sens       Spec         ROC SD      Sens SD      Spec SD   
# 0.005  0.000   0.9344411  0.9607816  0.619252271  0.02074180  0.010784237  0.08124073
# 0.005  0.001   0.9344411  0.9607816  0.619252271  0.02074180  0.010784237  0.08124073
# 0.005  0.005   0.9343041  0.9604153  0.615513627  0.02041161  0.010886519  0.07930211
# 0.005  0.010   0.9336180  0.9640797  0.604367575  0.02069065  0.009890829  0.08962823
# 0.005  0.050   0.9300562  0.9703081  0.552096436  0.02189918  0.008008708  0.07303216
# 0.005  0.100   0.9270592  0.9750714  0.527917540  0.02302235  0.009603227  0.06577881
# 0.005  0.500   0.9163701  0.9996337  0.046470999  0.02550051  0.001158343  0.03838840
# 0.010  0.000   0.9345542  0.9593164  0.624807827  0.02069951  0.011632009  0.08433873
# 0.010  0.001   0.9345542  0.9593164  0.624807827  0.02069951  0.011632009  0.08433873 ****
# 0.010  0.005   0.9342977  0.9604153  0.615513627  0.02041324  0.010886519  0.07930211
# 0.010  0.010   0.9336044  0.9640797  0.604367575  0.02072738  0.009890829  0.08962823
# 0.010  0.050   0.9299748  0.9703081  0.552096436  0.02198172  0.008008708  0.07303216
# 0.010  0.100   0.9269237  0.9750714  0.527917540  0.02287754  0.009446707  0.06577881
# 0.010  0.500   0.9156093  1.0000000  0.037141859  0.02563645  0.000000000  0.03031366
# 0.050  0.000   0.9337031  0.9589474  0.628511530  0.02025406  0.010177953  0.08173260
# 0.050  0.001   0.9344979  0.9589488  0.628511530  0.02063099  0.011019936  0.08173260
# 0.050  0.005   0.9341545  0.9611479  0.617400419  0.02057201  0.010383722  0.08118894
# 0.050  0.010   0.9334130  0.9644460  0.602515723  0.02061721  0.009778081  0.08923875
# 0.050  0.050   0.9296008  0.9710407  0.544723969  0.02181228  0.008374835  0.07088513
# 0.050  0.100   0.9253418  0.9772705  0.509399022  0.02298612  0.008617882  0.06975714
# 0.050  0.500   0.9068101  1.0000000  0.001851852  0.02776691  0.000000000  0.00585607

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were alpha = 0.01 and lambda = 0.001. 

# ============== GLMBOOST ==================
fitControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalGlmBoostDtm = train(blenderFiltTrainSVM, blenderYvals, method='glmboost',  trControl=fitControl, tuneLength=8, metric="ROC")
# mstop  ROC        Sens       Spec       ROC SD      Sens SD      Spec SD   
# 50    0.9037939  0.9769056  0.5373515  0.02953959  0.007734223  0.07797827
# 100    0.9075692  0.9743388  0.5503145  0.02656514  0.008107440  0.07062670
# 150    0.9114505  0.9717733  0.5503145  0.02546233  0.008483039  0.07327462
# 200    0.9173382  0.9725086  0.5484626  0.02440427  0.007576952  0.07591400
# 250    0.9218064  0.9714097  0.5466108  0.02207997  0.007075240  0.07484056
# 300    0.9254168  0.9710434  0.5503145  0.02146835  0.005303609  0.07869525
# 350    0.9274383  0.9717774  0.5558700  0.02118911  0.005463125  0.07816036
# 400    0.9037939  0.9725113  0.5614605  0.02953959  0.005235114  0.08222333

# Tuning parameter 'prune' was held constant at a value of no
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were mstop = 350 and prune = no.


fitControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
FinalGlmBoostNoDtm = train(blenderTrainSVMNoDtm, blenderYvals, method='glmboost',  trControl=fitControl, tuneLength=8, metric="ROC")

# ==============================================
# Setup Stacked DataFrame and Model - GLMNET
# ==============================================
# Include some features from original training set and predictions of all models
ensembleData = ensembleNoDtm
ensembleData$FinalRFCaretDtmNs7 = predict(FinalRFCaretDtmNs7, newdata=ensembleFiltTrain, type='prob')[,2]
ensembleData$FinalRFCaretNoDtmNs7 = predict(FinalRFCaretNoDtmNs7, newdata=ensembleNoDtm, type='prob')[,2]
ensembleData$FinalGBMCaretDtm = predict(FinalGBMCaretDtm, newdata=ensembleFiltTrain, type='prob')[,2]
ensembleData$FinalNNETCaretDtm = predict(FinalNNETCaretDtm, newdata=ensembleFiltTrain, type='prob')[,2]
ensembleData$FinalGlmNetDtm = predict(FinalGlmNetDtm, newdata=model.matrix(~ ., ensembleFiltTrain), type='prob')[,2]
ensembleData$FinalGlmBoostDtm = predict(FinalGlmBoostDtm, newdata=ensembleFiltTrainSVM, type='prob')[,2]

testEnsembleData = FinalTestNoDtm
testEnsembleData$FinalRFCaretDtmNs7 = predict(FinalRFCaretDtmNs7, newdata=filtTest, type='prob')[,2]
testEnsembleData$FinalRFCaretNoDtmNs7 = predict(FinalRFCaretNoDtmNs7, newdata=FinalTestNoDtm, type='prob')[,2]
testEnsembleData$FinalGBMCaretDtm = predict(FinalGBMCaretDtm, newdata=filtTest, type='prob')[,2]
testEnsembleData$FinalNNETCaretDtm = predict(FinalNNETCaretDtm, newdata=filtTest, type='prob')[,2]
testEnsembleData$FinalGlmNetDtm = predict(FinalGlmNetDtm, newdata=model.matrix(~ ., filtTest), type='prob')[,2]
testEnsembleData$FinalGlmBoostDtm = predict(FinalGlmBoostDtm, newdata=FinalTestSVM, type='prob')[,2]

# ensembleData$FinalSVMCaretDtm = predict(FinalSVMCaretDtm, newdata=ensembleFiltTrainSVM, type='prob')[,2] # CRASHES R. Not sure why? Had to discard SVM

# ==============================================
# Train GLMNET metamodel
# ==============================================
fitControl <- trainControl(method='repeatedcv', number=10, repeats=10, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
# metaModelGLMNET = cv.glmnet(model.matrix(~ ., ensembleData), ensembleYvals, family='binomial', type.measure='auc', parallel=TRUE, intercept=TRUE, alpha=1)
metaModelGLMNET = train(model.matrix(~ ., ensembleData), ensembleYvals, method='glmnet',  trControl=fitControl, tuneGrid=expand.grid(alpha=c(0.05, 0.2, 0.5, 0.6, 0.7, 0.8, 0.9, 1), lambda=c(0, 0.005, 0.01, 0.05, 0.1, 0.5)), family='binomial', metric="ROC", intercept=TRUE)
# alpha  lambda  ROC        Sens       Spec       ROC SD      Sens SD      Spec SD   
# 0.05   0.000   0.9495489  0.9581338  0.7295260  0.01320464  0.010888790  0.06080976
# 0.05   0.005   0.9492114  0.9594614  0.7259221  0.01313640  0.010545143  0.06205528
# 0.05   0.010   0.9490760  0.9617480  0.7210779  0.01333931  0.010068403  0.06176233
# 0.05   0.050   0.9490312  0.9648099  0.6947370  0.01361972  0.009088645  0.06177513
# 0.05   0.100   0.9485412  0.9672074  0.6709708  0.01364757  0.009320695  0.06449144
# 0.05   0.500   0.9475422  0.9779421  0.5540097  0.01387901  0.008271314  0.06148458
# 0.20   0.000   0.9493111  0.9577275  0.7304123  0.01340356  0.011128446  0.06165090
# 0.20   0.005   0.9495478  0.9603096  0.7228571  0.01339974  0.010141804  0.06161209
# 0.20   0.010   0.9496905  0.9619321  0.7192597  0.01365570  0.010486902  0.06329516
# 0.20   0.050   0.9491983  0.9651051  0.6857403  0.01370614  0.009341847  0.06327323
# 0.20   0.100   0.9489463  0.9682033  0.6547532  0.01354855  0.009230324  0.06497071
# 0.20   0.500   0.9487857  0.9937291  0.2460779  0.01304849  0.004403205  0.05681256
# 0.50   0.000   0.9490137  0.9576536  0.7293344  0.01352742  0.011042878  0.06196783
# 0.50   0.005   0.9499501  0.9611208  0.7206916  0.01373024  0.009955515  0.06188790
# 0.50   0.010   0.9501724  0.9622641  0.7131201  0.01393544  0.009975831  0.06042939
# 0.50   0.050   0.9500285  0.9654367  0.6756753  0.01311128  0.009665020  0.06324915
# 0.50   0.100   0.9495869  0.9710070  0.6390747  0.01277141  0.009187221  0.06188676
# 0.50   0.500   0.9497378  1.0000000  0.0000000  0.01289289  0.000000000  0.00000000
# 0.60   0.000   0.9490121  0.9573953  0.7298766  0.01352600  0.011288302  0.06156259
# 0.60   0.005   0.9502340  0.9608624  0.7187143  0.01374584  0.009852108  0.06218456
# 0.60   0.010   0.9498323  0.9626700  0.7113247  0.01387838  0.010131593  0.05881325
# 0.60   0.050   0.9503035  0.9657685  0.6711623  0.01287267  0.009571693  0.06223962
# 0.60   0.100   0.9496924  0.9722610  0.6356526  0.01273883  0.009074109  0.06146855
# 0.60   0.500   0.5000000  1.0000000  0.0000000  0.00000000  0.000000000  0.00000000
# 0.70   0.000   0.9492707  0.9575429  0.7300552  0.01334047  0.010930552  0.06164164
# 0.70   0.005   0.9504580  0.9612679  0.7154773  0.01373883  0.010068321  0.06201846
# 0.70   0.010   0.9496904  0.9629280  0.7111494  0.01378786  0.010011239  0.05903088
# 0.70   0.050   0.9502216  0.9666170  0.6702403  0.01279927  0.010228413  0.06102918
# 0.70   0.100   0.9498177  0.9741050  0.6264675  0.01279093  0.009021278  0.06200519
# 0.70   0.500   0.5000000  1.0000000  0.0000000  0.00000000  0.000000000  0.00000000
# 0.80   0.000   0.9493695  0.9577640  0.7302338  0.01327284  0.010980109  0.06177797
# 0.80   0.005   0.9506861  0.9613417  0.7145682  0.01374346  0.009930297  0.06170358
# 0.80   0.010   0.9496287  0.9630020  0.7109675  0.01379870  0.010229332  0.05944776
# 0.80   0.050   0.9501781  0.9675023  0.6671818  0.01270546  0.009637402  0.06051028
# 0.80   0.100   0.9500335  0.9749537  0.6212208  0.01275828  0.008902466  0.06406417
# 0.80   0.500   0.5000000  1.0000000  0.0000000  0.00000000  0.000000000  0.00000000
# 0.90   0.000   0.9491879  0.9576534  0.7300519  0.01335853  0.010855352  0.06137984
# 0.90   0.005   0.9508956  0.9612313  0.7125812  0.01366506  0.009831947  0.06158576
# 0.90   0.010   0.9497338  0.9628546  0.7113409  0.01370790  0.010049681  0.06139309
# 0.90   0.050   0.9501740  0.9675022  0.6655519  0.01266788  0.009680131  0.06156079
# 0.90   0.100   0.9501207  0.9761712  0.6039351  0.01272123  0.008733120  0.06215407
# 0.90   0.500   0.5000000  1.0000000  0.0000000  0.00000000  0.000000000  0.00000000
# 1.00   0.000   0.9495609  0.9578377  0.7304156  0.01318526  0.010953675  0.06164511
# 1.00   0.005   0.9509148  0.9611577  0.7136558  0.01368272  0.009746004  0.06037114 ****
# 1.00   0.010   0.9499526  0.9624856  0.7104416  0.01359686  0.009965370  0.06168682
# 1.00   0.050   0.9503052  0.9669489  0.6675552  0.01255545  0.009711515  0.06018631
# 1.00   0.100   0.9503546  0.9772414  0.5908149  0.01254603  0.008772696  0.05848477
# 1.00   0.500   0.5000000  1.0000000  0.0000000  0.00000000  0.000000000  0.00000000

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were alpha = 1 and lambda = 0.005.

predmetaModelGLMNET = predict(metaModelGLMNET, newdata=model.matrix(~ ., testEnsembleData), type='prob')[,2]
ensemble_v8_GLMNET = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predmetaModelGLMNET)
write.csv(ensemble_v8_GLMNET, "Ensemble_v9_GLMNET.csv", row.names=FALSE)

# ==============================================
# Train GBM metamodel <-- Would have ranked 150
# ==============================================
fitControl <- trainControl(method='repeatedcv', number=10, repeats=3, returnResamp='none', classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(3, 4, 5, 7), n.trees = c(1000, 1500, 2000, 3000), shrinkage = c(0.004, 0.01))
metaModelGBM = train(ensembleData, ensembleYvals, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid) # 
# shrinkage  interaction.depth  n.trees  ROC        Sens       Spec       ROC SD       Sens SD     Spec SD   
# 0.002      7                  1000     0.9503037  0.9611470  0.6840693  0.012039220  0.01216097  0.06121961
# 0.002      7                  1500     0.9517284  0.9573353  0.7069156  0.011033536  0.01188783  0.06163338
# 0.002      7                  2000     0.9521694  0.9564743  0.7141450  0.010513534  0.01264161  0.06060032
# 0.002      7                  3000     0.9523376  0.9556142  0.7152922  0.009890612  0.01278978  0.05850673
# 0.002      8                  1000     0.9507843  0.9611465  0.6852814  0.012078825  0.01153183  0.06098763
# 0.002      8                  1500     0.9515011  0.9570893  0.7081277  0.011307199  0.01195906  0.06113461
# 0.002      8                  2000     0.9521966  0.9559823  0.7141450  0.010474241  0.01251287  0.06179868
# 0.002      8                  3000     0.9522774  0.9548757  0.7153463  0.010004657  0.01276207  0.06021130
# 0.002      9                  1000     0.9509858  0.9609001  0.6828680  0.011170699  0.01164657  0.06207487
# 0.002      9                  1500     0.9518874  0.9573349  0.7038961  0.010443372  0.01148688  0.06166886
# 0.002      9                  2000     0.9522452  0.9558597  0.7105303  0.010355887  0.01203609  0.05972156
# 0.002      9                  3000     0.9522753  0.9553686  0.7141126  0.009805038  0.01263572  0.05643684
# 0.004      7                  1000     0.9519595  0.9562283  0.7141450  0.010330337  0.01291143  0.05677755
# 0.004      7                  1500     0.9521431  0.9551217  0.7159199  0.009973452  0.01214016  0.05899120
# 0.004      7                  2000     0.9518861  0.9540152  0.7147078  0.009854276  0.01227952  0.05753919
# 0.004      7                  3000     0.9515073  0.9546306  0.7128896  0.009579825  0.01259489  0.06004482
# 0.004      8                  1000     0.9515016  0.9563508  0.7135390  0.010539615  0.01210137  0.06161562
# 0.004      8                  1500     0.9520046  0.9545072  0.7117316  0.009514114  0.01237753  0.05776835
# 0.004      8                  2000     0.9518681  0.9548771  0.7177165  0.009390749  0.01361339  0.05462188
# 0.004      8                  3000     0.9511216  0.9543860  0.7062987  0.009461713  0.01380877  0.06127306
# 0.004      9                  1000     0.9520236  0.9563508  0.7123160  0.010812933  0.01324838  0.06134624
# 0.004      9                  1500     0.9522587  0.9557367  0.7147294  0.009826495  0.01288473  0.05776550
# 0.004      9                  2000     0.9520016  0.9545081  0.7117316  0.009606232  0.01360234  0.05633434
# 0.004      9                  3000     0.9511627  0.9548771  0.7044913  0.009483527  0.01294202  0.06125048
# 0.006      7                  1000     0.9517920  0.9546297  0.7177273  0.009848071  0.01248467  0.05947130
# 0.006      7                  1500     0.9517973  0.9549992  0.7171212  0.009638910  0.01271720  0.05889225
# 0.006      7                  2000     0.9514609  0.9543846  0.7080844  0.009427531  0.01336689  0.06012366
# 0.006      7                  3000     0.9503990  0.9553695  0.6990693  0.009692434  0.01285463  0.06110519
# 0.006      8                  1000     0.9522636  0.9556137  0.7158874  0.009862467  0.01257072  0.05914729
# 0.006      8                  1500     0.9518187  0.9540161  0.7122727  0.009429810  0.01279921  0.05953238
# 0.006      8                  2000     0.9513849  0.9543855  0.7038853  0.009519286  0.01350217  0.05770052
# 0.006      8                  3000     0.9500215  0.9550005  0.6972727  0.009676041  0.01363680  0.06587947
# 0.006      9                  1000     0.9524999  0.9553686  0.7165152  0.009851467  0.01303886  0.05758539 ****
# 0.006      9                  1500     0.9521334  0.9546311  0.7135390  0.009338216  0.01263038  0.05927242
# 0.006      9                  2000     0.9514309  0.9550001  0.7087662  0.009293549  0.01322014  0.06328260
# 0.006      9                  3000     0.9500156  0.9551244  0.7009091  0.009673402  0.01302521  0.06472224

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 1000, interaction.depth = 9 and shrinkage = 0.006. 

predmetaModelGBM = predict(metaModelGBM, newdata=testEnsembleData, type='prob')[,2]
ensemble_v8_GBM = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predmetaModelGBM)
write.csv(ensemble_v8_GBM, "Ensemble_v9_GBM.csv", row.names=FALSE) 
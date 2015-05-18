# ==============================================================================
# Kaggle Competition - Analytics Edge, Spring 2015
#
# Author: Sudeep Mandal / HamsterHuey
#
# Notes: This script uses CaretEnsemble package to train ensembles using 
# either linear combination or model stacking with a meta-model (GLMNET or GBM).
# This model (Ensemble_v7_linear_GBM_NNET_RF) was ranked 140 on Private LB
#
# Reference: http://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
# ===============================================================================

# Read Data
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
library(tm)

accuracy = function(vec1, vec2){
  t = table(vec1, vec2)
  (t[1,1] + t[2,2])/length(vec1)
}

# Function to do all the work of creating the final DataFrames
parseTextFields = function(NewsTrain, NewsTest, sparseValue){
  require(tm)
  
  CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
  CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
  
  CorpusHeadline = tm_map(CorpusHeadline, tolower)
  CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
  CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
  CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
  CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
  
  CorpusAbstract = tm_map(CorpusAbstract, tolower)
  CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
  CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
  CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
  CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
  
  dtmH = DocumentTermMatrix(CorpusHeadline)
  sparse = removeSparseTerms(dtmH, sparseValue)
  HeadlineWords = as.data.frame(as.matrix(sparse))
  
  dtmA = DocumentTermMatrix(CorpusAbstract)
  sparse = removeSparseTerms(dtmA, sparseValue)
  AbstractWords = as.data.frame(as.matrix(sparse))
  
  # Ensure proper variable names  by adding prefix using paste0
  colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
  colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))
  
  colnames(AbstractWords) = make.names(colnames(AbstractWords))
  colnames(AbstractWords) = paste0("A", colnames(AbstractWords))
  
  # Combine all DTMs into one DataFrame
  dtmFinal = cbind(HeadlineWords, AbstractWords)
  dtm_names = names(dtmFinal)
  
  dtmFinal$WordCount = c(log(1+NewsTrain$WordCount), log(1+NewsTest$WordCount))
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
  dtmFinal$HeadlineLength = nchar(c(NewsTrain$Headline, NewsTest$Headline))
  
  # Zero Wordcount articles should have Popular = 0
  dtmFinal$zeroWordCount = as.factor(c(NewsTrain$WordCount==0, NewsTest$WordCount==0))
  
  # Colon in headline? (Typically not popular)
  dtmFinal$Colon = as.factor(c(grepl('^.+:.+$', NewsTrain$Headline), grepl('^.+:.+$', NewsTest$Headline)))
  
  # DTM Phrases (Manually)
  # ======================
  dtmFinal$globalWarming = as.factor(c(grepl('global warming', NewsTrain$Abstract, ignore.case = TRUE), grepl('global warming', NewsTest$Abstract, ignore.case = TRUE)))
  
  dtmFinal$iOS = as.factor(c(grepl('^.*(iOS).*$', NewsTrain$Headline, ignore.case=FALSE), grepl('^.*(iOS).*$', NewsTest$Headline, ignore.case=FALSE)))
  
  dtmFinal$obamacare = as.factor(c(grepl('^.*(obamacare).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(obamacare).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$frack = as.factor(c(grepl('^.*(frack).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(frack).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$racism = as.factor(c(grepl('^.*(racism).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(racism).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$sexual = as.factor(c(grepl('^.*(sexual).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(sexual).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$geithner = as.factor(c(grepl('^.*(geithner).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(geithner).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  # Mildly positive to neutral
  dtmFinal$inequality = as.factor(c(grepl('^.*(inequality).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(inequality).*$', NewsTest$Headline, ignore.case=TRUE)))
  dtmFinal$readRespond = as.factor(c(grepl('^.*(readers respond).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(readers respond).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  # Strongly negative correlation with Popularity
  dtmFinal$mAgenda = as.factor(c(grepl('^(Morning Agenda).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Morning Agenda).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$fDraft = as.factor(c(grepl('^(First Draft Focus).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(First Draft Focus).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$dailyReport = as.factor(c(grepl('^(Daily Report).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Daily Report).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$coverStory = as.factor(c(grepl('^(Behind the cover story).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Behind the cover story).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$QnA = as.factor(c(grepl('^(Q\\. and A\\.).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Q\\. and A\\.).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$NYToday = as.factor(c(grepl('^(New York Today).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(New York Today).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$historyColon = as.factor(c(grepl('^([0-9]+:).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^([0-9]+:).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$inPerf = as.factor(c(grepl('^(In performance).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(In performance).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$todayIn = as.factor(c(grepl('(today in|small business)', NewsTrain$Headline, ignore.case=TRUE), grepl('(today in|small business)', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$weeklyQuandry = as.factor(c(grepl('^(Weekly Quandary).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Weekly Quandary).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$fashionWeek = as.factor(c(grepl('scenes from|fashion week', NewsTrain$Headline, ignore.case=TRUE), grepl('scenes from|fashion week', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$thinkLikeDoc = as.factor(c(grepl('^(Think Like a Doctor).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Think Like a Doctor).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$popcast = as.factor(c(grepl('^(Popcast).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Popcast).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$pics = as.factor(c(grepl('^.*(pictures|photograph).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(pictures|photograph).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$pipe = as.factor(c(grepl('^.*(\\|).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^.*(\\|).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$factsFigures = as.factor(c(grepl('^(Facts & Figures).*$', NewsTrain$Headline, ignore.case=TRUE), grepl('^(Facts & Figures).*$', NewsTest$Headline, ignore.case=TRUE)))
  
  dtmFinal$appearedIn = as.factor(c(grepl('appeared in', NewsTrain$Abstract, ignore.case = TRUE), grepl('appeared in', NewsTest$Abstract, ignore.case = TRUE)))
  
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
  
  return(list('a'=FinalTrain, 'b'=FinalTest, 'dtmH'=dtmH, 'dtmA'=dtmA, dtm_names))
}

t = parseTextFields(NewsTrain, NewsTest, 0.99)
FinalTrain = t[1]$a
FinalTest = t[2]$b

# NoDtmVars keeps track of all features (column names) that did not come from Bag of words analysis.
noDtmVars = names(FinalTrain)[!(names(FinalTrain) %in% t[[5]])] 
noDtmVars = noDtmVars[noDtmVars != 'Popular']

# =================================================
# Begin fitting models
# =================================================
library(doParallel)
registerDoParallel(cores=4)
library(caret)
library(caretEnsemble)
library(mlbench)
library(pROC)

# Outcome should be factor with valid R variable names so 0,1 won't work
Yvals = ifelse(FinalTrain$Popular == 1, 'yes', 'no')
Yvals = as.factor(Yvals)

# Set up FinalTrainNoDtm with only non-DTM variables - For models that don't use Bag of Words analysis
FinalTrainNoDtm = FinalTrain[noDtmVars]
FinalTestNoDtm = FinalTest[noDtmVars]

# Set up FinalTrainDtm with only DTM variables - To explore training models on only bag of words
FinalTrainDtm = FinalTrain
FinalTestDtm = FinalTest
FinalTrainDtm = FinalTrain[, -which(names(FinalTrain) %in% noDtmVars)]
FinalTestDtm = FinalTest[, -which(names(FinalTest) %in% noDtmVars)]
FinalTrainDtm$Popular=NULL
FinalTestDtm$Popular=NULL

# Dummify Factor variables (needed for SVM and GLMBOOST)
FinalTrainGlmnetNoDtm = FinalTrainNoDtm
FinalTrainNoDtmDummy = dummyVars('~ .', data=FinalTrainGlmnetNoDtm, fullRank=FALSE)
FinalTrainGlmnetNoDtm = as.data.frame(predict(FinalTrainNoDtmDummy, newdata=FinalTrainGlmnetNoDtm))
FinalTestGlmnetNoDtm = FinalTestNoDtm
FinalTestNoDtmDummy = dummyVars('~ .', data=FinalTestGlmnetNoDtm, fullRank=FALSE)
FinalTestGlmnetNoDtm = as.data.frame(predict(FinalTestNoDtmDummy, newdata=FinalTestGlmnetNoDtm))

# Setup trainControl object (uses bootstrap for CV)
my_control <- trainControl(
  method='boot',
  number=25,
  savePredictions=TRUE,
  classProbs=TRUE,
  index=createResample(FinalTrain$Popular, 25),
  summaryFunction=twoClassSummary
)

# Use caretEnsemble library to train and create ensemble of models
model_list_1 <- caretList(
  FinalTrainNoDtm, Yvals,
  trControl=my_control,
  metric='ROC',
  tuneList=list(
    rf=caretModelSpec(method='rf', tuneGrid=expand.grid(mtry=c(9)), ntree=2000, nodesize=7),
    nnet=caretModelSpec(method='nnet', tuneGrid = expand.grid(size = c(5, 10, 15, 20), decay = c(0, 1e-4, 1e-2, 1e-1, 6e-1)), MaxNWts = 5000),
    gbm=caretModelSpec(method='gbm', tuneGrid=expand.grid(interaction.depth = c(1, 2, 3), n.trees = c(150, 250, 350, 450, 1000), shrinkage = c(0.01,0.1)))
    # glmnet=caretModelSpec(method='glmnet'),
    # svmRad=caretModelSpec(method='svmRadial')
  )
)

# Visualize and analyze
plot(model_list_1$nnet)
plot(model_list_1$gbm)
resamps <- resamples(list(GBM = model_list_1$gbm,
                          RF = model_list_1$rf,
                          NNET = model_list_1$nnet))
splom(resamps)
xyplot(resamples(model_list_1[c(2,3)]))
modelCor(resamples(model_list_1))

# TUNED VALUES
# ============
# NNET
# ------
# size  decay  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 5    0.6    0.9407146  0.9558324  0.6854026  0.005531792  0.006742229  0.02503259
# 5    0.8    0.9414052  0.9570419  0.6848843  0.004852502  0.005984175  0.02333159
# 5    1.0    0.9413715  0.9580844  0.6762829  0.005736391  0.005976069  0.02366885
# 5    1.5    0.9410311  0.9597978  0.6652768  0.004829447  0.005420409  0.02457815
# 15    0.6    0.9395209  0.9554213  0.6821969  0.005278713  0.005899102  0.02334356
# 15    0.8    0.9405823  0.9572989  0.6831438  0.004905674  0.005448844  0.02029951
# 15    1.0    0.9410996  0.9587391  0.6748319  0.004371201  0.005667423  0.02474961
# 15    1.5    0.9414440  0.9602115  0.6645197  0.004796757  0.005461701  0.02183587
# 20    0.6    0.9404176  0.9556671  0.6850017  0.004537843  0.006185826  0.02529701
# 20    0.8    0.9410542  0.9576696  0.6799090  0.004561025  0.005672687  0.02206105
# 20    1.0    0.9418221  0.9583233  0.6769624  0.004451653  0.005579472  0.02111681
# 20    1.5    0.9413768  0.9603888  0.6650480  0.004707115  0.005285962  0.02322616
# 30    0.6    0.9407602  0.9565352  0.6835219  0.004483172  0.006683686  0.02222288
# 30    0.8    0.9414474  0.9576719  0.6777342  0.004481381  0.005060219  0.02156246
# 30    1.0    0.9415996  0.9587108  0.6709788  0.004164584  0.004921262  0.02382210
# 30    1.5    0.9413928  0.9596590  0.6634356  0.004588481  0.005272669  0.02192739

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were size = 20 and decay = 1. 
# ------
# GBM
# ------
# shrinkage  interaction.depth  n.trees  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 0.01       2                   300     0.9289535  0.9669327  0.5929673  0.006051811  0.005069993  0.02469667
# 0.01       2                   600     0.9346314  0.9647053  0.6278949  0.005402129  0.005013160  0.02450202
# 0.01       2                  1000     0.9375096  0.9617373  0.6543925  0.005379453  0.006155385  0.02487822
# 0.01       2                  2000     0.9398473  0.9597583  0.6727938  0.005441562  0.006300750  0.02204142
# 0.01       2                  3000     0.9404115  0.9589056  0.6794610  0.005452790  0.006203228  0.01902791
# 0.01       3                   300     0.9331842  0.9664168  0.6082431  0.005537001  0.004533761  0.02497792
# 0.01       3                   600     0.9374843  0.9623874  0.6496092  0.005239568  0.006009033  0.02373276
# 0.01       3                  1000     0.9395392  0.9602688  0.6679073  0.005308866  0.006049518  0.02328602
# 0.01       3                  2000     0.9408204  0.9580863  0.6845474  0.005365004  0.006179239  0.01902448
# 0.01       3                  3000     0.9407305  0.9568546  0.6875098  0.005307066  0.006150000  0.01942439
# 0.01       4                   300     0.9354931  0.9656212  0.6177633  0.005335445  0.005528611  0.02553713
# 0.01       4                   600     0.9388805  0.9613929  0.6539769  0.005173481  0.005836123  0.02291320
# 0.01       4                  1000     0.9403343  0.9594621  0.6728071  0.005255637  0.006257442  0.02266123
# 0.01       4                  2000     0.9409493  0.9566540  0.6870695  0.005201100  0.006340934  0.01968989
# 0.01       4                  3000     0.9404822  0.9557406  0.6885841  0.005182498  0.006300379  0.02069384
# 0.01       5                   300     0.9366017  0.9650192  0.6238433  0.005205580  0.005986312  0.02564487
# 0.01       5                   600     0.9396632  0.9609507  0.6628500  0.005080178  0.006225536  0.02357819
# 0.01       5                  1000     0.9407537  0.9588212  0.6780114  0.005141527  0.006292328  0.02149830
# 0.01       5                  2000     0.9408812  0.9562181  0.6877804  0.005199436  0.005820294  0.02098995
# 0.01       5                  3000     0.9400583  0.9550067  0.6894833  0.005242603  0.006354047  0.01973998
# 0.06       2                   300     0.9390807  0.9599121  0.6701804  0.005237885  0.006465140  0.02266635
# 0.06       2                   600     0.9400278  0.9578330  0.6793798  0.005503184  0.006413493  0.02172313
# 0.06       2                  1000     0.9393200  0.9561180  0.6818473  0.005516515  0.006446825  0.01854307
# 0.06       2                  2000     0.9367125  0.9530293  0.6789908  0.005613853  0.006087782  0.01737396
# 0.06       2                  3000     0.9345337  0.9508724  0.6800768  0.005554959  0.005854538  0.01997590
# 0.06       3                   300     0.9400765  0.9580327  0.6787395  0.005350837  0.005959473  0.02049492
# 0.06       3                   600     0.9396523  0.9553041  0.6858348  0.005478833  0.005767061  0.01986926
# 0.06       3                  1000     0.9382232  0.9537922  0.6855268  0.005303909  0.006620605  0.02114251
# 0.06       3                  2000     0.9344451  0.9515371  0.6817193  0.005278364  0.005591795  0.01779228
# 0.06       3                  3000     0.9308962  0.9501651  0.6758124  0.005333257  0.005926079  0.01963715
# 0.06       4                   300     0.9401769  0.9568188  0.6849305  0.005285903  0.006137502  0.02101392
# 0.06       4                   600     0.9390591  0.9539248  0.6871185  0.005447309  0.006014115  0.01911313
# 0.06       4                  1000     0.9369487  0.9531534  0.6834042  0.005457620  0.006070496  0.01605465
# 0.06       4                  2000     0.9319742  0.9511702  0.6745580  0.005540343  0.006917931  0.01795209
# 0.06       4                  3000     0.9285477  0.9496949  0.6728465  0.005063566  0.006092922  0.01459398
# 0.06       5                   300     0.9398759  0.9564943  0.6830493  0.005394064  0.006809441  0.02025741
# 0.06       5                   600     0.9381915  0.9545574  0.6862567  0.005554227  0.006328698  0.02140311
# 0.06       5                  1000     0.9353536  0.9522855  0.6813985  0.005465612  0.006029036  0.02059293
# 0.06       5                  2000     0.9302009  0.9508859  0.6752577  0.005212482  0.006197544  0.02267916
# 0.06       5                  3000     0.9267633  0.9499890  0.6700631  0.005217810  0.006293972  0.02021947

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 2000, interaction.depth = 4 and shrinkage = 0.01. 
# ------
# RF
# ------
# ROC        Sens       Spec       ROC SD       Sens SD      Spec SD   
# 0.9435444  0.9570449  0.7039796  0.005007405  0.006847966  0.02607896

# Tuning parameter 'mtry' was held constant at a value of 9


# Use caretEnsemble method to create optimized linear combination of trained models in caretList object
greedy_ensemble_1 <- caretEnsemble(model_list_1)
summary(greedy_ensemble_1)
# The following models were ensembled: rf, nnet, gbm 
# They were weighted: 
# 0.43 0.39 0.18
# The resulting AUC is: 0.9464
# The fit for each individual model on the AUC is: 
# method    metric    metricSD
# rf 0.9432257 0.005007405
# nnet 0.9414117 0.004524957
# gbm 0.9403172 0.006523788

# Analyze linear ensemble
varImp(greedy_ensemble_1)

# Make predictions on greedy ensemble (linear mix)
library('caTools')
model_preds_linear <- lapply(model_list_1, predict, newdata=FinalTestNoDtm, type='prob')
model_preds_linear <- lapply(model_preds_linear, function(x) x[,'yes'])
model_preds_linear <- data.frame(model_preds_linear)
ens_preds_linear <- predict(greedy_ensemble_1, newdata=FinalTestNoDtm)

# Write output CSV file
Ensemble_v7_lin = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = ens_preds_linear)
write.csv(Ensemble_v7_lin, "Ensemble_v7_linear_GBM_NNET_RF.csv", row.names=FALSE)

#================================================
# END OF SUBMISSION THAT RANKED 150 on Private LB
# Continue reading at your peril
#================================================

# Use caretStack method for stacking models with a metamodel. Trained a GBM and GLMNET meta model
glm_ensemble <- caretStack(
  model_list_1, 
  method='glm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=20,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
CF/sum(CF)
# rf        nnet         gbm 
# 0.70612882  0.33631223 -0.04244105 

# Make predictions on Stacked GLM model
ens_preds_stacked_glm <- predict(glm_ensemble, newdata=FinalTestNoDtm, type='prob')$yes
Ensemble_v7_stack = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = ens_preds_stacked_glm)
write.csv(Ensemble_v7_stack, "Ensemble_v7_stacked_GLM.csv", row.names=FALSE)

# GBM based stacking
gbm_ensemble <- caretStack(
  model_list_1, 
  method='gbm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=20,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  ),
  tuneGrid=expand.grid(n.trees=c(100, 150, 200, 500), interaction.depth=c(3,4,5), shrinkage=c(0.01, 0.1))
)
varImp(gbm_ensemble$ens_model)
# Overall
# rf    100.00
# nnet   49.37
# gbm     0.00
# shrinkage  interaction.depth  n.trees  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD    
# 0.01       3                  100      0.9422806  0.9764670  0.5802788  0.002093448  0.001876126  0.012485155
# 0.01       3                  150      0.9447522  0.9717002  0.6253520  0.001599673  0.001504096  0.011464813
# 0.01       3                  200      0.9455648  0.9671641  0.6575649  0.001302657  0.001655299  0.011830571
# 0.01       3                  500      0.9466555  0.9596424  0.6970396  0.001443154  0.002471967  0.012256733
# 0.01       4                  100      0.9440769  0.9765238  0.5804685  0.001798143  0.001856252  0.012305442
# 0.01       4                  150      0.9453163  0.9714593  0.6286776  0.001457314  0.001411247  0.009815598
# 0.01       4                  200      0.9458385  0.9669208  0.6606236  0.001307441  0.001584942  0.010065182
# 0.01       4                  500      0.9467278  0.9599051  0.6958485  0.001461904  0.002284889  0.011636439
# 0.01       5                  100      0.9448603  0.9765647  0.5816531  0.001453807  0.001896300  0.011614540
# 0.01       5                  150      0.9454526  0.9711399  0.6317023  0.001326939  0.001480525  0.010736266
# 0.01       5                  200      0.9459135  0.9665468  0.6630282  0.001483890  0.001588893  0.009395433
# 0.01       5                  500      0.9467920  0.9596664  0.6966616  0.001458759  0.002247808  0.011515288
# 0.10       3                  100      0.9466562  0.9607662  0.6922446  0.001478376  0.002221848  0.010190609
# 0.10       3                  150      0.9466553  0.9610805  0.6902490  0.001438664  0.001787164  0.008778056
# 0.10       3                  200      0.9466035  0.9608176  0.6902706  0.001446054  0.001735805  0.009013578
# 0.10       3                  500      0.9460386  0.9601109  0.6895898  0.001448052  0.001832982  0.007802363
# 0.10       4                  100      0.9466869  0.9602687  0.6940984  0.001422635  0.001905843  0.009526630
# 0.10       4                  150      0.9465864  0.9605727  0.6910883  0.001478064  0.001737902  0.008926414
# 0.10       4                  200      0.9464898  0.9604944  0.6909630  0.001477822  0.001901853  0.009332898
# 0.10       4                  500      0.9457019  0.9593341  0.6887999  0.001453069  0.001656077  0.007722906
# 0.10       5                  100      0.9466125  0.9606392  0.6919283  0.001466987  0.002140880  0.010338956
# 0.10       5                  150      0.9465318  0.9606703  0.6917515  0.001515027  0.001948728  0.009019354
# 0.10       5                  200      0.9463744  0.9604233  0.6915232  0.001498417  0.001791454  0.008393423
# 0.10       5                  500      0.9453730  0.9593251  0.6879579  0.001538386  0.001538182  0.006985967

# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 500, interaction.depth = 5 and shrinkage = 0.01. 

# Make predictions on Stacked ensemblemodel_preds2 <- model_preds
ens_preds_stacked_gbm <- predict(gbm_ensemble, newdata=FinalTestNoDtm, type='prob')$yes
Ensemble_v7_stack = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = ens_preds_stacked_gbm)
write.csv(Ensemble_v7_stack, "Ensemble_v7_stacked_GBM.csv", row.names=FALSE)
# Libraries
library(ROCR)
library(caTools)
library(rpart)
library(randomForest)
library(caret)
library(e1071)
library(rpart.plot)
library(caTools)
library(tm)
library(cvTools)
library(cvAUC)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(arm)
library(plyr)
library(countrycode)
library(foreach)
library(doParallel)
library(RWeka)
#library(rJava)
library(MASS)
library(pryr)
library(glmnet)

# Read data
setwd('F:/Kaggle competition')

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
News = rbind(NewsTrain[, !(colnames(NewsTrain) %in% "Popular")], NewsTest)

# Text operations
CorpusHeadline = Corpus(VectorSource(News$Headline))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
LeaveWords = c('you', 'your', 'we', 'what', 'when', 'why',"shouldn't",'should', 'would')
RemoveWords = c(stopwords("english"))
RemoveWords = RemoveWords[! RemoveWords %in% LeaveWords]
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, RemoveWords)
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
HeadlineDTM <- DocumentTermMatrix(CorpusHeadline, control = list(tokenize = BigramTokenizer))
sparseHeadline = removeSparseTerms(dtmHeadline, 0.999)
HeadlineWords = as.data.frame(as.matrix(sparseHeadline), row.names = FALSE)
colnames(HeadlineWords) = make.names(paste("Headline",colnames(HeadlineWords)))
gc()
CorpusAbstract = Corpus(VectorSource(News$Abstract))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
LeaveWords = c('you', 'your', 'we', 'what', 'when', 'why',"shouldn't",'should', 'would')
RemoveWords = c(stopwords("english"))
RemoveWords = RemoveWords[! RemoveWords %in% LeaveWords]
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, RemoveWords)
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
AbstractDTM <- DocumentTermMatrix(CorpusAbstract, control = list(tokenize = BigramTokenizer))
sparseAbstract = removeSparseTerms(dtmAbstract, 0.999)
AbstractWords = as.data.frame(as.matrix(sparseAbstract), row.names = FALSE)
colnames(AbstractWords) = make.names(paste("Abstract",colnames(AbstractWords)))
gc()
#Creating numeric Vars
News$WordCount[News$WordCount<10|News$WordCount>=4000] = mean(News$WordCount)
News$HeadlineLen= nchar(News$Headline)
News$AbstractLen= nchar(News$Abstract)

#Creating factor Vars
News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")
News$Weekday = weekdays(News$PubDate)
News$Hour = format(News$PubDate, "%H")
drops = c("PubDate")
News = News[,!(names(News) %in% drops)]
News$EduHigh = as.factor(
  (grepl("school",tolower(News$Abstract))#0.2
   |grepl("campus",tolower(News$Abstract))#0.3
   |grepl("education",tolower(News$Abstract))#0.1
   |grepl("colleg",tolower(News$Abstract))##0.3
   |grepl("student",tolower(News$Abstract))#0.15
   |grepl("academic",tolower(News$Abstract))#0.36
   |grepl("university",tolower(News$Abstract))#0.12
   |grepl("teach",tolower(News$Abstract)))# 0.13
  & nchar(News$NewsDesk) ==0 & nchar(News$SectionName)==0 
)
News$EduHigh2 = as.factor(
  grepl("campus",tolower(News$Abstract))#0.3
  |grepl("colleg",tolower(News$Abstract))##0.3
  |grepl("academic",tolower(News$Abstract))#0.36
)


News$QQ = as.factor(
  grepl("question",tolower(News$Abstract))
)
News$Web = as.factor(
  grepl("tweet",tolower(News$Abstract))
  |grepl("facebook",tolower(News$Abstract))
  |grepl("Apple",News$Abstract)
)
News$Agressive = as.factor(
  
  grepl("military",tolower(News$Abstract))
  |grepl("terror",tolower(News$Abstract))
  |grepl("harass",tolower(News$Abstract))
  |grepl("violence",tolower(News$Abstract))  
  |grepl("rape",tolower(News$Abstract))
  |grepl("insult",tolower(News$Abstract))
  
)
News$Actual = as.factor(  
  grepl("recent",tolower(News$Abstract))
)
News$Woman =  as.factor(  
  grepl("woman",tolower(News$Abstract))
  |grepl("women",tolower(News$Abstract))
)
News$Comment = as.factor(
  grepl("comment", tolower(News$Abstract))  
)
News$False = as.factor(
  grepl("false", tolower(News$Abstract))  
  |grepl("wrong",tolower(News$Abstract))
  |grepl("false", tolower(News$Headline))  
  |grepl("wrong",tolower(News$Headline))
)

###############  New approach  ###############
News$Group = as.factor(
  ifelse(
    (
      ( 
        nchar(News$NewsDesk)==0
        &(nchar(News$SectionName)==0|News$SectionName == 'Health'|News$SectionName == 'Open'|News$SectionName == 'Travel')
      )
      |News$NewsDesk == 'Sports'
      |News$NewsDesk == 'Metro'
      |News$NewsDesk == 'Culture'
      |News$SectionName == 'Arts'
      |(nchar(News$NewsDesk) == 0 & News$SectionName == 'N.Y. / Region')
    ), 'Empty',
    ifelse(
      (News$SectionName=='Business Day'), 'BD',
      ifelse(
        (News$SectionName=='Crosswords/Games'), 'CrossGames', 
        ifelse(
          ((News$SectionName == 'Multimedia')|
             (News$SubsectionName == 'Room For Debate')
           |(News$NewsDesk == 'Foreign')
           |(News$NewsDesk == 'Magazine')
           |(News$NewsDesk == 'National')
           |(News$NewsDesk == 'Styles' & News$SectionName != 'U.S.')
           |(News$NewsDesk == 'Travel')
           |(News$NewsDesk == 'TStyle')       
           |(nchar(News$NewsDesk) == 0 & News$SubsectionName == 'Asia Pacific') 
          ), 'VL',
          ifelse(
            (News$SectionName=='U.S.'& nchar(News$NewsDesk) == 0), 'EUS',
            ifelse(
              ((News$NewsDesk == 'Science'& News$SectionName=='Opinion')
               |(News$NewsDesk == 'Business'& nchar(News$SectionName) == 0)
               |(News$NewsDesk == 'Business'& News$SectionName == 'Technology')
               |(nchar(News$NewsDesk) == 0 & News$SectionName == 'Technology')
               |(nchar(News$NewsDesk) == 0 & News$SectionName == 'Opinion' & nchar(News$SubsectionName) == 0)
              ), 'Tech',
              ifelse(
                (News$NewsDesk=='Foreign'), 'Foreign',
                ifelse(
                  (News$NewsDesk=='OpEd'|News$SubsectionName == 'The Public Editor'), 'OpEd',
                  ifelse(
                    (News$NewsDesk=='Science'&News$SectionName != 'Opinion'), 'Science',
                    ifelse(
                      (News$NewsDesk=='Styles' & News$SectionName == 'U.S.'), 'StUS', 'Error'
                    )))))))))))
News$Qmark = as.factor(grepl("\\?",News$Headline)|
                         (grepl("\\?",News$Abstract)== TRUE & nchar(News$NewsDesk)==0 
                          & nchar(News$SectionName)==0))
News$You = as.factor(grepl("you",News$Headline)
                     |grepl("you",News$Abstract)
                     |grepl("you",News$Snippet))

News$EduMult = as.factor((News$SubsectionName == 'Education'|News$SectionName == 'Multimedia'))
News$BD = as.factor(News$NewsDesk == 'Business' & News$SectionName == 'Business Day')
News$Cross = as.factor(News$SectionName == 'Crosswords/Games')
News$StyleE = as.factor(News$NewsDesk == 'Styles' & nchar(News$SectionName) == 0)
News$StyleUS = as.factor(News$NewsDesk == 'Styles' & News$SectionName == 'U.S.')
News$RFD = as.factor(News$SubsectionName=='Room For Debate')
News$All.billion = as.factor(News$All.billion + News$All.million)

News$Should = as.factor(grepl('should',tolower(News$Headline))
                        |grepl('should',tolower(News$Abstract))
                        |grepl('should',tolower(News$Snippet))
)

News$War = as.factor(grepl('war',tolower(News$Headline))
                     |grepl('war',tolower(News$Abstract))
                     |grepl('war',tolower(News$Snippet))            
)
News$Year = as.factor(grepl('^19', News$Headline)
                      |grepl('18',News$Headline))

News$Patient = as.factor(grepl('patient',tolower(News$Abstract))
                         |grepl('patient',tolower(News$Headline))
)

#News$LowOped
for (j in 1:nrow(News)) {
  i=1
  while (i<=length(state.name))
  {
    News$State[j] = (
      grepl(tolower(state.name[i]), tolower(News$Headline[j]))
      
    )
    i=i+1
    if(News$State[j] == TRUE) {i=999999}
  }
}
News$State = as.factor(News$State==1 & News$Group=='OpEd')

# View(countrycode_data)
# FarWorld=c(tolower(subset(countrycode_data, country.name !='United States')[,1])
#            ,'UK','Russia','Afgan','Arab','Samoa','Bolivia','Bosnia','Brunei','Congo','Iran','Moldova','Micronesia','Vietnam',''
# 
# for (j in 1:nrow(News)) {
#   i=1
#   while (i<=length(OthWorld))
#   {
#     News$Country[j] = grepl(OthWorld[i], tolower(News$Abstract[j]))
#     i=i+1
#     if(News$Country[j] == TRUE) {i=999999}
#   }
# }

#News$Country = as.factor(News$Country==TRUE & News$Group=='OpEd')
News$Geogr = as.factor(grepl('asia', tolower(News$Abstract))
                       |grepl('europ',tolower(News$Abstract))
                       |grepl('afric',tolower(News$Abstract))
                       |grepl('mexic',tolower(News$Abstract))
                       |grepl('canad',tolower(News$Abstract))
                       |grepl('china',tolower(News$Abstract))
                       |grepl('brazil',tolower(News$Abstract))
                       |grepl('argent',tolower(News$Abstract))
)

News$LowInd = as.factor(News$Headline == 'Daily Clip Report'
                        |grepl('^Democrat',News$Snippet)
                        |grepl('^The Daily Gift',News$Headline)
                        |grepl('^What We',News$Headline)
                        |grepl('^Daily Clip Report',News$Headline)
                        |grepl('^Gov',News$Snippet)
                        |grepl('^Highlight',News$Snippet)
                        |grepl('^Hillary',News$Snippet)
                        |News$Headline == 'Tune In to The Times'
                        |grepl('^President Obama',News$Snippet)
                        |grepl('^Representative',News$Snippet)
                        |grepl('^Senator',News$Snippet)
                        |grepl('^Speaker',News$Snippet)
                        |grepl('^The president',News$Snippet)
                        |grepl('^The White House',News$Snippet)
                        |grepl('^Republicans',News$Snippet)
                        
                        #                         |grepl('^Times critics', News$Abstract)
                        #                         |grepl('^Have you been', News$Abstract)
                        #                         |grepl('^This feature looks', News$Abstract)
                        #                         |grepl('^Plus', News$Abstract)
                        #                         |grepl('^6 Q', News$Headline)
                        #                         |grepl('^Word of the Day', News$Headline)
                        #                         |grepl('^New York Today', News$Headline)
                        #                         |grepl('^Photos from', News$Abstract)
                        #                         |grepl('^Over the past week', News$Abstract)
                        #                         |grepl('^On Mondays', News$Abstract)
                        #                         |grepl('^Joe on WNYC', News$Headline)
                        #                         |grepl('^Test Yourself', News$Headline)
                        #                         |grepl('^Weekly News Quiz', News$Headline)
                        #                         |grepl('^What We',News$Headline)
                        #                         |grepl('^From the International Herald',News$Abstract)
                        #                         |grepl('^Friday and the weekend',News$Abstract)
                        #                         |grepl('^A weekly capsule',News$Abstract)
                        #                         |grepl('^A slideshow of arts',News$Abstract)
)
News$Politics = as.factor((grepl('democrat',tolower(News$Abstract))
                           |grepl('republican',tolower(News$Abstract))
                           |grepl('senat',tolower(News$Abstract))
                           |grepl('obama',tolower(News$Abstract))
                           |grepl('hillary',tolower(News$Abstract))
                           |grepl('president',tolower(News$Abstract))
                           |grepl('^election',tolower(News$Abstract))
                           |grepl(' election',tolower(News$Abstract))
                           |grepl('republican',tolower(News$Abstract))
                           |grepl('white House',tolower(News$Abstract))
) & News$NewsDesk!='OpEd')

News$Google = grepl('googl',tolower(News$Abstract))
News$Theater = grepl('theater',tolower(News$Abstract))
News$LowOpEd = as.factor(
  (grepl('music',tolower(News$Headline))
   |grepl('music',tolower(News$Abstract))
   |grepl('Civil War',News$Abstract)
   |grepl('Confederat',News$Abstract)
   |grepl('Union',News$Abstract)
   |grepl('^Joe on WNYC',News$Headline)                
   |grepl('^What We',News$Headline)
  )
  & (News$NewsDesk =='OpEd'))
News$DailyRep = as.factor(
  (grepl('daily', tolower(News$Headline))
   &grepl('report', tolower(News$Headline)))
)
News$TechHigh = as.factor(
  (grepl('Apple', News$Headline)
   |grepl('Apple', News$Abstract)
   |grepl('Amazon', News$Headline)
   |grepl('Amazon', News$Abstract)
   |grepl('Facebook', News$Headline)
   |grepl('Facebook', News$Abstract))
  &News$NewsDesk == 'Business' & News$SectionName == 'Technology'
)
# Creating Train/Test sets
TrainGLM <- cbind(head(News, nrow(NewsTrain)), as.factor(NewsTrain$Popular))
colnames(TrainGLM)[ncol(TrainGLM)] = 'Popular'
TestGLM <- cbind(tail(News, nrow(NewsTest)))

Train <- cbind(head(cbind(News, AbstractWords, HeadlineWords), nrow(NewsTrain)), as.factor(NewsTrain$Popular))
colnames(Train)[ncol(Train)] = 'Popular'
Test  <- cbind(tail(cbind(News, AbstractWords, HeadlineWords), nrow(NewsTest)))
# Cross-validating random forest model with non-zero coefficients to check it's performance
# Parameters:
# data - data frame containing your training data
# model - the function to call to make the model (e.g. rpart, gbm, knn3 etc.)
# pargs - a list of arguments for the predict function
# ... - parameters for the model
# Examples:
# kfold(TrainData,randomForest,pargs=list(type="prob"),nodesize=1,ntree=500)
# kfold(TrainData,rpart,method="class",cp=0.01)
kfold = function(data,model,pargs=NULL,...) {
  require(caret)
  # k is the number of folds, 5 or 10 are common choices
  k = 5
  metrics = list(k)
  means = numeric(5)
  
  set.seed(1)
  folds = createFolds(data$Popular,k=k,list=TRUE,returnTrain=TRUE)
  
  
  for (i in 1:length(folds)) {
    dfT = data[folds[[i]],]
    
    dfP = data[folds[[i]]*-1,]
    
    # change the formula (Popular~.) in the line below if you need to
    set.seed(1)
    m = model(Popular ~ 
                +Headline.accus
              +Headline.acrost
              +Headline.affair
              +Headline.agenda
              +Headline.aig
              +Headline.anoth
              +Headline.answer
              +Headline.babi
              +Headline.bankruptci
              +Headline.boy
              +Headline.brain
              +Headline.breach
              +Headline.busi
              +Headline.california
              +Headline.can
              +Headline.center
              +Headline.children
              +Headline.claim
              +Headline.concern
              +Headline.convers
              +Headline.cook
              +Headline.corpor
              +Headline.count
              +Headline.countri
              +Headline.crisi
              +Headline.crossword
              +Headline.dead
              +Headline.defens
              +Headline.diabet
              +Headline.dinner
              +Headline.discuss
              +Headline.dont
              +Headline.eas
              +Headline.econom
              +Headline.email
              +Headline.empir
              +Headline.energi
              +Headline.exchang
              +Headline.expect
              +Headline.fall
              +Headline.famous
              +Headline.ferguson
              +Headline.fiction
              +Headline.friend
              +Headline.front
              +Headline.game
              +Headline.gas
              +Headline.get
              +Headline.golden
              +Headline.goldman
              +Headline.happen
              +Headline.health
              +Headline.hidden
              +Headline.higher
              +Headline.homeland
              +Headline.inequ
              +Headline.invis
              +Headline.isi
              +Headline.keep
              +Headline.lawsuit
              +Headline.less
              +Headline.let
              +Headline.like
              +Headline.love
              +Headline.marijuana
              +Headline.martin
              +Headline.microsoft
              +Headline.minist
              +Headline.miss
              +Headline.moment
              +Headline.move
              +Headline.network
              +Headline.never
              +Headline.new
              +Headline.payment
              +Headline.perfect
              +Headline.person
              +Headline.phone
              +Headline.point
              +Headline.polic
              +Headline.possibl
              +Headline.premier
              +Headline.prepar
              +Headline.prison
              +Headline.problem
              +Headline.pull
              +Headline.put
              +Headline.quandari
              +Headline.reaction
              +Headline.read
              +Headline.reader
              +Headline.readi
              +Headline.realli
              +Headline.recap
              +Headline.respond
              +Headline.river
              +Headline.robot
              +Headline.role
              +Headline.sach
              +Headline.scotland
              +Headline.seat
              +Headline.seek
              +Headline.sexual
              +Headline.shoot
              +Headline.shot
              +Headline.should
              +Headline.sign
              +Headline.summer
              +Headline.swift
              +Headline.tell
              +Headline.thanksgiv
              +Headline.thought
              +Headline.today
              +Headline.town
              +Headline.uber
              +Headline.updat
              +Headline.vacat
              +Headline.varieti
              +Headline.vegetarian
              +Headline.veteran
              +Headline.vice
              +Headline.victori
              +Headline.voter
              +Headline.wall
              +Headline.want
              +Headline.war
              +Headline.warren
              +Headline.weigh
              +Headline.whi
              +Headline.wife
              +Headline.women
              +Headline.worth
              +Headline.yahoo
              +Headline.yet
              +Abstract.1864
              +Abstract.accus
              +Abstract.action
              +Abstract.actual
              +Abstract.adopt
              +Abstract.age
              +Abstract.agreement
              +Abstract.almost
              +Abstract.america
              +Abstract.among
              +Abstract.answer
              +Abstract.anyon
              +Abstract.appl
              +Abstract.appli
              +Abstract.argu
              +Abstract.awar
              +Abstract.babi
              +Abstract.becom
              +Abstract.believ
              +Abstract.ben
              +Abstract.bit
              +Abstract.blog
              +Abstract.blood
              +Abstract.blunt
              +Abstract.borrow
              +Abstract.bring
              +Abstract.built
              +Abstract.buy
              +Abstract.can
              +Abstract.carl
              +Abstract.caus
              +Abstract.central
              +Abstract.centuri
              +Abstract.chang
              +Abstract.choic
              +Abstract.close
              +Abstract.coast
              +Abstract.comedian
              +Abstract.comfort
              +Abstract.comment
              +Abstract.commiss
              +Abstract.communiti
              +Abstract.competitor
              +Abstract.condit
              +Abstract.conglomer
              +Abstract.conserv
              +Abstract.convict
              +Abstract.corner
              +Abstract.correct
              +Abstract.corrupt
              +Abstract.counti
              +Abstract.coupl
              +Abstract.cox
              +Abstract.cultur
              +Abstract.currenc
              +Abstract.debut
              +Abstract.decad
              +Abstract.decis
              +Abstract.deepli
              +Abstract.deploy
              +Abstract.detect
              +Abstract.devic
              +Abstract.diabet
              +Abstract.diet
              +Abstract.diseas
              +Abstract.disput
              +Abstract.drink
              +Abstract.earlier
              +Abstract.earth
              +Abstract.easier
              +Abstract.educ
              +Abstract.eight
              +Abstract.els
              +Abstract.emot
              +Abstract.encourag
              +Abstract.enough
              +Abstract.entertain
              +Abstract.environ
              +Abstract.epidem
              +Abstract.episod
              +Abstract.equal
              +Abstract.evan
              +Abstract.except
              +Abstract.exercis
              +Abstract.expert
              +Abstract.explor
              +Abstract.express
              +Abstract.fact
              +Abstract.factor
              +Abstract.fall
              +Abstract.far
              +Abstract.fenc
              +Abstract.fish
              +Abstract.flight
              +Abstract.forward
              +Abstract.four
              +Abstract.girl
              +Abstract.googl
              +Abstract.green
              +Abstract.group
              +Abstract.guarante
              +Abstract.hacker
              +Abstract.harass
              +Abstract.hard
              +Abstract.heat
              +Abstract.hillari
              +Abstract.ignor
              +Abstract.ill
              +Abstract.impos
              +Abstract.incid
              +Abstract.increas
              +Abstract.index
              +Abstract.infect
              +Abstract.inflat
              +Abstract.initi
              +Abstract.invis
              +Abstract.ive
              +Abstract.joan
              +Abstract.know
              +Abstract.languag
              +Abstract.larg
              +Abstract.latest
              +Abstract.lawyer
              +Abstract.leagu
              +Abstract.legisl
              +Abstract.liber
              +Abstract.link
              +Abstract.live
              +Abstract.local
              +Abstract.male
              +Abstract.man
              +Abstract.marriag
              +Abstract.match
              +Abstract.may
              +Abstract.mayor
              +Abstract.measur
              +Abstract.mind
              +Abstract.minor
              +Abstract.modern
              +Abstract.music
              +Abstract.mysteri
              +Abstract.nativ
              +Abstract.negoti
              +Abstract.note
              +Abstract.object
              +Abstract.observ
              +Abstract.parent
              +Abstract.particular
              +Abstract.paul
              +Abstract.pennsylvania
              +Abstract.phone
              +Abstract.play
              +Abstract.plenti
              +Abstract.poor
              +Abstract.posit
              +Abstract.power
              +Abstract.prais
              +Abstract.previous
              +Abstract.prison
              +Abstract.profit
              +Abstract.prosecut
              +Abstract.prosecutor
              +Abstract.quit
              +Abstract.rape
              +Abstract.rate
              +Abstract.reaction
              +Abstract.read
              +Abstract.reader
              +Abstract.recal
              +Abstract.recent
              +Abstract.recip
              +Abstract.reduc
              +Abstract.regul
              +Abstract.remind
              +Abstract.requir
              +Abstract.reserv
              +Abstract.resourc
              +Abstract.respons
              +Abstract.retrospect
              +Abstract.rival
              +Abstract.root
              +Abstract.rush
              +Abstract.said
              +Abstract.salad
              +Abstract.sanction
              +Abstract.say
              +Abstract.school
              +Abstract.scotland
              +Abstract.sensit
              +Abstract.septemb
              +Abstract.serious
              +Abstract.share
              +Abstract.sharp
              +Abstract.signal
              +Abstract.small
              +Abstract.smartphon
              +Abstract.smith
              +Abstract.solv
              +Abstract.sound
              +Abstract.stay
              +Abstract.street
              +Abstract.suggest
              +Abstract.system
              +Abstract.tech
              +Abstract.tell
              +Abstract.theyr
              +Abstract.think
              +Abstract.thought
              +Abstract.tie
              +Abstract.treat
              +Abstract.treatment
              +Abstract.trillion
              +Abstract.troubl
              +Abstract.truth
              +Abstract.tuesday
              +Abstract.understand
              +Abstract.unfair
              +Abstract.unusu
              +Abstract.updat
              +Abstract.vice
              +Abstract.violenc
              +Abstract.virus
              +Abstract.vision
              +Abstract.wall
              +Abstract.warren
              +Abstract.wear
              +Abstract.wearabl
              +Abstract.when
              +Abstract.worker
              +Abstract.your
              +TechHigh
              +DailyRep
              +LowOpEd
              +Politics
              +LowInd
              +Geogr
              +Patient
              +RFD
              +StyleUS
              +Cross
              +Qmark
              +Group
              +False
              +Woman
              +Agressive
              +EduHigh2
              +EduHigh
              +Weekday
              +Hour
              +SubsectionName
              +SectionName
              +NewsDesk
              +WordCount
              +HeadlineLen
              +AbstractLen
              ,data=dfT,...)
    gc()
    set.seed(1)
    p = do.call(predict,c(list(object=m,newdata=dfP),pargs))
    gc()
    
    #      AnalErrCV = as.data.frame(cbind(dfP$UniqueID, (p[,2])))
    #      write.csv(AnalErrCV, paste("ErrCV",i,".csv"), row.names=FALSE)
    #     
    
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
    else if (min(predictions) > cutoff)  {
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
start = Sys.time()
kfold (data = Train,model = randomForest,pargs=list(type="prob"),ntree = 10000, nodesize = 1)
print(Sys.time() - start)

#Training the final model and building predictions for Test set
set.seed(1)
RF1 = randomForest (Popular ~ 
                      +Headline.accus
                    +Headline.acrost
                    +Headline.affair
                    +Headline.agenda
                    +Headline.aig
                    +Headline.anoth
                    +Headline.answer
                    +Headline.babi
                    +Headline.bankruptci
                    +Headline.boy
                    +Headline.brain
                    +Headline.breach
                    +Headline.busi
                    +Headline.california
                    +Headline.can
                    +Headline.center
                    +Headline.children
                    +Headline.claim
                    +Headline.concern
                    +Headline.convers
                    +Headline.cook
                    +Headline.corpor
                    +Headline.count
                    +Headline.countri
                    +Headline.crisi
                    +Headline.crossword
                    +Headline.dead
                    +Headline.defens
                    +Headline.diabet
                    +Headline.dinner
                    +Headline.discuss
                    +Headline.dont
                    +Headline.eas
                    +Headline.econom
                    +Headline.email
                    +Headline.empir
                    +Headline.energi
                    +Headline.exchang
                    +Headline.expect
                    +Headline.fall
                    +Headline.famous
                    +Headline.ferguson
                    +Headline.fiction
                    +Headline.friend
                    +Headline.front
                    +Headline.game
                    +Headline.gas
                    +Headline.get
                    +Headline.golden
                    +Headline.goldman
                    +Headline.happen
                    +Headline.health
                    +Headline.hidden
                    +Headline.higher
                    +Headline.homeland
                    +Headline.inequ
                    +Headline.invis
                    +Headline.isi
                    +Headline.keep
                    +Headline.lawsuit
                    +Headline.less
                    +Headline.let
                    +Headline.like
                    +Headline.love
                    +Headline.marijuana
                    +Headline.martin
                    +Headline.microsoft
                    +Headline.minist
                    +Headline.miss
                    +Headline.moment
                    +Headline.move
                    +Headline.network
                    +Headline.never
                    +Headline.new
                    +Headline.payment
                    +Headline.perfect
                    +Headline.person
                    +Headline.phone
                    +Headline.point
                    +Headline.polic
                    +Headline.possibl
                    +Headline.premier
                    +Headline.prepar
                    +Headline.prison
                    +Headline.problem
                    +Headline.pull
                    +Headline.put
                    +Headline.quandari
                    +Headline.reaction
                    +Headline.read
                    +Headline.reader
                    +Headline.readi
                    +Headline.realli
                    +Headline.recap
                    +Headline.respond
                    +Headline.river
                    +Headline.robot
                    +Headline.role
                    +Headline.sach
                    +Headline.scotland
                    +Headline.seat
                    +Headline.seek
                    +Headline.sexual
                    +Headline.shoot
                    +Headline.shot
                    +Headline.should
                    +Headline.sign
                    +Headline.summer
                    +Headline.swift
                    +Headline.tell
                    +Headline.thanksgiv
                    +Headline.thought
                    +Headline.today
                    +Headline.town
                    +Headline.uber
                    +Headline.updat
                    +Headline.vacat
                    +Headline.varieti
                    +Headline.vegetarian
                    +Headline.veteran
                    +Headline.vice
                    +Headline.victori
                    +Headline.voter
                    +Headline.wall
                    +Headline.want
                    +Headline.war
                    +Headline.warren
                    +Headline.weigh
                    +Headline.whi
                    +Headline.wife
                    +Headline.women
                    +Headline.worth
                    +Headline.yahoo
                    +Headline.yet
                    +Abstract.1864
                    +Abstract.accus
                    +Abstract.action
                    +Abstract.actual
                    +Abstract.adopt
                    +Abstract.age
                    +Abstract.agreement
                    +Abstract.almost
                    +Abstract.america
                    +Abstract.among
                    +Abstract.answer
                    +Abstract.anyon
                    +Abstract.appl
                    +Abstract.appli
                    +Abstract.argu
                    +Abstract.awar
                    +Abstract.babi
                    +Abstract.becom
                    +Abstract.believ
                    +Abstract.ben
                    +Abstract.bit
                    +Abstract.blog
                    +Abstract.blood
                    +Abstract.blunt
                    +Abstract.borrow
                    +Abstract.bring
                    +Abstract.built
                    +Abstract.buy
                    +Abstract.can
                    +Abstract.carl
                    +Abstract.caus
                    +Abstract.central
                    +Abstract.centuri
                    +Abstract.chang
                    +Abstract.choic
                    +Abstract.close
                    +Abstract.coast
                    +Abstract.comedian
                    +Abstract.comfort
                    +Abstract.comment
                    +Abstract.commiss
                    +Abstract.communiti
                    +Abstract.competitor
                    +Abstract.condit
                    +Abstract.conglomer
                    +Abstract.conserv
                    +Abstract.convict
                    +Abstract.corner
                    +Abstract.correct
                    +Abstract.corrupt
                    +Abstract.counti
                    +Abstract.coupl
                    +Abstract.cox
                    +Abstract.cultur
                    +Abstract.currenc
                    +Abstract.debut
                    +Abstract.decad
                    +Abstract.decis
                    +Abstract.deepli
                    +Abstract.deploy
                    +Abstract.detect
                    +Abstract.devic
                    +Abstract.diabet
                    +Abstract.diet
                    +Abstract.diseas
                    +Abstract.disput
                    +Abstract.drink
                    +Abstract.earlier
                    +Abstract.earth
                    +Abstract.easier
                    +Abstract.educ
                    +Abstract.eight
                    +Abstract.els
                    +Abstract.emot
                    +Abstract.encourag
                    +Abstract.enough
                    +Abstract.entertain
                    +Abstract.environ
                    +Abstract.epidem
                    +Abstract.episod
                    +Abstract.equal
                    +Abstract.evan
                    +Abstract.except
                    +Abstract.exercis
                    +Abstract.expert
                    +Abstract.explor
                    +Abstract.express
                    +Abstract.fact
                    +Abstract.factor
                    +Abstract.fall
                    +Abstract.far
                    +Abstract.fenc
                    +Abstract.fish
                    +Abstract.flight
                    +Abstract.forward
                    +Abstract.four
                    +Abstract.girl
                    +Abstract.googl
                    +Abstract.green
                    +Abstract.group
                    +Abstract.guarante
                    +Abstract.hacker
                    +Abstract.harass
                    +Abstract.hard
                    +Abstract.heat
                    +Abstract.hillari
                    +Abstract.ignor
                    +Abstract.ill
                    +Abstract.impos
                    +Abstract.incid
                    +Abstract.increas
                    +Abstract.index
                    +Abstract.infect
                    +Abstract.inflat
                    +Abstract.initi
                    +Abstract.invis
                    +Abstract.ive
                    +Abstract.joan
                    +Abstract.know
                    +Abstract.languag
                    +Abstract.larg
                    +Abstract.latest
                    +Abstract.lawyer
                    +Abstract.leagu
                    +Abstract.legisl
                    +Abstract.liber
                    +Abstract.link
                    +Abstract.live
                    +Abstract.local
                    +Abstract.male
                    +Abstract.man
                    +Abstract.marriag
                    +Abstract.match
                    +Abstract.may
                    +Abstract.mayor
                    +Abstract.measur
                    +Abstract.mind
                    +Abstract.minor
                    +Abstract.modern
                    +Abstract.music
                    +Abstract.mysteri
                    +Abstract.nativ
                    +Abstract.negoti
                    +Abstract.note
                    +Abstract.object
                    +Abstract.observ
                    +Abstract.parent
                    +Abstract.particular
                    +Abstract.paul
                    +Abstract.pennsylvania
                    +Abstract.phone
                    +Abstract.play
                    +Abstract.plenti
                    +Abstract.poor
                    +Abstract.posit
                    +Abstract.power
                    +Abstract.prais
                    +Abstract.previous
                    +Abstract.prison
                    +Abstract.profit
                    +Abstract.prosecut
                    +Abstract.prosecutor
                    +Abstract.quit
                    +Abstract.rape
                    +Abstract.rate
                    +Abstract.reaction
                    +Abstract.read
                    +Abstract.reader
                    +Abstract.recal
                    +Abstract.recent
                    +Abstract.recip
                    +Abstract.reduc
                    +Abstract.regul
                    +Abstract.remind
                    +Abstract.requir
                    +Abstract.reserv
                    +Abstract.resourc
                    +Abstract.respons
                    +Abstract.retrospect
                    +Abstract.rival
                    +Abstract.root
                    +Abstract.rush
                    +Abstract.said
                    +Abstract.salad
                    +Abstract.sanction
                    +Abstract.say
                    +Abstract.school
                    +Abstract.scotland
                    +Abstract.sensit
                    +Abstract.septemb
                    +Abstract.serious
                    +Abstract.share
                    +Abstract.sharp
                    +Abstract.signal
                    +Abstract.small
                    +Abstract.smartphon
                    +Abstract.smith
                    +Abstract.solv
                    +Abstract.sound
                    +Abstract.stay
                    +Abstract.street
                    +Abstract.suggest
                    +Abstract.system
                    +Abstract.tech
                    +Abstract.tell
                    +Abstract.theyr
                    +Abstract.think
                    +Abstract.thought
                    +Abstract.tie
                    +Abstract.treat
                    +Abstract.treatment
                    +Abstract.trillion
                    +Abstract.troubl
                    +Abstract.truth
                    +Abstract.tuesday
                    +Abstract.understand
                    +Abstract.unfair
                    +Abstract.unusu
                    +Abstract.updat
                    +Abstract.vice
                    +Abstract.violenc
                    +Abstract.virus
                    +Abstract.vision
                    +Abstract.wall
                    +Abstract.warren
                    +Abstract.wear
                    +Abstract.wearabl
                    +Abstract.when
                    +Abstract.worker
                    +Abstract.your
                    +TechHigh
                    +DailyRep
                    +LowOpEd
                    +Politics
                    +LowInd
                    +Geogr
                    +Patient
                    +RFD
                    +StyleUS
                    +Cross
                    +Qmark
                    +Group
                    +False
                    +Woman
                    +Agressive
                    +EduHigh2
                    +EduHigh
                    +Weekday
                    +Hour
                    +SubsectionName
                    +SectionName
                    +NewsDesk
                    +WordCount
                    +HeadlineLen
                    +AbstractLen
                    , data = Train, ntree = 10000, nodesize = 1
) 
options(scipen=9999)
PredTest = predict(RF1, newdata=Test, type="prob")[,2]
MySubmission = data.frame(UniqueID = Test$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionRF71.csv", row.names=FALSE)



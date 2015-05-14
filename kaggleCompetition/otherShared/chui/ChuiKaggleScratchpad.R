# install.packages("randomForest")
library(randomForest)
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)
# install.packages("ROCR")
library(ROCR)
library(rpart)
library(rpart.plot) 
# install.packages("stringr")
library(stringr)
# install.packages("car")
library(car)

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTest$Popular<-NA
News=rbind(NewsTrain,NewsTest)
News$HandA<-paste(News$Headline,News$Abstract)
News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")
News$yday = News$PubDate$yday
News$Mday = News$PubDate$Mday
News$Mon = News$PubDate$mon-10
News$Hour = News$PubDate$hour
News$Weekday = News$PubDate$wday
News$Hour=as.factor(News$Hour)
News$weekday=as.factor(News$Weekday)

News$PopularInt=News$Popular
News$Popular=as.factor(News$Popular)

News$HeadlineQuestion=as.integer(grepl("?",News$Headline,fixed=TRUE))
News$HeadlineQuestion=as.factor(News$HeadlineQuestion)
News$AbstractQuestion=as.integer(grepl("?",News$Abstract,fixed=TRUE))
News$AbstractQuestion=as.factor(News$AbstractQuestion)

News$HeadlineLength=nchar(News$Headline)
News$AbstractLength=nchar(News$Abstract)
News$LogWordCount=log(News$WordCount+1)

# normalize
minWC=min(News$WordCount)
maxWC=max(News$WordCount)
News$NORMWordCount=(News$WordCount-minWC)/(maxWC-minWC)

minHL=min(News$HeadlineLength)
maxHL=max(News$HeadlineLength)
News$NORMHeadlineLength=(News$HeadlineLength-minHL)/(maxHL-minHL)

minAL=min(News$AbstractLength)
maxAL=max(News$AbstractLength)
News$NORMAbstractLength=(News$AbstractLength-minAL)/(maxAL-minAL)

minWC=min(News$WordCount)
maxWC=max(News$WordCount)
News$NORMWordCount=(News$WordCount-minWC)/(maxWC-minWC)
hist(News$NORMHeadlineLength)

# picture captions and other tiny snippets are really not substantive articles,
# they have 0 Populars and logistic will otherwise favor shortness
# many but not all of these are captured in RegularBlogs/UnpopularRegularBlogs
News$Short=as.factor(News$LogWordCount<1.8)

# there's correlated cluster in word count vs. abstract length which is likely at Abstract 
# just repeated as text, also not Popular
# not sure my attempt to count words in abstract will ever quite match the original word count 
# though...
News$AbstractWordCount=sapply(gregexpr("\\b\\W+\\b", News$Abstract, perl=TRUE), function(x) sum(x>0) ) + 1
minAWC=min(News$AbstractWordCount)
maxAWC=max(News$AbstractWordCount)
News$NORMAbstractWordCount=(News$AbstractWordCount-minAWC)/(maxAWC-minAWC)

hist(News$NORMAbstractWordCount)

#it's close!
News$AlternativeShort<-as.factor(News$WordCount<(News$AbstractWordCount+2))
News$AlternativeShort2<-as.factor(News$WordCount<(News$AbstractWordCount*4))
#hmm, maybe not needed like I thought as already have linear combinations of word count and lengths?

News$NewsType<-str_trim(paste(News$NewsDesk,News$SectionName,News$SubsectionName))
table(News$NewsType)
News$NewsType<-recode(News$NewsType,"'Open'='none';'Sports'='none';'Sports Sports'='none';'Travel Travel'='Travel' ;'Culture Arts'='Culture';'Foreign'='Foreign World';'Magazine Magazine'='Magazine';
       'National'='U.S.';'National U.S. Politics'='U.S.';'OpEd Opinion'='OpEd';'Opinion'='OpEd';'Science Health'='Science'; 'Styles Health'='Styles'; 'Styles Style Fashion & Style'='Styles';'N.Y. / Region'='Metro N.Y. / Region';
                      'Arts'='Culture';'Business Business Day Small Business'='Business Day Small Business';'Business'='Business Day Dealbook';'Business Business Day Dealbook'='Business Day Dealbook';'World Asia Pacific'='Foreign World';'Health'='Science';'Business Crosswords/Games'='Crosswords/Games';'Technology'='Business Technology'",as.factor.result=TRUE)
table(News$NewsType)

#words 
#business is a category that has a medium number of popular items seemingly not well caught 
# by RegularBlogs
News$Facebook = as.factor(ifelse(grepl("Facebook",News$Headline,ignore.case=FALSE),1,0))
News$Apple = as.factor(ifelse(grepl("Apple",News$Headline,ignore.case=FALSE),1,0))
News$WallStreet=as.factor(ifelse(grepl("Wall St",News$HandA,ignore.case=FALSE),1,0))
News$Asking=as.factor(ifelse(grepl("you think|Tell us|Tell Us|Do you|Do You|your comments|You Think",News$HandA,ignore.case=FALSE),1,0))
#tried these next, not that useful
News$Race=as.factor(ifelse(grepl("Race|racist|Garner|Ferguson|Ramos|black Americans|African-American|blacks|white privilege|white police|black men|black students|racial|racism|white Americans|Racist|Racism|Black Americans|Blacks|Whites|Racial|Black Students",News$HandA,ignore.case=FALSE),1,0))
News$Controversy=as.factor(ifelse(grepl("conflict|furor|outrage|outcry|violence|protests|uproar|partisan|provoked|readers protest|shocked|turmoil|controversy|crisis|riot",News$HandA,ignore.case=FALSE),1,0))
News$Asking=as.factor(ifelse(grepl("you think|Tell us|Tell Us|Do you|Do You|your comments|You Think",News$HandA,ignore.case=FALSE),1,0))
#these next seem popular even if not a question?
News$HowWhatWhenWhy=as.factor(ifelse(substr(News$Headline,1,3)=='How'|substr(News$Headline,1,3)=='Why'|substr(News$Headline,1,4)=='What'|substr(News$Headline,1,4)=='When',1,0))

#Regular Blogs
#These should be useful because I can see them distinquishing popularity within NewsType- a subcategory.
#should I have one big column and let the forest have at it?
#My first idea was to lump unpopular (never popular) ones as one category and let the others (ones sometimes popular) be factors
#but maybe one big column like a subcategory is better?  - NO, MODELS HATED THIS REGULAR BLOGS BUT NOT SURE WHY (unpopular regular blogs with just 2 options was very useful I think)
#These were repeated headlines, repeated things before a colon, repeated things before | 
#and  repeated patterns at the start of Headlines
# | was a problem, could not treat like colon even with escape (?) but those were all in Education category which was 0 popularity already so I ignored them (6Q's, Word of the Day, etc)
# also turned out to be a limit on the number of categories forest will handle
# Would this have worked better if I had made a single column for each regular blog and let the forest have at that?

News$YourTurn <- as.factor(ifelse(grepl("Your Turn:",News$Headline,ignore.case=FALSE),1,0))
News$ThinkLikeaDoctor <- as.factor(ifelse(grepl("Think Like a Doctor",News$Headline,ignore.case=FALSE),1,0))
News$Quandary <- as.factor(ifelse(grepl("Quandary",News$Headline,ignore.case=FALSE),1,0))
News$ReadersRespond <- as.factor(ifelse(grepl("Readers Respond",News$Headline,ignore.case=FALSE),1,0))
News$NewYork <- as.factor(ifelse(grepl("New York",News$Headline,ignore.case=FALSE),1,0))
News$FandF <- as.factor(ifelse(grepl("Facts & Figures",News$Headline,ignore.case=FALSE),1,0))
News$NoComment <- as.factor(ifelse(grepl("No Comment Necessary",News$Headline,ignore.case=FALSE),1,0))
News$TV <- as.factor(ifelse(grepl("' Recap",News$Headline,ignore.case=FALSE),1,0))
News$AskWell <- as.factor(ifelse(grepl("Ask Well",News$Headline,ignore.case=FALSE),1,0))
News$TVnewsroom <- as.factor(ifelse(grepl("'The Newsroom' Recap",News$Headline,ignore.case=FALSE),1,0))
News$TVaffair <- as.factor(ifelse(grepl("'The Affair' Recap",News$Headline,ignore.case=FALSE),1,0))
News$TVhomeland <- as.factor(ifelse(grepl("'Homeland' Recap",News$Headline,ignore.case=FALSE),1,0))
News$CivilWar <- ifelse(grepl("Civil War|Confederate|Union soldier|Union officer|John Bell Hood",News$HandA,ignore.case=FALSE),1,0)
News$ReadingWith<-ifelse(grepl("Reading The Times With|Reading the Times With|Reading the Paper With",News$Headline,ignore.case=FALSE),1,0)
News$Archive<-ifelse(grepl("Herald Tribune archive",News$Abstract,ignore.case=FALSE),1,0)
News$ArtsHappenings<-ifelse(grepl("International Arts Events",News$Headline,ignore.case=FALSE),1,0)
News$DailyClip<-ifelse(grepl("Daily Clip Report",News$Headline,ignore.case=FALSE),1,0)
News$Fashion<-ifelse(grepl("Fashion Week",News$Headline,ignore.case=FALSE),1,0)
News$Joe<-ifelse(grepl("Joe on WNYC",News$Headline,ignore.case=FALSE),1,0)
News$Reading<-ifelse(grepl("What We're Reading|What Were Reading",News$Headline,ignore.case=FALSE),1,0)
News$Tune<-ifelse(grepl("Tune In to The Times|Tune Into The Times",News$Headline,ignore.case=FALSE),1,0)
News$Wrap<-ifelse(grepl("Weekly Wrap",News$Headline,ignore.case=FALSE),1,0)
News$NYT<-ifelse(grepl("New York Today",News$Headline,ignore.case=FALSE),1,0)
News$TodayIn<-ifelse(substr(News$Headline,1,8)=="Today in",1,0)
News$Remembering=ifelse(grepl("Remembering",News$Headline,ignore.case=FALSE),1,0)
News$Pictures=ifelse(grepl("Photos of the Day|Pictures of the Day|Week in Pictures",News$Headline,ignore.case=FALSE),1,0)
News$Cryptic=ifelse(grepl("Variety: Cryptic Crossword",News$Headline,ignore.case=FALSE),1,0)
News$TTop=ifelse(grepl("Top 10|From T's First 10 Years",News$Headline,ignore.case=FALSE),1,0)
News$videoreviews=ifelse(grepl("Video Reviews of",News$Headline,ignore.case=FALSE),1,0)
News$Historical=ifelse(!is.na(as.numeric(substr(News$Headline,1,4)))&as.numeric(substr(News$Headline,1,4))>1700&as.numeric(substr(News$Headline,1,4))<2014,1,0)


News$TTop[substr(News$Headline,1,2)=="10"]<-1
subset(News$Headline,News$TTop>0)
News$BC<-substr(News$Headline,1,regexpr(":",News$Headline)-1)
News$BC<-gsub("'","_",News$BC)
test=as.data.frame(table(News$BC))
write.csv(test, "beforecolon.csv", row.names=FALSE)
subset(News$Headline,News$TTop>0)

News$UnpopularRegularBlogs<-recode(News$BC,"'Weekend Reading'='1' ; 'Under Cover'='1';'Weekly Wrap'='1';'What We_re Watching'='1';'Analytics'='1';'Lunchtime Laughs'='1';'New York City_s Week in Pictures'='1';'Politics Helpline'='1';'Tune In to The Times'='1';'Behind the Cover Story'='1';
        'Behind the Poster'='1';'Classical Playlist'='1';'Popcast'='1';'Friday Night Music'='1';'Book Review Podcast'='1';'International Arts Events Happening in the Week Ahead'='1';
        'From the Upshot'='1';'From The Upshot'='1';'Walkabout'='1';'In Performance'='1';'London Fashion Week'='1';'First Draft Video'='1';'On This Day'='1';'Pictures of the Day'='1';'Q. and A.'='1';'Milan Fashion Week'='1';'The Daily Gift'='1';'New York Fashion Week'='1';'Verbatim'='1';'Paris Fashion Week'='1';'What We_re Reading'='1';'First Draft Focus'='1';'Today in Politics'='1';'Today in Small Business'='1';'Daily Report'='1';'Daily Clip Report'='1';'Morning Agenda'='1';'Variety'='1';else='0'")
News$UnpopularRegularBlogs[News$Cryptic==1|News$NYT==1]<-'0'
News$UnpopularRegularBlogs[News$Joe==1|News$Pictures==1|News$videoreviews==1|News$Fashion==1|News$DailyClip==1|News$ArtsHappenings==1|News$TodayIn==1|News$Archive==1|News$Tune==1|News$Reading==1|News$Wrap==1|News$ReadingWith==1|News$Tune==1]<-'1'
News$RegularBlogs<-recode(News$BC,"'Behind the Poster'='Behind the';
                                                      'Inside the Times 100'='Inside the Times 100';
                                                      'Photos of the Day'='Pictures';
                                                      'What We_re Reading'='What We_re';
                                                      'From the Upshot'='From The Upshot';
                                                      'Quandary'='Quandary';
                                                      'Living With Cancer'='Living With Cancer';
                                                      'Weekend Reading'='Weekend Reading';
                                                      'Under Cover'='Under Cover';
                                                      'Weekly Quandary'='Weekly Quandary';
                                                      'Analytics'='Analytics';
                                                      'Lunchtime Laughs'='Lunchtime Laughs';
                                                      'Politics Helpline'='Politics Helpline';
                                                      'Behind the Cover Story'='Behind the';
                                                      'Facts & Figures'='Facts & Figures';
                                                      'Think Like a Doctor'='Think Like a Doctor';
                                                      'What We_re Watching'='What We_re';
                                                      'No Comment Necessary'='No Comment Necessary';
                                                      'Popcast'='Popcast';
                                                      'Classical Playlist'='Classical Playlist';
                                                      'First Draft Video'='First Draft';
                                                      'Friday Night Music'='Friday Night Music';
                                                      'Your Turn'='Your Turn';
                                                      'Book Review Podcast'='Book Review Podcast';
                                                      'From The Upshot'='From The Upshot';
                                                      'Variety'='Variety';
                                                      'Ask Well'='Ask Well';
                                                      'On This Day'='History';
                                                      'Readers Respond'='Readers Respond';
                                                      'Walkabout'='Walkabout';
                                                      'In Performance'='In Performance';
                                                      'Pictures of the Day'='Pictures';
                                                      'Q. and A.'='Q. and A.';
                                                      'Verbatim'='Verbatim';
                                                      'The Daily Gift'='The Daily Gift';
                                                      'First Draft Focus'='First Draft';
                                                      'Today in Small Business'='Today in';
                                                      'Daily Report'='Daily Report';
                                                      'Morning Agenda'='Morning Agenda';
                                                      'New York Today'='New York Today';else='NO'")
#now from the things without colon or otherwise needing consolidation
News$RegularBlogs[News$Ttop==1]<-'Top Ten of Something'
News$RegularBlogs[News$Cryptic==1]<-'NO'
News$RegularBlogs[News$Tune==1]<-'Tune In To The Times'
News$RegularBlogs[News$Wrap==1]<-'Weekly Wrap'
News$RegularBlogs[News$Quandary==1]<-'Quandary'
News$RegularBlogs[News$Joe==1]<-'Joe on WNYC'
News$RegularBlogs[News$TV==1]<-'TV recap'
News$RegularBlogs[News$TVaffair==1]<-'TV affair'
News$RegularBlogs[News$TVnewsroom==1]<-'TV newsroom'
News$RegularBlogs[News$TVhomeland==1]<-'TV homeland'
News$RegularBlogs[News$CivilWar==1]<-'Civil War'
News$RegularBlogs[News$DailyClip==1]<-'Daily Clip'
News$RegularBlogs[News$ArtsHappenings==1]<-'Arts Happenings'
News$RegularBlogs[News$Fashion==1]<-'Fashion Week'
News$RegularBlogs[News$Archive==1]<-'History'
News$RegularBlogs[News$ReadingWith==1]<-'Reading the Times With...'
News$RegularBlogs[News$Reading==1]<-'What We_re'
News$RegularBlogs[News$TodayIn==1]<-'Today in'
News$RegularBlogs[News$Pictures==1]<-'Pictures'
News$RegularBlogs[News$VideoRreviews==1]<-'Video'
News$RegularBlogs[News$Historical==1&News$RegularBlogs=='none']<-'History'
table(News$RegularBlogs,News$NewsType)
#impute some NewsTypes 
News$NewsType[News$NewsType=='']<-'none'

News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Fashion Week']<-'TStyle'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Pictures']<-'Multimedia'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Readers Respond']<-'OpEd'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='The Daily Gift']<-'TStyle'
News$NewsType[News$RegularBlogs=='What We_re']<-'Culture'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Reading the Times With...']<-'Culture'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Politics Helpline']<-'U.S.'
News$NewsType[News$NewsType=='none'&News$RegularBlogs=='Today in']<-'U.S.'
News$NewsType[grepl("Today in Politics",News$Headline,ignore.case=TRUE)]<-'U.S.'

News$UnpopularRegularBlogs<-as.factor(News$UnpopularRegularBlogs)
News$RegularBlogs<-as.factor(News$RegularBlogs)
News$NewsType<-as.factor(News$NewsType)

NewsTrain = head(News, nrow(NewsTrain))
NewsTest = tail(News, nrow(NewsTest))

#write files back out to save
write.csv(NewsTrain, "NewsTrainNEW.csv", row.names=FALSE)
write.csv(NewsTest, "NewsTestNEW.csv", row.names=FALSE)
write.csv(News, "News.csv", row.names=FALSE)

#didn't have enough time for this but tried modeling just categories that had a lot of oppotunity - signiificant but not 100% popularity
NewsTrainBus=subset(NewsTrain,NewsType=="Business Day Small Business"|NewsType=="Business Day Dealbook"|NewsType=="Business Day Technology")
NewsTrainop=subset(NewsTrain,NewsType=="OpEd"|NewsType=="Opinion The Public Editor")

#logistic regression - without Popular as a factor
SimpleMod = glm(PopularInt ~ LogWordCount+weekday+Hour+HeadlineQuestion+HeadlineLength+AbstractLength+NewsType+Facebook+Apple+UnpopularRegularBlogs+HowWhatWhenWhy+
                  AbstractQuestion+Quandary+ReadersRespond+CivilWar+TVaffair+TVhomeland+WallStreet+Asking, data=NewsTrain, family=binomial)
# And then make predictions on the test set:
PredTest = predict(SimpleMod, newdata=NewsTest, type="response")
PredTrain = predict(SimpleMod, newdata=NewsTrain, type="response")
tbl=table(NewsTrain$PopularInt,PredTrain>.5)
sum( diag(tbl) ) / nrow(NewsTrain)
predROCR = prediction(PredTrain, NewsTrain$PopularInt)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
summary(SimpleMod)

#  prep submission
PredOp=PredTest
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTestBus)
str(MySubmission)
b=MySubmission$Probability1>0
MySubmission$Probability1=ifelse(b,MySubmission$Probability1,0)
summary(MySubmission)
write.csv(MySubmission, "April30LOG.csv", row.names=FALSE)


#random forest- with Popular as factor
forest = randomForest(Popular ~   NORMWordCount+weekday+Hour+Mon+HeadlineQuestion+NORMHeadlineLength+NewsType+UnpopularRegularBlogs+
                         TV+HowWhatWhenWhy+Quandary+Apple+Facebook+TVaffair+TVhomeland+TVnewsroom, data=NewsTrain,ntree=500,importance=TRUE)
PredTest = predict(forest, newdata=NewsTest,type="prob")[,2]
PredTrain= predict(forest, newdata=NewsTrain, type="prob")[,2]
tbl=table(NewsTrain$Popular,PredTrain>.5)
sum( diag(tbl) ) / nrow(NewsTrain)
predROCR = prediction(PredTrain, NewsTrain$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

#  prep submission
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
str(MySubmission)
b=MySubmission$Probability1>0
MySubmission$Probability1=ifelse(b,MySubmission$Probability1,0)
summary(MySubmission)
write.csv(MySubmission, "FOREST.csv", row.names=FALSE)

varImpPlot(forest)



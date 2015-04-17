setwd("D:/doc/study/TheAnalyticsEdge/unit2")
pisaTrain <-read.csv("pisa2009train.csv")
str(pisaTrain )
pisaTest <-read.csv("pisa2009test.csv")
str(pisaTest )

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore~., data=pisaTrain)
summary(lmScore)

SSE<-sum(lmScore$residuals^2)
RMSE<-sqrt(SSE/nrow(pisaTrain))

predTest<-predict(lmScore, newdata=pisaTest)
summary(predTest)
max(predTest)-min(predTest)

SSETest<-sum((predTest - pisaTest$readingScore)^2)
RMSETest<-sqrt(SSETest/nrow(pisaTest))

SSTTest<-sum((mean(pisaTrain$readingScore) -  pisaTest$readingScore)^2)

R2Test<- 1 - SSETest/SSTTest

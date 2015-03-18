setwd("D:/doc/study/TheAnalyticsEdge/unit2")
FluTrain <-read.csv("FluTrain.csv")
str(FluTrain )
FluTest <-read.csv("FluTest.csv")

FluTrain[which.max(FluTrain$ILI),]
FluTrain[which.max(FluTrain$Queries),]

hist(FluTrain$ILI, breaks=50)

plot( FluTrain$Queries, log(FluTrain$ILI))

FluTrend1<-lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

correlation<-cor(log(FluTrain$ILI), FluTrain$Queries)
correlation^2
log(1/correlation)
exp(-0.5*correlation)

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
pred<-PredTest1[which(FluTest$Week=="2012-03-11 - 2012-03-17")]
obs<-FluTest[which(FluTest$Week=="2012-03-11 - 2012-03-17"),"ILI"]
(obs - pred)/obs

SSE<-sum((PredTest1-FluTest$ILI)^2)
RMSE<-sqrt(SSE/nrow(FluTest))


library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2<-lm(log(ILI)~log(ILILag2)+Queries, data= FluTrain)
summary(FluTrend2)

ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2<-sum((PredTest2-FluTest$ILI)^2)
RMSE2<-sqrt(SSE2/nrow(FluTest))


setwd('D:/workspace/The Analytics Edge/finalExam')

# 1

Airlines = read.csv('AirlineDelay.csv')

set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]

nrow(AirlinesTrain)
nrow(AirlinesTest)

# 3
lmAll<-lm(TotalDelay~., data = AirlinesTrain)
summary(lmAll)

# 5
cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)

cor(AirlinesTrain$OriginAvgWind , AirlinesTrain$OriginWindGust)

# 7
summary(lmAll)


# 10
pred.lmAll = predict(lmAll, newdata=AirlinesTest)

SSE = sum((pred.lmAll - AirlinesTest$TotalDelay)^2)
SST = sum((mean(AirlinesTrain$TotalDelay) - AirlinesTest$TotalDelay)^2)

R-squared = 1-SSE/SST

# 12
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

summary(Airlines$DelayClass)

Airlines$TotalDelay = NULL

set.seed(15071)
spl = sample.split(Airlines$DelayClass, SplitRatio=0.7)
AirlinesTrain = subset(Airlines, spl == TRUE)
AirlinesTest = subset(Airlines, spl == FALSE)

# 13
library(rpart)
rpart.fit = rpart(DelayClass~., data=AirlinesTrain,method = "class")

library(rpart.plot)
prp(rpart.fit)

# 15
pred.rpart = predict(rpart.fit,type="class")
table(AirlinesTrain$DelayClass, pred.rpart)

(361+3094)/nrow(AirlinesTrain)

# 16
table(AirlinesTrain$DelayClass)
3282/nrow(AirlinesTrain)

# 17
pred.rpart = predict(rpart.fit,newdata =AirlinesTest, type="class")
table(AirlinesTest$DelayClass, pred.rpart)
(153+1301)/nrow(AirlinesTest)

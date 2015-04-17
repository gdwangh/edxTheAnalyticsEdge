setwd("D:/doc/study/TheAnalyticsEdge/unit3")

# 1.1
parole = read.csv("parole.csv")
str(parole)
summary(parole)

# 1.2
table(parole$violator)

# 2.2
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# 3.1
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# 4.1
mod1<-glm(violator~., data = train, family="binomial")
summary(mod1)
exp(1.61)

# 4.3
pred<-predict(mod1, 
              newdata=data.frame(male=1, race=1, age= 50, state=as.factor(1),time.served=3, max.sentence=12,multiple.offenses=0,crime=as.factor(2)), 
              type="response")
odds=pred/(1-pred)

# 5.1
predTest<-predict(mod1, newdata=test, type="response")
max(predTest)

# 5.2
table(test$violator, predTest>0.5)
12/(11+12)
167/(167+12)
(167+12)/(167+12+11+12)

# 5.3
table(test$violator)
179/(179+23)

# 5.6
library("ROCR")
ROCRpredTest = prediction(predTest, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

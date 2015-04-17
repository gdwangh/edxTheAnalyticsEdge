setwd("D:/doc/study/TheAnalyticsEdge/unit3")
quality = read.csv("quality.csv")

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog) 

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)

install.packages("ROCR")
library("ROCR")
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

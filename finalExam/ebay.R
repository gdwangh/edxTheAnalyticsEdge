setwd('D:/workspace/The Analytics Edge/finalExam')

Sys.setlocale("LC_ALL", "C")
#  1
eBay = read.csv("eBay.csv", stringsAsFactors=FALSE,encoding="latin1")
table(eBay$sold)

799/nrow(eBay)

# 2
summary(eBay)

# 3
hist(eBay$size)
table(eBay$size)

# 4
eBay$sold = as.factor(eBay$sold)
eBay$condition  = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color  = as.factor(eBay$color)
eBay$material  = as.factor(eBay$material)

# 5
set.seed(144)

library(caTools)

spl = sample.split(eBay$sold, 0.7)
training = subset(eBay, spl==TRUE)
testing = subset(eBay, spl==FALSE)

# 6
glmFit = glm(sold~biddable+startprice+condition+heel+style+color+material, data = training, family = "binomial")
summary(glmFit)

# 7
ashoe = data.frame(biddable=0, startprice=100,
                   condition=factor("Pre-owned", levels(eBay$condition)), 
                   heel=factor("High", levels(eBay$heel)), 
                   style=factor("Open Toe",levels(eBay$style)),
                   color=factor("Black",levels(eBay$color)), 
                   material=factor("Satin",levels(eBay$material)))


pred = predict(glmFit, newdata=ashoe, type="response")

# 8
exp(0.8325406) - 1

# 9
pred = predict(glmFit, newdata=testing, type="response")

table(training$sold)  
# 0    1 
# 2098  559

table((pred > 0.5))

# 10
library("ROCR")
ROCRpredTest = prediction(pred, testing$sold)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# 11
ROCRperf = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

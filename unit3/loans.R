# setwd("D:/workspace/The Analytics Edge/unit3")
setwd("D:/doc/study/TheAnalyticsEdge/unit3")

# 1.1
loans = read.csv("loans.csv")
str(loans)
summary(loans)

table(loans$not.fully.paid)
1533/9578   # 0.1600543

# 1.3
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)
12/(50+12)  # 0.1935484

# 1.4
install.packages("mice") 
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

summary(loans)
loads_imputed = read.csv("loans_imputed.csv")
summary(loads_imputed)

# 为了有个共同的基准，作业要求从文件中读出的数据作为处理数据
loans = read.csv("loans_imputed.csv")
summary(loads_imputed)


# 2.1
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio=0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)

model1 = glm(not.fully.paid~., data=train, family="binomial")
summary(model1)

-9.317e-03 * (700-710)
exp(-9.317e-03 * (700-710))

# 2.3
predicted.risk = predict(model1, newdata=test, type="response")
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk>=0.5)
(2400+3)/(2400+13+457+3)

table(test$not.fully.paid)
2413/(2413+460)

# 2.4
library("ROCR")
ROCRpredTest = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# 3.2
model2 = glm(not.fully.paid~int.rate, data=train,family="binomial")
summary(model2)
predTest2 = predict(model2, newdata=test, type="response")

max(predTest2)
table(predTest2>=0.5)

# 3.3
ROCRpredTest2 = prediction(predTest2, test$not.fully.paid)
auc2 = as.numeric(performance(ROCRpredTest2, "auc")@y.values)
auc2

# 4.1
10 * exp(0.06 * 3)

# 5.1
test$profit = exp(test$int.rate*3) - 1   # 如果能够收回贷款时的盈利
test$profit[test$not.fully.paid == 1] = -1   # 如果一点钱都收不回来的亏损

max(test$profit) * 10

# 6.1
highInterest=subset(test, int.rate>=0.15)
mean(highInterest$profit)

table(highInterest$not.fully.paid)
110/(327+110)

# 6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk<=0.17633053)
nrow(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
19/(81+19)

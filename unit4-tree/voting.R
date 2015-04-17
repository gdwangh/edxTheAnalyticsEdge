setwd('D:/workspace/The Analytics Edge/unit4')
gerber=read.csv("gerber.csv")
str(gerber)

# 1.1
nrow(gerber[gerber$voting==1,])/nrow(gerber)

# 1.2
colMeans(gerber[gerber$voting==1,c(4:7)])


tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# 1.3
votlm<-glm(voting~civicduty+hawthorne+self+neighbors, data=gerber, family="binomial")
summary(votlm)

# 1.4
vot.pred = predict(votlm, type="response")
table(gerber$voting, vot.pred >= 0.3)
(134513+51966)/nrow(gerber)

# 1.5
table(gerber$voting, vot.pred >= 0.5)
235388/nrow(gerber)

# 1.6
# baseline, not voting
table(gerber$voting)
235388/nrow(gerber)

library(ROCR)
ROCRpred = prediction(vot.pred, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)


# 2.1
library(caret)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# 2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# 2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)   # 注意，sex=0在右边的叶子!!!

# 3.1
ContolTree = rpart(voting~control,data=gerber, cp=0.0)
prp(ContolTree,digits = 6)
abs(0.296638-0.34)

# 3.2
ContolSexTree = rpart(voting~control+sex,data=gerber, cp=0.0)
prp(ContolSexTree,digits = 6)

abs(0.302795-0.345818)  # 0: 0.043023
abs(0.290456-0.334176)  # 1: 0.04372

0.043023 - 0.04372

# 3.3
CtrlSexlm = glm(voting~control+sex, data=gerber, family="binomial")
summary(CtrlSexlm)

# 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(CtrlSexlm, newdata=Possibilities, type="response")

# tree, women, control
0.290456
# glm, women,control
0.2908065

abs(0.290456 - 0.2908065)

# 3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

# 3.6
predict(LogModel2, newdata=Possibilities, type="response")

# new glm, women, control
0.2904558

abs(0.290456 - 0.2904558)

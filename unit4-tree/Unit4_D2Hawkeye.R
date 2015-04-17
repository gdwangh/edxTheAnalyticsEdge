# Unit 4 - "Keeping an Eye on Healthcare Costs" Lecture


# VIDEO 6
setwd('D:/workspace/The Analytics Edge/unit4')

# Read in the data
Claims = read.csv("ClaimsData.csv")

str(Claims)

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# Split the data
library(caTools)

set.seed(88)

spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)

ClaimsTrain = subset(Claims, spl==TRUE)

ClaimsTest = subset(Claims, spl==FALSE)

# Quick question
mean(ClaimsTrain$age)

nrow(subset(ClaimsTrain, diabetes>0))/nrow(ClaimsTrain)

# VIDEO 7

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)  # accuracy = 0.6838135

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

# Quick question
# used the baseline method of predicting the most frequent outcome for all observations
table(ClaimsTrain$bucket2008)

# most frequent outcome is 1
table(ClaimsTest$bucket2009)
122978/nrow(ClaimsTest)  # accuracy = 0.67127

sum(matrix(c(table(ClaimsTest$bucket2009), rep(0,20)),nrow=5,ncol=5) * PenaltyMatrix)/nrow(ClaimsTest)



# VIDEO 8

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree)


# Make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# New CART model with loss matrix
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))

# Redo predictions and penalty error
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

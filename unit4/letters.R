setwd('D:/workspace/The Analytics Edge/unit4')

# 1.1
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
spl = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(train$isB)
1175/(1175+383)

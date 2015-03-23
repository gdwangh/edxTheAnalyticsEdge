setwd("D:/workspace/The Analytics Edge/unit3")

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

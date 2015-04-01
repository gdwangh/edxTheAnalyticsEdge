data(state)
statedata = data.frame(state.x77)
str(statedata)

# 1.1
statelm = lm(Life.Exp~., data=statedata)
summary(statelm)

# 1.2
stateSSE = sum(statelm$residual^2)
stateSSE

# 1.3
statelm2 = lm(Life.Exp~Population+Murder+HS.Grad+Frost, data=statedata)
summary(statelm2)

# 1.4
stateSSE2 = sum(statelm2$residual^2)
stateSSE2

# 2.1
library(rpart)
library(rpart.plot)
stateTree = rpart(Life.Exp~., data=statedata)
prp(stateTree)

# 2.2
stateTree.pred = predict(stateTree)
stateTreeSSE = sum((stateTree.pred-statedata$Life.Exp)^2)
stateTreeSSE

# 2.3
stateTree2 = rpart(Life.Exp~., data=statedata, minbucket=5)
prp(stateTree2)

# 2.5
stateTree2.pred = predict(stateTree2)
stateTree2SSE = sum((stateTree2.pred-statedata$Life.Exp)^2)
stateTree2SSE

# 2.6
stateTree3 = rpart(Life.Exp~Area, data=statedata, minbucket=1)
prp(stateTree3)

stateTree3.pred = predict(stateTree3)
stateTree3SSE = sum((stateTree3.pred-statedata$Life.Exp)^2)
stateTree3SSE

# 3.1
library(caret)
set.seed(111)
tr.control = trainControl(method = "cv", number = 10)

cp.grid = expand.grid( .cp = seq(0.01, 0.5, 0.01))
train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# 3.2
stateTree4 = rpart(Life.Exp ~ ., data = statedata,cp=0.12)
prp(stateTree4)

# 3.3
stateTree4.pred = predict(stateTree4)
stateTree4.sse = sum((stateTree4.pred - statedata$Life.Exp)^2)
stateTree4.sse

# 3.5
set.seed(111)
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

stateTree5 = rpart(Life.Exp ~ Area, data = statedata,cp=0.02)
prp(stateTree5)

# 3.6
stateTree5.pred = predict(stateTree5)
stateTree5.sse = sum((stateTree5.pred - statedata$Life.Exp)^2)
stateTree5.sse

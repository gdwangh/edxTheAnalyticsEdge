setwd("D:/doc/study/TheAnalyticsEdge/unit2")

data(state)

statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

plot(statedata$y, statedata$x)

tapply(statedata$HS.Grad, statedata$state.region, mean)

boxplot(Murder~state.region, data=statedata)

subset(statedata,state.region=="Northeast", Murder)

lmState<-lm(Life.Exp~Population+Income+Illiteracy+Murder+ HS.Grad+Frost+Area, data=statedata)
summary(lmState)

plot(statedata$Income, statedata$Life.Exp)

lmState2<-lm(Life.Exp~Population+Murder+ HS.Grad+Frost, data=statedata)
summary(lmState2)
sort(predict(lmState2))
statedata[which.min(statedata$Life.Exp),]

sort(predict(lmState2))
statedata[which.max(statedata$Life.Exp),]

which.min(abs(lmState2$residuals))
which.max(abs(lmState2$residuals))

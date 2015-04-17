setwd("D:/doc/study/TheAnalyticsEdge/unit2")
climate<-read.csv("climate_change.csv")
str(climate)
train<-subset(climate, Year<="2006")
test<-subset(climate, Year>"2006")
str(train)

model1<-lm(Temp~MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=train)
summary(model1)

cor(train[,c(3:10)])

model2<-lm(Temp~MEI + N2O +  TSI + Aerosols,data=train)
summary(model2)

model3<-step(model1)
summary(model3)

pred<-predict(model3, newdata=test)
SSE<-sum((pred-test$Temp)^2)
SST<-sum((test$Temp-mean(train$Temp))^2)
R2<-1-SSE/SST

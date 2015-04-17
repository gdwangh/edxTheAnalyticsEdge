setwd("D:/doc/study/TheAnalyticsEdge/unit2")
elantra <-read.csv("elantra.csv")
str(elantra)

elantraTrain<-subset(elantra, Year<=2012)
elantraTest<-subset(elantra, Year>2012)
str(elantraTrain)

lmSale<-lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + Queries, data=elantraTrain)
summary(lmSale)

lmSaleMon<-lm(ElantraSales ~ Month + Unemployment + CPI_energy + CPI_all + Queries, data=elantraTrain)
summary(lmSaleMon)


elantraTrain$MonthFactor <- as.factor(elantraTrain$Month)
lmSaleMon2<-lm(ElantraSales ~ MonthFactor + Unemployment + CPI_energy + CPI_all + Queries, data=elantraTrain)
summary(lmSaleMon2)

cor(elantraTrain[,c(1,4:7)])

lmSaleMon3<-lm(ElantraSales ~ MonthFactor + Unemployment + CPI_energy + CPI_all, data=elantraTrain)
summary(lmSaleMon3)


elantraTest$MonthFactor <- as.factor(elantraTest$Month)
pred<-predict(lmSaleMon3, newdata=elantraTest)
SSE<-sum((pred - elantraTest$ElantraSales)^2)

SST<-sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)

R2<- 1- SSE/SST

max(abs(pred - elantraTest$ElantraSales))

elantraTest[which.max(abs(pred - elantraTest$ElantraSales)), c(1:2)]

setwd("D:/workspace/The Analytics Edge/unit2")
# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# Quick Question
predWin = predict(WinsReg, newdata=data.frame(RD=713-614))

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

# Quick Question
pred<- predict(RunsReg, newdata=data.frame(OBP= 0.311, SLG=0.405))
pred
# Regression model to predict runs allowed
RunsAllow = lm(RA~OOBP+OSLG, data=moneyball)

pred<- predict(RunsAllow, newdata=data.frame(OOBP= 0.297, OSLG=0.370))
pred

# select TWO players 
player<-data.frame(OBP=c(0.338,0.391,0.369,0.313,0.361), 
                   SLG=c(0.540,0.450,0.374,0.447,0.500)
predRunsReg<-predict(RunsReg, newdata=player)

# teamRank
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)

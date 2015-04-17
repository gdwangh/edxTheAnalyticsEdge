setwd("D:/doc/study/TheAnalyticsEdge/unit3")

# 1.1
baseball=read.csv("baseball.csv")
str(baseball)

# 1.2
length(table(baseball$Year))

# 1.3
baseball = subset(baseball, Playoffs==1)
str(baseball)
table(baseball$Year)

# 2.1
PlayoffTable = table(baseball$Year)
names(PlayoffTable)

# 2.3
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
nrow(subset(baseball, NumCompetitors==8))

# 3.1
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
nrow(subset(baseball, WorldSeries==0))

# 3.2
model1 = glm(WorldSeries~Year, data=baseball, family="binomial")
summary(model1)

model2 = glm(WorldSeries~RS, data=baseball, family="binomial")
summary(model2)

model3 = glm(WorldSeries~RA, data=baseball, family="binomial")
summary(model3)

model4 = glm(WorldSeries~W, data=baseball, family="binomial")
summary(model4)

model5 = glm(WorldSeries~OBP, data=baseball, family="binomial")
summary(model5)

model6 = glm(WorldSeries~SLG, data=baseball, family="binomial")
summary(model6)

model7 = glm(WorldSeries~BA, data=baseball, family="binomial")
summary(model7)

model8 = glm(WorldSeries~RankSeason, data=baseball, family="binomial")
summary(model8)

model9 = glm(WorldSeries~OOBP, data=baseball, family="binomial")
summary(model9)

model10 = glm(WorldSeries~OSLG, data=baseball, family="binomial")
summary(model10)

model11 = glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")
summary(model11)

model12 = glm(WorldSeries~League, data=baseball, family="binomial")
summary(model12)

# 4.1
model13 = glm(WorldSeries ~ Year+RA+RankSeason+NumCompetitors, data=baseball, family="binomial")
summary(model13)

# 4.2
cor(baseball[c(3:16)])


# 4.3

glm(WorldSeries~Year, data=baseball, family="binomial")$aic
glm(WorldSeries~RA, data=baseball, family="binomial")$aic
glm(WorldSeries~RankSeason, data=baseball, family="binomial")$aic 
glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")$aic
glm(WorldSeries~Year+RA, data=baseball, family="binomial")$aic
glm(WorldSeries~Year+RankSeason, data=baseball, family="binomial")$aic
glm(WorldSeries~Year+NumCompetitors, data=baseball, family="binomial")$aic
glm(WorldSeries~RA+RankSeason, data=baseball, family="binomial")$aic
glm(WorldSeries~RA+NumCompetitors, data=baseball, family="binomial")$aic
glm(WorldSeries~RankSeason+NumCompetitors, data=baseball, family="binomial")$aic
        
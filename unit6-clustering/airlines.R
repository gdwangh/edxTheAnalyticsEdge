# setwd("D:/doc/study/TheAnalyticsEdge/unit6")
setwd('D:/workspace/The Analytics Edge/unit6')

# 1.1 
airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

# normalize data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# 1.2
distance = dist(airlinesNorm, method="euclidean")
HclusterAirlinesNorm = hclust(distance, method="ward.D")
plot(HclusterAirlinesNorm)

# 2.2
HierClusterGrp = cutree(HclusterAirlinesNorm, k= 5)
table(HierClusterGrp)

# 2.3
tapply(airlines$Balance, HierClusterGrp, mean)
tapply(airlines$QualMiles, HierClusterGrp, mean)
tapply(airlines$BonusMiles, HierClusterGrp, mean)
tapply(airlines$BonusTrans, HierClusterGrp, mean)
tapply(airlines$FlightMiles, HierClusterGrp, mean)
tapply(airlines$FlightTrans, HierClusterGrp, mean)
tapply(airlines$DaysSinceEnroll, HierClusterGrp, mean)


# 3.1 k-mean
set.seed(88)
KAirlines = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
KAirlinesClusterGrp = KAirlines$cluster
table(KAirlinesClusterGrp)

# 3.2
str(KAirlines$centers)

tapply(airlines$Balance, KAirlinesClusterGrp, mean)
tapply(airlines$QualMiles, KAirlinesClusterGrp, mean)
tapply(airlines$BonusMiles, KAirlinesClusterGrp, mean)
tapply(airlines$BonusTrans, KAirlinesClusterGrp, mean)
tapply(airlines$FlightMiles, KAirlinesClusterGrp, mean)
tapply(airlines$FlightTrans, KAirlinesClusterGrp, mean)
tapply(airlines$DaysSinceEnroll, KAirlinesClusterGrp, mean)


KAirlines$centers[1,]
sapply(1:5, function(idx) { KAirlines$centers[idx,]})

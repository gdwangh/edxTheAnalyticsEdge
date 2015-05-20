setwd('D:/workspace/The Analytics Edge/finalExam')
# setwd('D:/doc/study/TheAnalyticsEdge/finalExam')

# 1
hubwayTrip = read.csv("HubwayTrips.csv");
nrow(hubwayTrip)

# 2
mean(hubwayTrip$Duration)

tapply(hubwayTrip$Duration, hubwayTrip$Weekday, mean)


# 3
table(hubwayTrip$Morning)
table(hubwayTrip$Afternoon)
table(hubwayTrip$Evening)

# 4
prop.table(table(hubwayTrip$Male))

# 6
library(caret)

preproc = preProcess(hubwayTrip)

HubwayNorm = predict(preproc, hubwayTrip)

summary(HubwayNorm)

# 8
set.seed(5000)
k=10
KMC = kmeans(HubwayNorm, centers = k, iter.max = 1000)
HubwayNorm$cluster_id = KMC$cluster

table(KMC$cluster)

#9
# trips taken by female users on weekday evenings:
# Male: <0
# Weekday: >0
# Evening: > 0
t=sapply(1:10, function(idx) { KMC$centers[idx,]})
which(t[6,]<0 & t[5,]>0 & t[4,]>0)

# or 
tapply(HubwayNorm$Male, HubwayNorm$cluster_id, mean)
tapply(HubwayNorm$Weekday, HubwayNorm$cluster_id, mean)
tapply(HubwayNorm$Evening, HubwayNorm$cluster_id, mean)


# 10
# leisurely (longer than average) afternoon trips taken on the weekends
# Duaration: >0
# Afternoon: > 0
# Weekday: < 0

t=sapply(1:10, function(idx) { KMC$centers[idx,]})
which(t[1,]>0 & t[3,]>0 & t[5,]<0)

# 11
# morning trips taken by older male users
# Morning: > 0
# Male: > 0
# Age: older
t = sapply(1:10, function(idx) { KMC$centers[idx,]})
which(t[2,]>0 & t[6,]>0 & t[7,]>0)

# 14
set.seed(8000)
k=20
KMC = kmeans(HubwayNorm, centers = k, iter.max = 1000)
HubwayNorm$cluster_id = KMC$cluster

table(KMC$cluster)

# 15
# shorter than average trips that occur on weekday evenings
# duration: < 0
# Weekday: > 0
# Evening: > 0
t=sapply(1:k, function(idx) { KMC$centers[idx,]})

which(t[1,]<0 & t[4,]>0 & t[5,]>0)

# 16
boxplot(HubwayNorm$Age~HubwayNorm$cluster_id)

library(ggplot2)
ggplot(HubwayNorm, aes(x=cluster_id, y=Age))+geom_histogram()
ggplot(HubwayNorm, aes(x=Age, y=cluster_id))+geom_point()

# 17

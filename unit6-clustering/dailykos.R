setwd("D:/doc/study/TheAnalyticsEdge/unit6")

# 1.1
dailykos = read.csv("dailykos.csv")
str(dailykos)
distance = dist(dailykos)

clusterDailykos = hclust(distance, method="ward.D")

# 1.2
plot(clusterDailykos)

# 1.4
HClusters = cutree(clusterDailykos, k=7)

Hcluster1 = subset(dailykos, HClusters==1)
Hcluster2 = subset(dailykos, HClusters==2)
Hcluster3 = subset(dailykos, HClusters==3)
Hcluster4 = subset(dailykos, HClusters==4)
Hcluster5 = subset(dailykos, HClusters==5)
Hcluster6 = subset(dailykos, HClusters==6)
Hcluster7 = subset(dailykos, HClusters==7)

nrow(Hcluster1)
nrow(Hcluster2)
nrow(Hcluster3)
nrow(Hcluster4)
nrow(Hcluster5)
nrow(Hcluster6)
nrow(Hcluster7)

# 1.5
tail(sort(colMeans(Hcluster1)))

# 1.6
tail(sort(colMeans(Hcluster2)))
tail(sort(colMeans(Hcluster3)))
tail(sort(colMeans(Hcluster4)))
tail(sort(colMeans(Hcluster5)))
tail(sort(colMeans(Hcluster6)))
tail(sort(colMeans(Hcluster7)))


# 2.1
set.seed(1000)
KDK = kmeans(dailykos, centers = 7)
kClusters = KDK$cluster

Kcluster1 = subset(dailykos, kClusters==1)
Kcluster2 = subset(dailykos, kClusters==2)
Kcluster3 = subset(dailykos, kClusters==3)
Kcluster4 = subset(dailykos, kClusters==4)
Kcluster5 = subset(dailykos, kClusters==5)
Kcluster6 = subset(dailykos, kClusters==6)
Kcluster7 = subset(dailykos, kClusters==7)

nrow(Kcluster1)
nrow(Kcluster2)
nrow(Kcluster3)
nrow(Kcluster4)
nrow(Kcluster5)
nrow(Kcluster6)
nrow(Kcluster7)

# 2.2
tail(sort(colMeans(Kcluster1)))
tail(sort(colMeans(Kcluster2)))
tail(sort(colMeans(Kcluster3)))
tail(sort(colMeans(Kcluster4)))
tail(sort(colMeans(Kcluster5)))
tail(sort(colMeans(Kcluster6)))
tail(sort(colMeans(Kcluster7)))

# 2.3
table(HClusters, kClusters)

#  2.4

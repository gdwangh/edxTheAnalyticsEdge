# Unit 6 - Introduction to Clustering
setwd('D:/workspace/The Analytics Edge/unit6')
# Video 6

# After following the steps in the video, load the data into R
movies = read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)

# Quick Question
sum(movies$Comedy==1, na.rm=TRUE)
table(movies$Comedy)

sum(movies$Western, na.rm=TRUE)
table(movies$Western)

sum(movies$Romance == 1 & movies$Drama == 1, na.rm=TRUE)
table(movies$Romance, movies$Drama)

# Video 7

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
# clusterMovies = hclust(distances, method = "ward") 
clusterMovies = hclust(distances, method = "ward.D")  # "ward" is rename in R with "ward.D"

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# Quick Question
clusterGroups2 = cutree(clusterMovies, k = 2)
cluster1 = subset(movies, clusterGroups2==1)
cluster2 = subset(movies, clusterGroups2==2)

colMeans(cluster2[2:20])

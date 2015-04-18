setwd("D:/doc/study/TheAnalyticsEdge/unit7-visualization")
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)

statesMap = map_data("state")

# 1.1
str(statesMap)
table(statesMap$group)

# 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# 2.1
polling = read.csv("PollingImputed.csv")

# split train set and test set 
Train = subset(polling, Year < "2012")
Test = subset(polling, Year=="2012")

# glm and predict
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

# create the data frame 
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
22/45
# 2.2

# trans region and merge
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

# Reorder the data
predictionMap = predictionMap[order(predictionMap$order),]

# 3.1 plot 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# 3.2 plot
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# 4

# plot 1
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# plot 2
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# plot 3
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")



setwd("D:/doc/study/15.071x The Analytics Edge/unit1")
mvt<-read.csv("mvtWeek1.csv")

str(mvt)

max(mvt$ID)

min(mvt$Beat)

summary(mvt$Arrest)

nrow(subset(mvt, LocationDescription=="ALLEY"))

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)


mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

table(mvt$Month)

table(mvt$Weekday)

table(subset(mvt, Arrest==TRUE, Month))

hist(mvt$Date, breaks=100)
boxplot(Date~Arrest, data=mvt)

table(subset(mvt, Year==2001, Arrest))
2151/(18517+2151)

table(subset(mvt, Year==2007, Arrest))
1212/(13068+1212)

table(subset(mvt, Year==2012, Arrest))
550/(13542+550)


sort(table(mvt$LocationDescription))

Top5<-subset(mvt,LocationDescription=="STREET"|LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"|LocationDescription=="ALLEY"|LocationDescription=="GAS STATION"|LocationDescription=="DRIVEWAY - RESIDENTIAL" )
Top5$LocationDescription = factor(Top5$LocationDescription)

prop.table(table(Top5$LocationDescription, Top5$Arrest),margin=1)

table(subset(Top5, LocationDescription=="GAS STATION", Weekday))

table(subset(Top5, LocationDescription=="DRIVEWAY - RESIDENTIAL", Weekday))

setwd("D:/doc/study/15.071x The Analytics Edge/unit1")
CPS<-read.csv("CPSData.csv")
summary(CPS)
str(CPS)
sort(table(CPS$State)) 
(116639+7073)/131302

table(CPS$Hispanic, CPS$Race)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)), 1)

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))


MetroAreaMap<-read.csv("MetroAreaCodes.csv")
CountryMap<-read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(table(CPS$MetroArea))

tapply(CPS$Hispanic, CPS$MetroArea, mean)>0.2

table(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)>0.2)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

str(CountryMap)
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

sort(table(CPS$Country))
table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country=="United States")
1668/(1668+3736)


table(CPS$MetroArea, CPS$Country=="India")

table(CPS$MetroArea, CPS$Country=="Brazil")

table(CPS$MetroArea, CPS$Country=="Somalia")


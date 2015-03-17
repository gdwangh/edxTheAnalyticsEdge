setwd("D:/workspace/The Analytics Edge")
poll<-read.csv("AnonymityPoll.csv")

table(poll$Smartphone)

summary(poll$Smartphone)

table(poll$State, poll$Region)

table(poll$Internet.Use, poll$Smartphone)

summary(poll)

limited<-subset(poll, Internet.Use==1|Smartphone==1)
summary(limited)

table(limited$Info.On.Internet)

table(limited$Worry.About.Info)
386/(404+386)

table(limited$Anonymity.Possible)
278/(475+278)

table(limited$Tried.Masking.Identity)
128/(128+656)

table(limited$Privacy.Laws.Effective)
186/(541+186)


hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, mean)

tapply(limited$Tried.Masking.Identity,limited$Smartphone, mean, na.rm=TRUE)


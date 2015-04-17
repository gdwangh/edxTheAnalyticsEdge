setwd("D:/doc/study/TheAnalyticsEdge/unit3")

# 1.1
songs = read.csv("songs.csv")
str(songs)
summary(songs)

table(songs$year)

# 1.2
MK = subset(songs, artistname=="Michael Jackson")

# 1.3
subset(songs, artistname=="Michael Jackson" & Top10==1, select="songtitle")

# 1.4
table(songs$timesignature)

# 1.5
songs[which.max(songs$tempo),"songtitle"]

# 2.1
SongsTrain = subset(songs, year!=2010)
SongsTest = subset(songs, year == 2010)

# 2.2
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# 3.1
cor(SongsTrain$loudness, SongsTrain$energy)

# 3.2, model omit loudness
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# 3.3 model omit energy
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# 4.1
predTest<-predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, predTest>=0.45)
(309+19)/(309+5+40+19)

# 4.2
table(SongsTrain$Top10)  # baseline is get 0

table(SongsTest$Top10)
314/(314+59)


# 4.4
19/(40+19)
309/(309+5)

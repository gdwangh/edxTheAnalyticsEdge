setwd('D:/workspace/The Analytics Edge/unit5')

# 1.1
emails = read.csv("emails.csv",stringsAsFactors=FALSE)
str(emails)

# 1.2
table(emails$spam)

# 1.3
emails$text[1]

# 1.5
max(nchar(emails$text))

# 1.6
min(nchar(emails$text))

# 2.1
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm

# 2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# 2.3
emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

# 2.4
emailsSparse$spam = emails$spam
colnames(emailsSparse)[colSums(emailsSparse[emailsSparse$spam==0, ])>=5000]

# 2.5
colnames(emailsSparse)[colSums(emailsSparse[emailsSparse$spam==1, ])>=1000]


# 2.6


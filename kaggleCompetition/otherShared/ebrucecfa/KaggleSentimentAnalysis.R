library(plyr)
library(stringr)
library(e1071)  

colnames(afinn_list) = c('word','score')

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1])
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")

#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}  

sentences = as.character(News$Headline)
HL.Sentiment = as.data.frame( sentimentScore(sentences, vNegTerms, negTerms, posTerms, vPosTerms))
colnames(HL.Sentiment) = c('HL','HLvNegTerms', 'HLnegTerms', 'HLposTerms', 'HLvPosTerms')

sentences = as.character(News$Abstract)
AB.Sentiment = as.data.frame( sentimentScore(sentences, vNegTerms, negTerms, posTerms, vPosTerms))
colnames(AB.Sentiment) = c('AB','ABvNegTerms', 'ABnegTerms', 'ABposTerms', 'ABvPosTerms')

# Combine the two
blogsentiment = cbind(HL.Sentiment, AB.Sentiment)
blogsentiment$HL = NULL
blogsentiment$AB = NULL

# Combine sentiment and News
News = cbind(News, blogsentiment)

str(News)

str(blogsentiment)

# Make Train & Test
Train = News[News$isTest==0,]
Test = News[News$isTest==1,]
Train$isTest=NULL
Train$UniqueID=NULL
Test$isTest=NULL
Test$Popular = NULL

# Determine sig vars
myglm = glm(Popular ~ ., data=Train, family=binomial)


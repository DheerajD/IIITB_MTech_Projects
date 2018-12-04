library(readr) 
library(purrr) 
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

#first I read the demonetisation csv file as input
demonetisationData <- read.csv("C:/D_ DheerajDoodhya/IIITB_MTech_Projects/Demonetisation_Tweets_Sentiment_Analysis/Demonetization_data29th.csv")

#extracting the tweets data from above dataset, don't need other columns for sentiment analysis
tweets_data <- as.factor(demonetisationData$CONTENT)

#Reading positive and negative data words from standard list of positive and negative words for sentiment score calculation
positive_words = scan('C:/D_ DheerajDoodhya/IIITB_MTech_Projects/Demonetisation_Tweets_Sentiment_Analysis/Non_learning_NLP_based_approach/positive-words.txt',what='character',sep=" ")
negative_words = scan('C:/D_ DheerajDoodhya/IIITB_MTech_Projects/Demonetisation_Tweets_Sentiment_Analysis/Non_learning_NLP_based_approach/negative-words.txt',what='character',sep=" ")

#Function to calculate sentiment score
score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  scores <- laply(sentences, function(sentence, positive_words, negative_words){
    
    preprocessTweets<-gsub("RT @\\w+","",sentence)
    #Removing 'RT' and people names from input sentences
    
    preprocessTweets<-gsub("http[^[:blank:]]+","",preprocessTweets)
    #Cleaning html links
    
    preprocessTweets<-gsub("[[:punct:]]","",preprocessTweets)
    #Removing punctuation characters
    
    preprocessTweets<-gsub("[[:cntrl:]]","",preprocessTweets)
    #Removing control characters like \n or \r
    
    preprocessTweets<-gsub("\\d+","",preprocessTweets)
    #Removing digits
    
    preprocessTweets<-trimws(preprocessTweets, which = c("both", "left", "right"))
    #Removing leading and trailing whitespace from character strings.
    
    preprocessTweets<-tolower(preprocessTweets)
    #Converting texts to lower case
    
    word_list<-strsplit(preprocessTweets,"\\s+")
    words <- unlist(word_list)
    #Spliting sentence into words with strsplit (stringr package)
    
    pos_matches <- match(words, positive_words)
    neg_matches <- match(words, negative_words)
    #Comparing words to the dictionaries of positive & negative terms
    
    pos_matches <- !is.na(pos_matches)
    neg_matches <- !is.na(neg_matches)
    #get the position of the matched term or NA
    
    score <- sum(pos_matches) - sum(neg_matches)
    #Final score
    
    return(score)
    
  }, positive_words, negative_words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  #Data frame with scores for each sentence
  
  return(scores.df)
}

#calculating sentiment score for all the tweets
sentiment_score <- score.sentiment(tweets_data,positive_words,negative_words,.progress='text')

#evaluating whether the sentiment score obtained is positive, negative or neutral
sentiment_score$positive <- as.numeric(sentiment_score$score >0)
sentiment_score$negative <- as.numeric(sentiment_score$score <0)
sentiment_score$neutral <- as.numeric(sentiment_score$score==0)

#Creating polarity variable for each sentence based on their sentiment_score
sentiment_score$polarity <- ifelse(sentiment_score$score >0,"Positive",ifelse(sentiment_score$score < 0,"Negative",ifelse(sentiment_score$score==0,"Neutral",0)))

#Adding new column 'sentiments' into dataset to store sentiment analysis score
demonetisationData$sentiments<-sentiment_score$polarity

#Adding tweets and there corresponding sentiment to a new dataset
demonetisationSentiments <- data.frame(demonetisationData$CONTENT, demonetisationData$sentiments);

#Exporting new generated dataset into csv file
write.csv(demonetisationSentiments,"C:/D_ DheerajDoodhya/IIITB_MTech_Projects/Demonetisation_Tweets_Sentiment_Analysis/dataset_with_sentiments.csv")



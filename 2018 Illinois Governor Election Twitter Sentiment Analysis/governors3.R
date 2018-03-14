## Lines 2-64 are for initial setup for reading tweets from Twitter using R.
## The code for lines 2-64 are from here.
## (https://analytics4all.org/2016/11/25/r-twitter-sentiment-analysis/)
install.packages("twitteR")
install.packages("stringr")
install.packages("plyr")
library(twitteR)
library(stringr)
library(plyr)

## Initializing first set of API keys
api_key = "ic06RYw869vaqLkZKZ1rFWLE6"
api_secret = "yvQQtxEiQmz2JTcIAjLmgUFCCFN18HUE9U4GVaZpDTH1scTJLC"
access_token = "973396302678589441-I5bAtINbIkY4oxsXLMyx05HKeUGvyoT"
access_token_secret = "Fb8WkCCqGm63uEdVtmx1RafTErs3ryLOVtWTU4BPhOjeb"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

positiveWords = download.file("https://raw.githubusercontent.com/ajayjjain/political-analysis/master/2018%20Illinois%20Governor%20Election%20Twitter%20Sentiment%20Analysis/positive-words.txt", "positiveWords.txt")
negativeWords = download.file("https://raw.githubusercontent.com/ajayjjain/political-analysis/master/2018%20Illinois%20Governor%20Election%20Twitter%20Sentiment%20Analysis/negative-words.txt", "negativeWords.txt")
neg = scan("negativeWords.txt", what="character", comment.char=";")
pos = scan("positiveWords.txt", what="character", comment.char=";")

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)

  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = strsplit(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
}



## The first three lines of each candidate segment are modified from the tutorial
## These lines pull tweets from Twitter mentioning a candidate's first and last name,
## and then determine how if each tweet is positive or negative.
## The lines afterwards are my original code.
## My lines of code summarize the sentiment scores, graph the sentiment scores,
## and determine how many tweets there are about each candidate.

bissTweets = searchTwitter('Daniel Biss', n = 10000)
bissTweets.text = laply(bissTweets,function(t)t$getText()) # gets text from Tweets
bissAnalysis = score.sentiment(bissTweets.text, pos, neg) # calls sentiment function
## Line below creates a graph of sentiment scores for Biss tweets
numberOfTweetsAboutBiss = length(bissAnalysis$text) # determines how many tweets about Biss

pritzkerTweets = searchTwitter('JB Pritzker', n = 10000)
pritzkerTweets.text = laply(pritzkerTweets,function(t)t$getText()) # gets text from Tweets
pritzkerAnalysis = score.sentiment(pritzkerTweets.text, pos, neg) # calls sentiment function
## Line below creates a graph of sentiment scores for Pritzker tweets
numberOfTweetsAboutPritzker = length(pritzkerAnalysis$text) # determines how many tweets about Pritzker


kennedyTweets = searchTwitter('Chris Kennedy', n = 10000)
kennedyTweets.text = laply(kennedyTweets,function(t)t$getText()) # gets text from Tweets
kennedyAnalysis = score.sentiment(kennedyTweets.text, pos, neg) # calls sentiment function
## Line below creates a graph of sentiment scores for Kennedy tweets
numberOfTweetsAboutKennedy = length(kennedyAnalysis$text) # determines how many tweets about Kennedy


raunerTweets = searchTwitter('Bruce Rauner', n = 10000)
raunerTweets.text = laply(raunerTweets,function(t)t$getText()) # gets text from Tweets
raunerAnalysis = score.sentiment(raunerTweets.text, pos, neg) # calls sentiment function
## Line below creates a graph of sentiment scores for Rauner tweets
numberOfTweetsAboutRauner = length(raunerAnalysis$text) # determines how many tweets about Rauner

api_key = "nPWQWfAWvumRXTKRIhMvrPJef"
api_secret = "hjtHfha7ivn3pSGCCnikXi5fVGbZs9GjMeTaYGgscppHFahGcj"
access_token = "973396302678589441-NEndge4Ho9h09D9wlqa8qxcB5APN6ad"
access_token_secret = "KTRWJqQWJG1FRBOmU2k5eXV2lmLa9ll9RavdcCs44goE8"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


ivesTweets = searchTwitter('Jeanne Ives', n = 10000)
ivesTweets.Text = laply(ivesTweets,function(t)t$getText()) # gets text from Tweets
ivesAnalysis = score.sentiment(ivesTweets.Text, pos, neg) # calls sentiment function
## Line below creates a graph of sentiment scores for Ives tweets
numberOfTweetsAboutIves = length(ivesAnalysis$text) # # determines how many tweets about Ives

## The line below pulls all the tweets retweeting NinaMorton posts that mention Jeanne Ives
ninamorton = subset(ivesAnalysis, grepl("NinaMorton", ivesAnalysis$text))
numberOfTweetsByNinaMorton = length(ninamorton$text) # # determines how many tweets by NinaMorton about Ives

## The line below pulls all the tweets retweeting redhead4645 posts that mention Jeanne Ives
redhead = subset(ivesAnalysis, grepl("redhead4645", ivesAnalysis$text))
numberOfTweetsByRedhead = length(redhead$text) # # determines how many tweets by redhead4645 about Ives

tweetDataFrame = data.frame(candidate = c("Daniel Biss", "JB Pritzker", "Chris Kennedy",
                                          "Bruce Rauner", "Jeanne Ives", "NinaMorton tweets about Ives",
                                          "redhead4645 tweets about Ives"), numberOfTweets = c(numberOfTweetsAboutBiss,
                                                                                               numberOfTweetsAboutPritzker,
                                                                                               numberOfTweetsAboutKennedy,
                                                                                               numberOfTweetsAboutRauner,
                                                                                               numberOfTweetsAboutIves,
                                                                                               numberOfTweetsByNinaMorton,
                                                                                               numberOfTweetsByRedhead))
View(tweetDataFrame) # outputs each candidate and how many tweets they were mentioned in

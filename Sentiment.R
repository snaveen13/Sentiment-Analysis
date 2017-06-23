library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
api_key <- "LnBgj8aLHFiA2YnZaxpE55K9N"
api_secret <- "xuZsSnDHZFSgRR3L1eQkm0caWwF8SsC9YsRJTJsP51DHX2uMw0"
access_token <- "231869993-tTrfdBDgHoXeB5IREw8zhnYR77eEPYB3T1jezJUv"
access_token_secret <- "g51cVPNzfKDfKMTHhFRCSEmLFEOvsUJAfGfzVp7w33xtF"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

neg_words <- read.delim("http://ptrckprry.com/course/ssd/data/negative-words.txt", header = FALSE, stringsAsFactors = FALSE)
neg_words <- as.data.frame(neg_words[-(1:33), ,drop = FALSE ])
View(neg_words)

pos_words <- read.delim("http://ptrckprry.com/course/ssd/data/positive-words.txt", header = FALSE, stringsAsFactors = FALSE)
pos_words <- as.data.frame(pos_words[-(1:33), ,drop = FALSE ])
View(pos_words)

pos.words <- unlist(pos_words)
neg.words <- unlist(neg_words)
head(pos.words)

negneg <- c('terror', 'terrorism','terrorist')

#Search twitter using Times Square
TimesSquare <- searchTwitter(searchString = "#TimesSquare", n = 5000)

#Convert the tweets to a text format.
times_text <- sapply(TimesSquare, function(t) t$getText() )

#Function for sentiment analysis
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores = score.sentiment(times_text, pos.words,negneg , .progress='text')
scores$score

hist(scores$score)

negative = sum(scores$score<0)
positive = sum(scores$score>0)
neutral = sum(scores$score==0)

plot(negative, positive, neutral)
df <- data.frame(c(negative, positive, neutral), row.names = c('negative', 'positive', 'neutral'))
barplot(table(df))

slices <- c(negative, positive, neutral)

pie(c(negative, positive, neutral), labels = c('negative', 'positive', 'neutral'), main = 'Pie chart of terrorism', col= rainbow(n =3))


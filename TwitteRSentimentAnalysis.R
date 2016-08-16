#-------------------------------------------#
#           Sentiment analysis              #
#-------------------------------------------#

library(twitteR)  # used to make the API call to twitter app
library(RCurl)    # used to make api calls supporting the twitter pacakage (might not be required on Linux)
library(maps)     # used to get the lon, lat of the cities
library(stringr)  # used for the string processing
library(sqldf)

setwd("C:\\R_Working_Dir\\InputData")

# Registering the twitter auth parameters (needs to be done only once)
if (!file.exists('cacert.perm')) download.file(url = 'http://curl.haxx.se/ca/cacert.pem', destfile='cacert.perm')

requestURL     = "https://api.twitter.com/oauth/request_token"
accessURL      = "https://api.twitter.com/oauth/access_token"
authURL        = "https://api.twitter.com/oauth/authorize"
consumerKey    = "Ko1M9ltsXD0lYyGPSTP3ZK8rr"
consumerSecret = "swb5Rp7TVyUOTyXtLewhjrj0y81D0wSbXL1zcY4icP0bY6LpeU"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package="RCurl"))

# Save credentials to local system
save(Cred, file = "twitter authentification.Rdata")

# Load the saved crenditals from the local system
load("twitter authentification.Rdata")
registerTwitterOAuth(Cred)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# Getting the latitude and the longitude of the cities to analyse
data(world.cities)
cities = data.frame(
  CITY    = c('Seattle', 'Chicago', 'New York', 'Los Angeles'),
  COUNTRY = c("USA", "USA", "USA", "USA"))

write.csv(world.cities,"cities")

# inner join
citiDat = merge(world.cities,cities, by.x = c('name','country.etc'), by.y = c('CITY','COUNTRY'))[,c(1,2,4,5)]
citiDat$SEARCH = paste(citiDat$lat, citiDat$long, "10mi", sep = ",")
citiDat = citiDat[,c(1,2,5)]

# Extracting the tweets
tweets = data.frame()
for (i in 1:dim(citiDat)[1]){
  tw=searchTwitter("I FEEL", n=40, geocode=citiDat[i,]$SEARCH, since = '2015-02-06')
  if(length(tw)==0) next # to handle exception for cities with no tweets
  tweets = rbind(tweets, data.frame(City = citiDat[i,1], Country = citiDat[i,2], tweets = twListToDF(tw)$text))
}

# Download lexicon from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar if not in the dir below
hu.liu.pos = scan('lexicon/positive-words.txt',  what='character', comment.char=';')
hu.liu.neg = scan('lexicon/negative-words.txt',  what='character', comment.char=';')

# Scoring the sentiment of the people; this is a basic approach; look at n-gram approach and fuzzy match
tweets$Score = 0
for(i in 1:dim(tweets)[1]){
  sentence = as.character(tweets[i,3])
  sentence = gsub('[[:punct:]]','',sentence) # removing the punctuation
  sentence = gsub('[[:cntrl:]]','',sentence) # removing the control symbols
  sentence = gsub('\\d+','',sentence)        # removing the digits
  sentence = tolower(sentence)
  word.list= str_split(sentence, '\\s+')     # splitting the sentence with the space
  words    = unlist(word.list)
  pos.matches = match(words, hu.liu.pos)     # match return NA if absent or some no. if present
  neg.matches = match(words, hu.liu.neg)
  tweets[i,4] = sum(!is.na(pos.matches)) - sum(!is.na(neg.matches)) 
}

# Getting the total tweets and the average sentiment
result = sqldf("SELECT City, COUNT(*) AS Tweets, AVG(Score) AS Sentiment FROM tweets GROUP BY City")

# Plotting the results
plot(result$Tweets, result$Sentiment, col = result$City, pch = 3, cex = 4)
text(result$Tweets+0.05, result$Sentiment+0.02, labels=as.character(result$City))

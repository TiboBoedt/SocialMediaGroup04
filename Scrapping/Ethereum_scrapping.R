get_token()
setwd("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Scraping")
#strings
search.string <- c("ETH", "#ETH", "#Ethereum", "Ethereum")

for(i in 1:length(search.string)){
  search <- search_tweets(search.string[i], n = 1000, lang = "en", include_rts = F)
  if(i == 1){
    tweets = search
  }
  else {
    tweets = rbind(tweets, search)
  }
}

#csv inladen
Ethereum_csv <- read_twitter_csv("Ethereum.csv")
#tweets toevegen
tweets <- rbind(Ethereum_csv, tweets)
#csv writen
write_as_csv(tweets, "Ethereum")

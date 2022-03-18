get_token()
setwd("/Users/thomassuys/OneDrive/UGent/MA1 HIR/Semester2/SMWA/Scraping groupwork")

#strings
search.string <- c("ADA", "#ADA", "#Cardano", "Cardano")

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
Cardano_csv <- read_twitter_csv("Cardano_thomas.csv")
#tweets toevegen
tweets_final <- rbind(Cardano_csv, tweets)
#csv writen
write_as_csv(tweets_final, "Cardano_thomas")

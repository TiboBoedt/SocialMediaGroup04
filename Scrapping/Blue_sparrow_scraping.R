get_token()
setwd("/Users/thomassuys/OneDrive/UGent/MA1 HIR/Semester2/SMWA/Scraping groupwork")

#strings
search.string <- c("#BlueSparrow", "bluesparrow", "BlueSparrow", "BLUESPARROW","bluesparrowtoken")

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
BlueSparrow_csv <- read_twitter_csv("BlueSparrow_thomas.csv")
#tweets toevegen
tweets_final <- rbind(BlueSparrow_csv, tweets)
#csv writen
write_as_csv(tweets_final, "BlueSparrow_thomas")

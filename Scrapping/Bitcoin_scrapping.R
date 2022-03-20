get_token()
setwd("/Users/thomassuys/OneDrive/UGent/MA1 HIR/Semester2/SMWA/Scraping groupwork")

#strings
search.string <- c("#bitcoin", "BTC", "bitcoin", "#BTC")

for(i in 1:length(search.string)){
  search <- search_tweets(search.string[i], n = 1000, lang = "en", include_rts = F)
  if(i == 1){
    tweets = search
  }
  else {
    tweets = rbind(tweets, search)
  }
}


bitcoin_csv <- read_twitter_csv("C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\Bitcoin.csv")

tweets <- rbind(bitcoin_csv, tweets)

write_as_csv(tweets, "C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\Bitcoin")


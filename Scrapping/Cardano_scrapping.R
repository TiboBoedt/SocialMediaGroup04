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


Cardano_csv <- read_twitter_csv("C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\Cardano.csv")

tweets <- rbind(Cardano_csv, tweets)

write_as_csv(tweets, "C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\Cardano")


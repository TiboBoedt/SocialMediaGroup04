get_token()
setwd("/Users/thomassuys/OneDrive/UGent/MA1 HIR/Semester2/SMWA/Scraping groupwork")

#strings
search.string <- c("SAND", "#SAND", "#TheSandbox", "The Sandbox", "Sandbox", "Sandbox")

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
<<<<<<< HEAD
TheSandbox_csv <- read_twitter_csv("C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\TheSandbox.csv")
=======
TheSandbox_csv <- read_twitter_csv("TheSandbox_thomas.csv")
>>>>>>> 9f987a183b4dbac2fb30b72e4a5612e02ace3a20
#tweets toevegen
tweets_final <- rbind(TheSandbox_csv, tweets)
#csv writen
<<<<<<< HEAD
write_as_csv(tweets, "C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\TheSandbox.csv")
=======
write_as_csv(tweets_final, "TheSandbox_thomas")
>>>>>>> 9f987a183b4dbac2fb30b72e4a5612e02ace3a20

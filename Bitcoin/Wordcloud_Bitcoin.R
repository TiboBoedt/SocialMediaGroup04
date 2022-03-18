############################# Bitcoin Wordcloud ################################
#To get a quick view on some of the most common words in tweets related to bitcoin
#we start by creating various wordclouds. We can do this for the whole dataset of 
#tweets at once, or an a daily base. 

################################################################################
############################ Read the csv file #################################
################################################################################

btc_tweets_df <- rtweet::read_twitter_csv("C:\\Users\\Boedt\\OneDrive\\Bureaublad\\Csv_Scrapping\\Bitcoin.csv")

################################################################################
############################# Wordcloud ########################################
################################################################################

#-# For the complete dataset #-#

#extract the tweets from btc_tweets_df
btc_tweets <- btc_tweets_df %>% pull(text)

#Create a corpus and pre-process the text 
btc_corpus <- btc_tweets %>% VectorSource() %>% Corpus() %>% 
  tm_map(content_transformer(str_to_lower)) %>% tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>%
  tm_map(removeWords, stop_words$word) %>% tm_map(removeWords, c("#bitcoin", "BTC", "bitcoin", "#BTC"))

unique_word_count(btc_corpus) #functions stored in the FunctionsTekstManipulation.R file
#56865 -> a lot of words: we will continue to clean the tweets, to make sure 
#we are left with the most important words, which are the ones we are interesed in

#running a spell check on a dataset this big will take forever and the added value
#of it does not compensate for this. Mainly because we assume the majority of the important
#words will be spelled correctly and common mistakes or "slang" will still occure 
#often enough

#Instead we check the total amount of unique words for different levels of sparsity 
#we start by creating a document-term matrix
btc_dtm <- DocumentTermMatrix(btc_corpus, control = list(wordlength = c(2,Inf)))
sparsity_levels <- seq(from = 0.9, to = 0.99, by = 0.01)
for(i in sparsity_levels){
  print(paste("sparsity level:", i, "unique words =", 
        unique_word_count(removeSparseTerms(btc_dtm, sparse = i))), sep = "")
}

btc_dtm_sparse <- btc_dtm %>% removeSparseTerms(sparse = 0.9)
as.matrix(btc_dtm_sparse)

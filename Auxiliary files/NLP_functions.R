generateWordcloud <- function(tweets_df, date = as_date(today()), cloud1 = T, wordsToRemove = c()){
  #tweets_df: dataframe from rtweet package
  #date: the data of the tweets from which we want to create a wordcloud
  #cloud: TRUE (default) -> use wordcloud function, FALSE -> use wordcloud2 function
  #wordsToRemove -> vector containing extra words we need removed from the cloud
  
  #isolate the tweets from the given date
  tweets_date <- tweets_df[which(as_date(tweets_df$created_at) %in% date),] %>% pull(text)
  
  if(is_empty(tweets_date)){
    cat("No tweets on this date in dataframe, try another date!")
  }
  else {
    #create corpus and start text-preprocessing
    
    #corpus
    Corpus <- tweets_date %>% VectorSource() %>% Corpus()
    
    #text-preprocessing
    Corpus_clean <- Corpus %>% tm_map(content_transformer(str_to_lower)) %>% 
      tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    #remove additional words if asked
    if(!is_empty(wordsToRemove)){
      Corpus_clean <- Corpus_clean %>% tm_map(removeWords, wordsToRemove)
    }
    
    dtm <- DocumentTermMatrix(Corpus_clean)
    m <- as.matrix(dtm)
    freq <- sort(colSums(m), decreasing = TRUE)
    t <- tibble(word = names(freq), freq = freq)
    
    if(cloud1){
      wordcloud(t$word, t$freq)
    }
    else {
      wordcloud2(data.frame(word = t$word, freq = t$freq))
    }
  }
  
}

generateWordgraph <- function(tweets_df, date = as_date(today()), probs = 0.99){
  #tweets_df: dataframe from rtweet package
  #date: the data of the tweets from which we want to create a wordcloud
  #probs = select only vertexes with degree greater than or equal to quantile given by the value of probs
  
  tweets_date <- tweets_df[which(as_date(tweets_df$created_at) == date),] %>% pull(text)
  
  tweets_date <- tweets_date %>% iconv('latin1', 'ascii', sub = '')
  
  dtm <- create_document_term_matrix(tweets_date) #performs text pre-processing as well!!!
  
  adj <- create_adjacency_matrix(dtm, probs = probs)
  
  plot_network(adj)
}
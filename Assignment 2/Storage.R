getSentimentDaily <- function(tweet_df, lexicon){
  unique_d <- unique(tweet_df$date)
  output_df <- data.frame(date, sentiment)
  for(date in unique_d){
    text <- tweet_df$text[which(tweet_df$date == date)]
    
    scoretweet <- numeric(length(text))
    
    for (i in 1:length(text)){
      
      text <- tolower(text)
      tweetsplit <- str_split(text[i]," ")[[1]] 
      m <- match(tweetsplit, dictionary$Word)
      present <- !is.na(m)
      wordvalences <- dictionary$VALENCE[m[present]]
      scoretweet[i] <- mean(wordvalences, na.rm=TRUE)
      if (is.na(scoretweet[i])) scoretweet[i] <- 0 else scoretweet[i] <- scoretweet[i]
    }
    output_df <- rbind(output_df, data.frame(date = date, sentiment = mean(scoretweet)))
  }
  return(output_df)
}

input <- data.frame(text = bitcoin$text, date = bitcoin$created_at)

getSentimentDaily(input, dictionary)
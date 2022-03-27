bing <- read_csv("bing_updated")

countUnigramsSent <- function(df, dict, sentiment){
  dict <- dict %>% filter(sentiment == sentiment)
  text <- df %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "") %>% str_squish()
  output <- numeric(length(text))
  for(i in 1:length(text)){
    unigrams <- str_split(text[i], " ")[[1]]
    m <- match(unigrams, dict$word)
    p <- !is.na(m)
    
    count <- sum(p)
    if(is.na(count)) output[i] = 0 else output[i] = count
  }
  return(output)
}

countBigramsSent <- function(df, dict, sentiment){
  dict <- dict %>% filter(sentiment == sentiment)
  text <- df %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "") %>% str_squish()
  output <- numeric(length(text))
  for(i in 1:length(text)){
    unigram <- str_split(text[i], " ")[[1]]
    
    bigrams <- rbind(unigram, c(unigram[2:length(unigram)], " "))
    bigrams <- paste(bigrams[1,1:ncol(bigrams)], bigrams[2, 1:ncol(bigrams)], " ")
    
    m <- match(bigrams, dict$word)
    p <- !is.na(m)
    
    count <- sum(p)
    
    output[i] <- count
  }
  return(output)
}

countPunct <- function(df, punct){
  if(punct == "?" | punct == "."){
    punct <- paste("\\", punct, sep = "")
  }
  text <- df %>% pull(text)
  output <- numeric(length(text))
  for(i in 1:length(text)){
    output[i] <- str_count(text[i], punct)
  }
  return(output)
}

lookupWordBinary <- function(df, word){
  word <- word %>% str_to_lower() #zekerheid
  text <- df %>% pull(text) %>% str_to_lower %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "") %>% str_squish()
  output <- numeric(length(text))
  for(i in 1:length(text)){
    count <- str_count(text[i], word)
    output[i] <- ifelse(count > 0, 1, 0)
  }
  return(output)
}

getLexiconSentiment <- function(df, dict){
  text <- df %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "") %>% str_squish()
  output <- numeric(length(text))
  for(i in 1:length(text)){
    text_unigram <- str_split(text[i], " ")[[1]]
    
    bigram <- rbind(text_unigram, c(text_unigram[2:length(text_unigram)], " "))
    text_bigram <- paste(bigram[1,1:ncol(bigram)], bigram[2, 1:ncol(bigram)])
    
    text_v <- c(text_unigram, text_bigram)
    
    m <- match(text_v, dict$word)
    
    p <- !is.na(m)
    
    present_score <- dict$sentiment_score[m[p]]
    
    output[i] = sum(present_score, na.rm = T)/sum(p)
    
    if(str_count(text[i], "whale") > 0){
      output[i] = output[i] * 1.5
    }
    if(str_count(text[i], "!") > 0){
      output[i] = output[i] * 1.5
    }
    
    if (is.na(output[i])) output[i] <- 0 else output[i] <- output[i]
    
  }
  return(output)
}



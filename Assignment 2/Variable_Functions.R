test <- read_csv("bing_updated")

countUnigramsSent <- function(df, dict, sentiment){
  dict <- dict %>% filter(sentiment == sentiment)
  text <- df %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "")
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

tic()
#countUnigramsSent(Bitcoin, bing, "negative")
toc()

countBigramsSent <- function(df, dict, sentiment){
  dict <- dict %>% filter(sentiment == sentiment)
  text <- df %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "")
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

tic()
t <- countBigramsSent(Bitcoin, bing, "positive")
toc()

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

tic()
countPunct(Bitcoin, "!")
toc()

tic()
countPunct(Bitcoin, "?")
toc()

tic()
countPunct(Bitcoin, ".")
toc()

Bitcoin$text[1]

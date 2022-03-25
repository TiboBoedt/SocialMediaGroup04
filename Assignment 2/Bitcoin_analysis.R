################################################################################
############################## Bitcoin Discreptives ############################
################################################################################

### FILE
Bitcoin <- correct_tweet_df #put in file


### WORDCLOUD & WORDGRAPH 
################################################################################
#first run the files "FunctionsTekstManipulatie.R" and "NLP_functions.R"

#We use the functions "generateWordcloud" and "generateWordgraph" to generate a wordcloud
#and wordgraph for each date available in the dataset. This why we get a quick idea
#of the talks of that day regarding bitcoin (and crypto)

(unique_dates <- unique(Bitcoin$created_at))
length(unique_dates)

unique_dates[1] #2022-03-10

remove1 <- c("btc", "bitcoin", "crypto", "cryptocurrency", "eth", "amp", "price")
generateWordcloud(Bitcoin, cloud1 = F, wordsToRemove = remove1, date = unique_dates[1])
generateWordgraph(Bitcoin, date = unique_dates[1])

#We notice two important clusters in the graph. One is more about the link of bitcoin
#and the ongoing war in Ukraine and the Second one more about the bitcoin itself. 
#We notice the connection between "buy" and "bitcoin" while "sell" is nowhere to be seen. 
#However we do also notice the word "don't" close to "get" and more, this could indicate
#more negative talks.

unique_dates[2] #2022-03-11
remove2 <- c("btc", "bitcoin", "crypto", "eth")
generateWordcloud(Bitcoin, date = unique_dates[2], cloud1 = F, wordsToRemove = remove2)
generateWordgraph(Bitcoin, date = unique_dates[2])

#we see an intesting cluster with the words bullish, buy, signals

unique_dates[3] #2022-03-12
remove3 <- c("btc", "bitcoin", "eth", "now", "crypto")
generateWordcloud(Bitcoin, date = unique_dates[3], cloud1 = F, wordsToRemove = remove3)
generateWordgraph(Bitcoin, date = unique_dates[3])

unique_dates[4] #2022-03-13
remove4 <- c("btc", "bitcoin", "cryptocurrency", "crypto", "eth", "price", "volume")
generateWordcloud(Bitcoin, date = unique_dates[4], cloud1 = F, wordsToRemove = remove4)
generateWordgraph(Bitcoin, date = unique_dates[4])

unique_dates[5] #2022-03-14
remove5 <- c("btc", "bitcoin", "eth", "price", "crypto", "cryptocurrency")
generateWordcloud(Bitcoin, date = unique_dates[5], cloud1 = F, wordsToRemove = remove5)
generateWordgraph(Bitcoin, date = unique_dates[5])

unique_dates[6] #2022-03-15
remove6 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[6], cloud1 = F, wordsToRemove = remove6)
generateWordgraph(Bitcoin, date = unique_dates[6])

unique_dates[7] #2022-03-16
remove7 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[7], cloud1 = F, wordsToRemove = remove7)
generateWordgraph(Bitcoin, date = unique_dates[7])

unique_dates[8] #2022-03-17
remove8 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[8], cloud1 = F, wordsToRemove = remove8)
generateWordgraph(Bitcoin, date = unique_dates[8])

unique_dates[9] #2022-03-18
remove9 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[9], cloud1 = F, wordsToRemove = remove9)
generateWordgraph(Bitcoin, date = unique_dates[9])

unique_dates[10] #2022-03-19
remove10 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[10], cloud1 = F, wordsToRemove = remove10)
generateWordgraph(Bitcoin, date = unique_dates[10])

unique_dates[11] #2022-03-20
remove11 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[11], cloud1 = F, wordsToRemove = remove11)
generateWordgraph(Bitcoin, date = unique_dates[11])

unique_dates[12] #2022-03-21
remove12 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[12], cloud1 = F, wordsToRemove = remove12)
generateWordgraph(Bitcoin, date = unique_dates[12])

unique_dates[13] #2022-03-22
remove13 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[13], cloud1 = F, wordsToRemove = remove13)
generateWordgraph(Bitcoin, date = unique_dates[13])

unique_dates[14] #2022-03-23
remove14 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[14], cloud1 = F, wordsToRemove = remove14)
generateWordgraph(Bitcoin, date = unique_dates[14])

unique_dates[15] #2022-03-24
remove15 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price")
generateWordcloud(Bitcoin, date = unique_dates[15], cloud1 = F, wordsToRemove = remove15)
generateWordgraph(Bitcoin, date = unique_dates[15])

################################################################################

#After quickly analysing the talk of the day we now take a closer look using word
#embeddings to better understand which words are close to some relevant terms like 
#bitcoin. We can use the output of this in the sentiment analysis to enrich our
#dictionary for example. 

### WORD EMBEDDING
################################################################################

### Word2Vec 

## skip-gram

#Start by creating a word2vec model for the full dataset. Afterwards we look on a day
#to day basis.
#We also check the results for different values of the hyperparamters 
skipgram_text <- Bitcoin %>% pull(text) %>% str_to_lower()
model_skipgram1 <- word2vec(skipgram_text, type = "skip-gram", dim = 15, iter = 50, window = 5)

#We start with looking to the words close to bitcoin and btc, which are our target words
predict(model_skipgram1, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)

model_skipgram2 <- word2vec(skipgram_text, type = "skip-gram", dim = 100, iter = 50, window = 5)
predict(model_skipgram2, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)

model_skipgram3 <- word2vec(skipgram_text, type = "skip-gram", dim = 15, iter = 50, window = 3)
predict(model_skipgram3, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)

model_skipgram4 <- word2vec(skipgram_text, type = "skip-gram", dim = 100, iter = 50, window = 3)
predict(model_skipgram4, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)

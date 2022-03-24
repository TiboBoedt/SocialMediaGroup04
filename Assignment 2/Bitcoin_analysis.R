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
generateWordcloud(Bitcoin, cloud1 = F, wordsToRemove = remove, date = unique_dates[1])
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
remove3 <- c("btc", "bitcoin", "crypto", "eth", "price", "now", "new")
generateWordcloud(Bitcoin, date = unique_dates[3], cloud1 = F, wordsToRemove = remove3)
generateWordgraph(Bitcoin, date = unique_dates[3])

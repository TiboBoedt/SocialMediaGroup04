################################################################################
############################## Bitcoin Discreptives ############################
################################################################################

### FILE
Bitcoin <- read_twitter_csv("Bitcoin2_no_spam.csv") #put in file


### WORDCLOUD & WORDGRAPH 
################################################################################
#first run the files "FunctionsTekstManipulatie.R" and "NLP_functions.R"

#We use the functions "generateWordcloud" and "generateWordgraph" to generate a wordcloud
#and wordgraph for each date available in the dataset. This why we get a quick idea
#of the talks of that day regarding bitcoin (and crypto)

(unique_dates <- unique(Bitcoin$created_at))
length(unique_dates)

unique_dates[1] #2022-03-10

remove1 <- c("btc", "bitcoin", "crypto", "cryptocurrency", "eth", "amp", "price", "btcusd", "just", "now", "btcusdt", "binance")
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

unique_dates[15] #2022-03-25
remove15 <- c("bitcoin", "btc", "eth", "crypto", "cryptocurrency", "price", "xrp", "ethereum", "binance")
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
#btc is close to numbers? For bitcoin we more terms containing the word crypto, but not
#necessarily words attracting our attention

model_skipgram2 <- word2vec(skipgram_text, type = "skip-gram", dim = 100, iter = 50, window = 5)
predict(model_skipgram2, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)
#bitcoin and btc are now closest to each other, as a result of increasing the dimension 
#of the vector. Another word that gets my attention is the word "antifud", let's see
#what that is close to.
predict(model_skipgram2, newdata = c("antifud"), type = "nearest", top_n = 5)
predict(model_skipgram2, newdata = c("fightfomo"), type = "nearest", top_n = 5)
predict(model_skipgram2, newdata = c("bitcoinislove"), type = "nearest", top_n = 5)

model_skipgram3 <- word2vec(skipgram_text, type = "skip-gram", dim = 15, iter = 50, window = 3)
predict(model_skipgram3, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)
predict(model_skipgram3, newdata = c("digitalassets"), type = "nearest", top_n = 5)
predict(model_skipgram3, newdata = c("carbonaraindex"), type = "nearest", top_n = 5)


model_skipgram4 <- word2vec(skipgram_text, type = "skip-gram", dim = 100, iter = 50, window = 3)
predict(model_skipgram4, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)


## cbow
cbow_text <- Bitcoin %>% pull(text) %>% str_to_lower()
model_cbow1 <- word2vec(cbow_text, type = "cbow", dim = 15, iter = 50, window = 5)
predict(model_cbow1, newdata = c("btc", "bitcoin"), type = "nearest", top_n = 5)
#cbow seems to make different associations and I would say more interesting on first sight
predict(model_cbow1, newdata = c("centralized", "centralised", "actively"), 
        type = "nearest", top_n = 5)
#centralized -> idiots thinking money should be decentralized :(
#Word embedding seems to imply that talks about bitcoin and maybe crypto in general 
#are the it should or can replace the traditional "fiat" money. 
predict(model_cbow1, newdata = c("physical", "wealthy", "companies", "shitcoins"), 
        type = "nearest", top_n = 5)

model_cbow2 <- word2vec(cbow_text, type = "cbow", dim = 100, iter = 50, window = 5)
predict(model_cbow2, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)
#the increase of dimension does not seem to be too usefull

model_cbow3 <- word2vec(cbow_text, type = "cbow", dim = 15, iter = 50, window = 3)
predict(model_cbow3, newdata = c("btc", "bitcoin"), type = "nearest", top_n = 5)
predict(model_cbow3, newdata = c("investing", "abuse", "recovers"), type = "nearest", top_n = 5)
predict(model_cbow3, newdata = c("safest", "scaling", "decentralization", "taxation",
                                 "plummets", "skyrockets"), top_n = 5)
predict(model_cbow3, newdata = c("thedipindex", "priceanalysis"), type = "nearest", top_n = 5)
#dipindex -> https://buythedips.io/
predict(model_cbow3, newdata = c("bitcoinprice", "bitcoinanalysis"), type = "nearest", top_n = 5)
predict(model_cbow3, newdata = c("tradingtips"), type = "nearest", top_n = 5)
predict(model_cbow3, newdata = c("bitcoinnewstoday"), type = "nearest", top_n = 10)
#-> still some spam present in dataset ? 

model_cbow4 <- word2vec(cbow_text, type = "cbow", dim = 15, iter = 50, window = 8)
predict(model_cbow4, newdata = c("bitcoin", "btc"), type = "nearest", top_n = 5)
predict(model_cbow4, newdata = c("bitstream", "taxed", "thailand"), type = "nearest", top_n = 5)
predict(model_cbow4, newdata = c("pumping", "altseason"), type = "nearest", top_n = 5)
predict(model_cbow4, newdata = c("plummets"), type = "nearest", top_n = 5)
predict(model_cbow4, newdata = c("outperforms"), type = "nearest", top_n = 5)

#it looks like changing the hyperparameters of the word2vec effect the analysis

### GloVe
tokens <- space_tokenizer(Bitcoin$text %>% tolower() %>% removePunctuation() %>%
                            removeWords(words = stopwords()) %>% stripWhitespace())

it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

summary(vocab$term_count)
vocab <- prune_vocabulary(vocab, term_count_min = 7L)

vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove <- GloVe$new(rank = 85, 
                   x_max = 5)
word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = 20)

word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components)

glove_bitcoin <- word_vectors["bitcoin",,drop = F]

cos_sim <- sim2(x = word_vectors, y = glove_bitcoin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

glove_btc <- word_vectors["btc",,drop = F]
cos_sim <- sim2(x = word_vectors, y = glove_btc, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
#we see the word buy pop up! 

glove_buy <- word_vectors["buy",,drop = F]
cos_sim <- sim2(x = word_vectors, y = glove_buy, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
#buy and sell? buy the dip/now??

glove_sell <- word_vectors["sell",, drop = F]
cos_sim <- sim2(x = word_vectors, y = glove_sell, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

glove_dip <- word_vectors["dip",,drop = F]
cos_sim <- sim2(x = word_vectors, y = glove_dip, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

glove_pump <- word_vectors["pump",,drop = F]
cos_sim <- sim2(x = word_vectors, y = glove_pump, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

## T-SNE
#we will now try to visualize the embedding from the glove algorithmen using t-sne.
#We do this with different values for the hyperparameters
train_df <- data.frame(word_vectors) %>% rownames_to_column("word")

tsne1 <- Rtsne(train_df[,-1], dims = 2, perplexity = 30, verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne1$Y) %>%
  mutate(
    word = train_df$word,
    col = colors[train_df$word]) %>%
  left_join(vocab, by = c("word" = "term")) 

ggplot(plot_df, aes(X1, X2, label = word, color = col)) + 
  geom_text(size = 4) +
  xlab("") + ylab("") +
  theme(legend.position = "none")  

#-> to many words for the visualization to be usefull, maybe remove some sparse words
#in the vector step. 

### TOPIC MODELING
#as a final step in getting to know the content of the tweets we execute a topic
#analysis on the data. 

#get and clean the text
topic_text <- Bitcoin %>% pull(text) %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("[[:digit:]]", "") %>% str_squish()

text_df <- tibble(doc = 1:length(topic_text), text = topic_text)
head(text_df, 10)

freq <- text_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  count(doc,word, name = "freq", sort = TRUE)

dtm <- freq %>% cast_dtm(document = doc, term = word, value = freq)

ldas <- list()
j <- 0
for (i in 2:10) {
  j <- j+1
  print(i)
  ldas[[j]] <- LDA(x = dtm, k = i, control = list(seed = 1234))
}

(AICs <- data.frame(k = 2:10, aic = map_dbl(ldas, AIC)))

(K <- AICs$k[which.min(AICs$aic)])

topicmodel <- LDA(x = dtm, k = 5, control = list(seed = 1234))

#The model is created, let's now take a closer look at the different topics
#and their "content"

(topic_term <- tidy(topicmodel, matrix = 'beta'))
#first terms don't seem to belong to any topic, which makes sense given that 
#it aren't real words. 

#let's get the top 20 terms per topic to get a better understanding of the topics
top_terms <- topic_term %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>%
  arrange(topic, desc(beta))
top_terms[1:20,] #Topic 1
top_terms[21:40,] #Topic 2
top_terms[41:60,] #Topic 3
top_terms[61:80,] #Topic 4
top_terms[81:100,] #Topic 5

#each topic seems to have bitcoin at the top -> this makes sense as all the tweets
#we are analysing should be about bitcoin.

#Let's plot them to get a cleaner overview
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

topic_term %>% 
  group_by(topic) %>% 
  top_n(50, beta) %>% 
  pivot_wider(term, topic, values_from = "beta", names_prefix = 'topic', values_fill = 0) %>% 
  column_to_rownames("term") %>%  
  comparison.cloud(colors = brewer.pal(3, "Set2")) 
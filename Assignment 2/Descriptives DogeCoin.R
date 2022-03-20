################################################################################
############################# DESCRIPTIVES DOGECOIn ############################
################################################################################

#read in the csv file
Dogecoin <- read_twitter_csv("Dogecoin.csv")
#remove duplicate text 
Dogecoin <- Dogecoin %>% distinct(text, .keep_all = TRUE)


############################# Quick basic descriptives #########################
### WORDCLOUD

#Pre-process text
Dogecoin_text <- Dogecoin %>% pull(text)
Dogecoin_text_clean <- Corpus(VectorSource(Dogecoin_text)) %>% 
  tm_map(content_transformer(str_to_lower)) %>% tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% tm_map(removeWords, stop_words$word) %>% 
  tm_map(stripWhitespace)

dtm_Dogecoin <- DocumentTermMatrix(Dogecoin_text_clean, control = list(wordlength = c(2, Inf)))
dtm_Dogecoin #43390 terms -> remove sparse terms
dtm_Dogecoin_sparse <- removeSparseTerms(dtm_Dogecoin, sparse = 0.99)
dtm_Dogecoin_sparse #182 terms
m_Dogecoin <- as.matrix(dtm_Dogecoin_sparse)
v_Dogecoin <- sort(colSums(m_Dogecoin), decreasing = TRUE)
d_Dogecoin <- data.frame(word = names(v_Dogecoin), freq = v_Dogecoin)

#remove the search strings from the dataframe

# Also remove the word doge itself.

search_string_Dogecoin <- str_to_lower(c("SAND", "#SAND", "#TheDogecoin", "The Dogecoin", "Dogecoin", "Dogecoin","Doge"))
for(i in 1:length(search_string_Dogecoin)){
  d_Dogecoin <- d_Dogecoin %>% filter(word != search_string_Dogecoin[i])
}

wordcloud2(d_Dogecoin)

# What we notice it that other crypto currencies are often mentioned toghetter -> futher intdicating the correlation between crypto's.
# Could remove other cyrpto curriencies to provide a more hollistic view of dogecoin.

search_string_Dogecoin <- str_to_lower(c("btc","bitcoin","eth","shiba", "shibainu","ethereum", "xrp","sol","bnb","ada","shib","dot","polkadot","mana","luna","terra", "solana", "avax", "avalance", "matic", "polygon"))
for(i in 1:length(search_string_Dogecoin)){
  d_Dogecoin <- d_Dogecoin %>% filter(word != search_string_Dogecoin[i])
}
wordcloud2(d_Dogecoin)

#wordfrequency graph
ggplot(d_Dogecoin[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("The Dogecoin") +
  theme(axis.text=element_text(size=7))

#remove extra words if you wish
search_string_Dogecoin_extra <- c("btc","bitcoin","eth","shiba", "shibainu","ethereum", "xrp","sol","bnb","ada","shib","dot","polkadot","mana","luna","terra", "solana", "avax", "avalance", "matic", "polygon")
for(i in 1:length(search_string_Dogecoin_extra)){
  d_Dogecoin <- d_Dogecoin %>% filter(word != search_string_Dogecoin_extra[i])
}

wordcloud2(d_Dogecoin)

#wordfrequency graph after removing additional words
ggplot(d_Dogecoin[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("The Dogecoin") +
  theme(axis.text=element_text(size=7))

### WORDGRAPH


### WORD EMBEDDINGS
Dogecoin_embedding_tekst <- Dogecoin_text %>% str_to_lower()
Dogecoin_embedding_tekst
#word2vec
#skip-gram
model_skip <- word2vec(x = Dogecoin_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "skip-gram" )

#check the words near Dogecoin to get an idea how there has been talked about the coin
predict(model_skip, c("dogecoin", "doge"), type = "nearest", top_n = 5)

iconv("\U0001f92f", "UTF-8", "latin1")

#take a look at some intereseting words from the nearest words to Dogecoin & theDogecoin
predict(model_skip, c("shibaarmy", "xrpcommunity","\U0001f92f", "❤️❤️"), type = "nearest", top_n = 10)
predict(model_skip, c("shibarm", "xec", "iris"),
        type = "nearest", top_n = 10)

#cbow
model_cbow <- word2vec(x = Dogecoin_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "cbow")

#check the words near Dogecoin to get an idea how there has been talked about the coin
predict(model_cbow, c("dogecoin", "doge"), type = "nearest", top_n = 5)

#Shitcoin/memecoin/shiba inu/ dogearmy -> Correlation between shibainu and dogecoin
#Cbox -> better performance

#take a look at some intereseting words from the nearest words to Dogecoin & theDogecoin
predict(model_cbow, c("shibainu", "dogearmy", "diamondhands"), type = "nearest", top_n = 10)

predict(model_cbow, c("shitcoin", "memecoin", "babydoge","elon"), 
        type = "nearest", top_n = 10)

predict(model_cbow, c( "babydoge","elon","diamondhands"), type = "nearest", 
        top_n = 10)

predict(model_cbow, c("companies", "banks", "economy"), 
        type = "nearest", top_n = 10)

#GloVe
tokens <- space_tokenizer(Dogecoin_embedding_tekst %>% removePunctuation() %>%
                            removeNumbers() %>% stripWhitespace() %>%
                            removeWords(words = stop_words))

#create vocabulary
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

#we remove sparse words from the set -> check for different levels of sparsity
summary(vocab$term_count)
#start by removing term_count_min = 74 (third quantile) -> clear plot
vocab <- prune_vocabulary(vocab, term_count_min = 4L) 

vectorizer <- vocab_vectorizer(vocab)

#create the term co-occurance matrix
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove <- GloVe$new(rank = 86, x_max = 5)

word_vectors_main <- glove$fit_transform(tcm, n_iter = 20)

word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components)

#let's new again look at the words close to Dogecoin and theDogecoin
Dogecoin <- word_vectors["dogecoin", , drop = FALSE]

cos_sim_Dogecoin <- sim2(x = word_vectors, y = Dogecoin, 
                        method = "cosine", norm = "l2")

head(sort(cos_sim_Dogecoin[,1], decreasing = TRUE), 10)

Doge <- word_vectors["doge", , drop = FALSE]

#let's new use tsne to reduce the dimension of the wordvectors and visualize the
#embedding

train_df <- data.frame(word_vectors) %>%
  rownames_to_column("word")

tsne <- Rtsne(train_df[,-1], 
              dims = 2, perplexity = 30,
              verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
  mutate(
    word = train_df$word,
    col = colors[train_df$word]) %>%
  left_join(vocab, by = c("word" = "term")) 

ggplot(plot_df, aes(X1, X2, label = word, color = col)) + 
  geom_text(size = 4) +
  xlab("") + ylab("") +
  theme(legend.position = "none")   

### TOPIC MODELING
Dogecoin_text_clean2 <- Dogecoin_text %>% str_to_lower() %>% str_replace_all("[[:digit:]]", "") %>%
  str_replace_all("[[:punct:]]", "") %>% str_squish()

text_df <- tibble(doc = 1:length(Dogecoin_text_clean2), text = Dogecoin_text_clean2)

freq <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(doc,word, name = "freq", sort = TRUE)

dtm <- freq %>%
  cast_dtm(doc, word, freq)
dtm

ldas <- list()
j <- 0
for (i in 2:10) {
  j <- j+1
  print(i)
  ldas[[j]] <- LDA(x = dtm, k = i, control = list(seed = 1234))
}

AICs <- data.frame(k = 2:10, aic = map_dbl(ldas, AIC))

K <- AICs$k[which.min(AICs$aic)]

topicmodel <- LDA(x = dtm, k = K, control = list(seed = 1234))

topic_term <- tidy(topicmodel, matrix = 'beta')

top_terms <- topic_term %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

doc_topic <- tidy(topicmodel, matrix = 'gamma')

topics_gamma <- doc_topic %>% arrange(desc(gamma))

user_topic <- topics_gamma %>%
  group_by(document) %>%
  top_n(1, gamma)

user_topic_tweet <- user_topic %>% 
  add_column(Tweets = tweets$text[as.numeric(user_topic$document)])
user_topic_tweet %>% slice_head() 

######################### More in depth Descriptives ###########################

### TO DO

# Change text preproceesing
# But allow digits -> If digit is lower than current price indicates bearishnish ..
cleanText <- function(text) {
  clean_texts <- text %>%
    str_replace_all("<.*>", "") %>%                         # remove remainig emojis
    str_replace_all("&amp;", "") %>%                        # remove &
    str_replace_all("(RT|via)((?:\\b\\W*@\\w+)+)", "") %>%  # remove retweet entities
    str_replace_all("@\\w+", "") %>%                        # remove @ people, replace_tag() also works
    str_replace_all('#', "") %>%                            #remove only hashtag, replace_hash also works
    str_replace_all("[[:punct:]]", "") %>%                  # remove punctuation
    str_replace_all("[[:digit:]]", "") %>%                  # remove digits
    str_replace_all("http\\w+", "") %>%                     # remove html links replace_html() also works
    str_replace_all("[ \t]{2,}", " ") %>%                   # remove unnecessary spaces
    str_replace_all("^\\s+|\\s+$", "") %>%                  # remove unnecessary spaces
    str_trim() %>% 
    str_to_lower()
  return(clean_texts)
}
text_clean <- cleanText(text_clean)

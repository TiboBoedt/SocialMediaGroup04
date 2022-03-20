################################################################################
############################# DESCRIPTIVES SANDBOX #############################
################################################################################

#read in the csv file
sandbox <- read_twitter_csv("TheSandbox.csv")
#remove duplicate text 
sandbox <- sandbox %>% distinct(text, .keep_all = TRUE)


############################# Quick basic descriptives #########################
### WORDCLOUD

#Pre-process text
sandbox_text <- sandbox %>% pull(text)
sandbox_text_clean <- Corpus(VectorSource(sandbox_text)) %>% 
  tm_map(content_transformer(str_to_lower)) %>% tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% tm_map(removeWords, stop_words$word) %>% 
  tm_map(stripWhitespace)

dtm_sandbox <- DocumentTermMatrix(sandbox_text_clean, control = list(wordlength = c(2, Inf)))
dtm_sandbox #63928 terms -> remove sparse terms
dtm_sandbox_sparse <- removeSparseTerms(dtm_sandbox, sparse = 0.99)
dtm_sandbox_sparse #159 terms
m_sandbox <- as.matrix(dtm_sandbox_sparse)
v_sandbox <- sort(colSums(m_sandbox), decreasing = TRUE)
d_sandbox <- data.frame(word = names(v_sandbox), freq = v_sandbox)

#remove the search strings from the dataframe
search_string_sandbox <- str_to_lower(c("SAND", "#SAND", "#TheSandbox", "The Sandbox", "Sandbox", "Sandbox"))
for(i in 1:length(search_string_sandbox)){
  d_sandbox <- d_sandbox %>% filter(word != search_string_sandbox[i])
}

wordcloud2(d_sandbox)

#wordfrequency graph
ggplot(d_sandbox[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("The Sandbox") +
  theme(axis.text=element_text(size=7))

#remove extra words if you wish
search_string_sandbox_extra <- c("check", "earn", "march", "thesandbox", "chance", "starting", "daily", "metaverse")
for(i in 1:length(search_string_sandbox_extra)){
  d_sandbox <- d_sandbox %>% filter(word != search_string_sandbox_extra[i])
}

wordcloud2(d_sandbox)

#wordfrequency graph after removing additional words
ggplot(d_sandbox[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("The Sandbox") +
  theme(axis.text=element_text(size=7))

### WORDGRAPH


### WORD EMBEDDINGS
sandbox_embedding_tekst <- sandbox_text %>% str_to_lower()

#word2vec
#skip-gram
model_skip <- word2vec(x = sandbox_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "skip-gram" )

#check the words near sandbox to get an idea how there has been talked about the coin
predict(model_skip, c("sandbox", "thesandbox"), type = "nearest", top_n = 5)

#take a look at some intereseting words from the nearest words to sandbox & thesandbox
predict(model_skip, c("metaversegame", "passiveincome"), type = "nearest", top_n = 10)
predict(model_skip, c("virtualrealestate", "cryptogames", "cryptogaming"),
        type = "nearest", top_n = 10)

#cbow
model_cbow <- word2vec(x = sandbox_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "cbow")

#check the words near sandbox to get an idea how there has been talked about the coin
predict(model_cbow, c("sandbox", "thesandbox"), type = "nearest", top_n = 5)

#take a look at some intereseting words from the nearest words to sandbox & thesandbox
predict(model_cbow, c("game", "world"), type = "nearest", top_n = 10)
predict(model_cbow, c("universe", "creating", "multiplayer"), 
        type = "nearest", top_n = 10)
predict(model_cbow, c("acquisition", "design", "stadium"), type = "nearest", 
        top_n = 10)
predict(model_cbow, c("institutions", "environment", "enable"), type = "nearest",
        top_n = 10)
predict(model_cbow, c("companies", "banks", "regulations", "economy"), 
        type = "nearest", top_n = 10)

#GloVe
tokens <- space_tokenizer(sandbox_embedding_tekst %>% removePunctuation() %>%
                            removeNumbers() %>% stripWhitespace() %>%
                            removeWords(words = stop_words))

#create vocabulary
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

#we remove sparse words from the set -> check for different levels of sparsity
summary(vocab$term_count)
#start by removing term_count_min = 4 (first quadrant)
vocab <- prune_vocabulary(vocab, term_count_min = 4L) 

vectorizer <- vocab_vectorizer(vocab)

#create the term co-occurance matrix
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove <- GloVe$new(rank = 86, x_max = 5)

word_vectors_main <- glove$fit_transform(tcm, n_iter = 20)

word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components)

#let's new again look at the words close to sandbox and thesandbox
sandbox <- word_vectors["sandbox", , drop = FALSE]

cos_sim_sandbox <- sim2(x = word_vectors, y = sandbox, 
                method = "cosine", norm = "l2")

head(sort(cos_sim_sandbox[,1], decreasing = TRUE), 10)

thesandbox <- word_vectors["thesandbox", , drop = FALSE]

#die norm = 12 doe ambetant bruur

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
sandbox_text_clean2 <- sandbox_text %>% str_to_lower() %>% str_replace_all("[[:digit:]]", "") %>%
  str_replace_all("[[:punct:]]", "") %>% str_squish()

text_df <- tibble(doc = 1:length(sandbox_text_clean2), text = sandbox_text_clean2)

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



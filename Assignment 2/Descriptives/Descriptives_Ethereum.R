################################################################################
############################# DESCRIPTIVES ETHEREUM ############################
################################################################################

#read in the csv file
Ethereum <- read_twitter_csv("Ethereum.csv")
#remove duplicate text 
Ethereum <- Ethereum %>% distinct(text, .keep_all = TRUE)

############################# Quick basic descriptives #########################
### WORDCLOUD

#Pre-process text
Ethereum_text <- Ethereum %>% pull(text)
Ethereum_text_clean <- Corpus(VectorSource(Ethereum_text)) %>% 
  tm_map(content_transformer(str_to_lower)) %>% tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% tm_map(removeWords, stop_words$word) %>% 
  tm_map(stripWhitespace)

dtm_ethereum <- DocumentTermMatrix(Ethereum_text_clean, control = list(wordlength = c(2, Inf)))
dtm_ethereum #70147 terms -> remove sparse terms
dtm_ethereum_sparse <- removeSparseTerms(dtm_ethereum, sparse = 0.99)
dtm_ethereum_sparse #164 terms
m_ethereum <- as.matrix(dtm_ethereum_sparse)
v_ethereum <- sort(colSums(m_ethereum), decreasing = TRUE)
d_ethereum <- data.frame(word = names(v_ethereum), freq = v_ethereum)

#remove the search strings from the dataframe
search_string_ethereum <- str_to_lower(c("ETH", "#ETH", "#Ethereum", "Ethereum"))
for(i in 1:length(search_string_ethereum)){
  d_ethereum <- d_ethereum %>% filter(word != search_string_ethereum[i])
}

wordcloud2(d_ethereum)

#wordfrequency graph
ggplot(d_ethereum[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("Ethereum") +
  theme(axis.text=element_text(size=7))

#remove extra words if you wish
search_string_ethereum_extra <- c("nfts", "nft", "btc", "bitcoin", "crypto", 
                                  "blockchain", "project", "cryptocurrency")
for(i in 1:length(search_string_ethereum_extra)){
  d_ethereum <- d_ethereum %>% filter(word != search_string_ethereum_extra[i])
}

wordcloud2(d_ethereum)

#wordfrequency graph
ggplot(d_ethereum[1:20,], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("Ethereum") +
  theme(axis.text=element_text(size=7))

### WORD EMBEDDINGS
ethereum_embedding_tekst <- Ethereum_text %>% str_to_lower()

#word2vec
#skip-gram
model_skip <- word2vec(x = ethereum_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "skip-gram" )

#check the words near ethereum to get an idea how there has been talked about the coin
predict(model_skip, c("ethereum", "eth"), type = "nearest", top_n = 10)

eth <- predict(model_skip, c("ethereum", "btc", "bitcoin"), type = "embedding")
eth <- eth["bitcoin",] - eth["btc",] + eth["ethereum",]
predict(model_skip, newdata = eth, type = "nearest", top_n = 5)

model_cbow <- word2vec(x = ethereum_embedding_tekst, dim = 15, iter = 20, 
                       window = 5, type = "cbow")

predict(model_cbow, c("ethereum", "eth"), type = "nearest", top_n = 10)
#it seems like ethereum and bitcoin are well related to eachother

predict(model_cbow, c("blockchain"), type = "nearest", top_n = 10)
predict(model_cbow, c("cryptoinvestor", "smartcontract"), 
        type = "nearest", top_n = 10)

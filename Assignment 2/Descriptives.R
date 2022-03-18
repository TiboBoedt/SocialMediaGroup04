########################################################################
############################# DESCRIPTIVES #############################
########################################################################

#TO DO:FUNCTIES MAKEN VAN PREPROCESSING?


###### IMPORT STATEMENTS
library(pacman)
p_load(rtweet)
setwd("/Users/thomassuys/OneDrive/UGent/MA1 HIR/Semester2/SMWA/Scraping groupwork")

# !! still need to select the altcoin, to compare correlations with bitcoin price movements
bitcoin <- read_twitter_csv("Bitcoin_thomas.csv")
bitcoin <- bitcoin %>% distinct(text, .keep_all = TRUE)

shiba_inu <- read_twitter_csv("ShibaInu_thomas.csv")
shiba_inu <- shiba_inu %>% distinct(text, .keep_all = TRUE)

cardano <- read_twitter_csv("Cardano_thomas.csv")
cardano <- cardano %>% distinct(text, .keep_all = TRUE)

sandbox <- read_twitter_csv("TheSandbox_thomas.csv")
sandbox <- sandbox %>% distinct(text, .keep_all = TRUE)

dogecoin <- read_twitter_csv("Dogecoin_thomas.csv")
dogecoin <- dogecoin %>% distinct(text, .keep_all = TRUE)

ethereum <- read_twitter_csv("Ethereum_thomas.csv")
ethereum <- ethereum %>% distinct(text, .keep_all = TRUE)


###### INITIAL DESCRIPTIVES

### WORDCLOUDS
p_load(tm,wordcloud, wordcloud2, httr, tidyverse)

## Bitcoin

# text preprocessing of the tweets
forremoval <- stopwords('english')
text_bitcoin <- tweets_data(bitcoin[1:1000,]) %>% pull(text)    # vector memory exhausted?
text_bitcoin_cleaned <- Corpus(VectorSource(text_bitcoin)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c(forremoval))

tdm_bitcoin <- TermDocumentMatrix(text_bitcoin_cleaned)
m_bitcoin <- as.matrix(tdm_bitcoin)
v_bitcoin <- sort(rowSums(m_bitcoin),decreasing=TRUE)
d_bitcoin <- tibble(word = names(v_bitcoin),freq=v_bitcoin)

# remove search strings and order
search.string1 <- "#bitcoin"
search.string2 <- "BTC"
search.string3 <- "bitcoin"
search.string4 <- "#BTC"
search.string5 <- "BITCOIN"
search.string6 <- "btc"


d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string1))
d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string2))
d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string3))
d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string4))
d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string5))
d_bitcoin <- d_bitcoin %>% filter(word != tolower(search.string6)) %>% arrange(desc(freq))

wordcloud2(d_bitcoin)

# does not work, common problem: silently failing wordcloud
#figpath <- "/Users/thomassuys/SocialMediaGroup04/twitter_bird.png"
#wordcloud2(d_bitcoin, figPath = figpath, size = 1.5, color = "skyblue")
#letterCloud(d_bitcoin, word = "BITCOIN", wordSize = 1)

### VARIABLE IMPORTANCE
p_load(SnowballC, slam, tm, randomForest)

text_bitcoin <- tweets_data(bitcoin[1:500,]) %>% pull(text)    # vector memory exhausted?
text_bitcoin_rts <- iconv(text_bitcoin, 'latin1', 'ascii', sub = '')
retweetCount <- bitcoin[1:500,] %>% pull(retweet_count)

text_bitcoin_rts_cleaned <- Corpus(VectorSource(text_bitcoin_rts)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(forremoval))

dtm_rts <- DocumentTermMatrix(text_bitcoin_rts_cleaned, control = list(wordLengths = c(2, Inf)))
  
x <- as_tibble(as.matrix(dtm_rts))
y <- retweetCount

rf <- randomForest(x,y, importance=TRUE)
imp <- importance(rf) %>% 
  as_tibble(., rownames = 'Features') %>% 
  arrange(desc(`%IncMSE`))

varImpPlot(rf, type = 1)

### Word embeddings
p_load(word2vec, text2vec, Rtsne, scales, ggrepel, tidyverse, tm)

text_bitcoin <- tweets_data(bitcoin[1:500,]) %>% pull(text)    # vector memory exhausted?

text_clean <- text_bitcoin %>%
  str_to_lower() %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("[[:digit:]]", "") %>%
  str_remove(search.string1) %>%
  str_remove(search.string2) %>%
  str_remove(search.string3) %>%
  str_remove(search.string4) %>%
  str_remove(search.string5) %>%
  str_remove(search.string6) %>%
  str_squish()


model <- word2vec(x = text_clean, 
                  type = "skip-gram", 
                  dim = 15, 
                  iter = 50, 
                  window = 10,
                  stopwords = stopwords())

embedding <- as.matrix(model)

#lookslike <- predict(model, c("solid", "ipad"), type = "nearest", top_n = 5)

#look for analogies

train_df <- data.frame(embedding) %>% 
  rownames_to_column("word")

tsne <- Rtsne(train_df[,-1], 
              dims = 2, perplexity = 50,
              verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
  mutate(
    word = train_df$word,
    col = colors[train_df$word])

ggplot(plot_df, aes(X1, X2, label = word, color = col)) + 
  geom_text(size = 4) +
  xlab("") + ylab("") +
  theme(legend.position = "none")   


### TOPIC MODELING
p_load(wordcloud, tm, topicmodels, topicdoc, tidytext, textclean)

text_df <- tibble(doc= 1:length(text_clean), text = text_clean)

freq <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(doc,word, name = "freq", sort = TRUE)

dtm <- freq %>%
  cast_dtm(doc, word, freq)

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


### WORD GRAPHS
search_string <- c('#BTC', '#bitcoin','BITCOIN','BTC','bitcoin','btc')
create_document_term_matrix <- function(data, search_string){
  
  p_load(SnowballC, tm)
  myCorpus <- Corpus(VectorSource(data))
  
  cat("Transform to lower case \n")
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  cat("Remove punctuation \n")
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  cat("Remove numbers \n")
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  cat("Remove stopwords \n")
  myStopwords <- c(stopwords('english'))
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  
  cat("Remove search string \n")
  search_string <- search_string
  myCorpus <- tm_map(myCorpus, removeWords, search_string)
  
  # cat("Stem corpus \n")
  # myCorpus = tm_map(myCorpus, stemDocument);
  # myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus);
  
  cat("Create document by term matrix \n")
  myDtm <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
  myDtm
}
doc_term_mat <- create_document_term_matrix(text_clean, search_string)

create_adjacency_matrix <- function(object, probs=0.99){
  
  #object = output from function create_document_term_matrix (a document by term matrix)
  #probs = select only vertexes with degree greater than or equal to quantile given by the value of probs
  
  cat("Create adjacency matrix \n")
  p_load(sna)
  
  mat <- as.matrix(object)
  mat[mat >= 1] <- 1 #change to boolean (adjacency) matrix
  Z <- t(mat) %*% mat
  
  cat("Apply filtering \n")
  ind <- sna::degree(as.matrix(Z),cmode = "indegree") >= quantile(sna::degree(as.matrix(Z),cmode = "indegree"),probs=0.99)
  #ind <- sna::betweenness(as.matrix(Z)) >= quantile(sna::betweenness(as.matrix(Z)),probs=0.99)
  
  Z <- Z[ind,ind]        
  
  cat("Resulting adjacency matrix has ",ncol(Z)," rows and columns \n")
  dim(Z)
  list(Z=Z,termbydocmat=object,ind=ind)
}
adj_mat <- create_adjacency_matrix(doc_term_mat)

plot_network <- function(object){
  #Object: output from the create_adjacency_matrix function
  
  #Create graph from adjacency matrix
  p_load(igraph)
  g <- graph.adjacency(object$Z, weighted=TRUE, mode ='undirected')
  g <- simplify(g)
  
  #Set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- igraph::degree(g)
  
  layout <- layout.auto(g)
  opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
  #Adjust the widths of the edges and add distance measure labels
  #Use 1 - binary (?dist) a proportion distance of two vectors
  #The binary distance (or Jaccard distance) measures the dissimilarity, so 1 is perfect and 0 is no overlap (using 1 - binary)
  edge.weight <- 7  #A maximizing thickness constant
  z1 <- edge.weight*(1-dist(t(object$termbydocmat)[object$ind,], method="binary"))
  E(g)$width <- c(z1)[c(z1) != 0] #Remove 0s: these won't have an edge
  clusters <- spinglass.community(g)
  cat("Clusters found: ", length(clusters$csize),"\n")
  cat("Modularity: ", clusters$modularity,"\n")
  plot(g, layout=layout, vertex.color=rainbow(4)[clusters$membership], vertex.frame.color=rainbow(4)[clusters$membership] )
}
plot_network(adj_mat)



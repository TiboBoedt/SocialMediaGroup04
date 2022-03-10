unique_word_count <- function(data){
  content <- character(length(data))
  if (any(class(data) %in% c("VCorpus", "Corpus","SimpleCorpus"))) {
    for (i in 1:length(data)) content[i] <- data[[i]]$content
  } else {
    content <- data
  }
  uniquewords <- unique(unlist(map(str_split(as.character(content)," "),unique)))
  length(uniquewords)
}

correct <- function(word) { 
  # How dissimilar is this word from all words in the wordlist?
  edit_dist <- adist(word, wordlist)
  
  # Is there a word that reasonably similar? 
  # If yes, which ones? Select the first result (because wordlist is sorted from most common to least common)
  # If no, append the original word
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1] 
}

create_document_term_matrix <- function(data){
  
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
  
  # cat("Stem corpus \n")
  # myCorpus = tm_map(myCorpus, stemDocument);
  # myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus);
  
  cat("Create document by term matrix \n")
  myDtm <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
  myDtm
}

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

corpus2text <- function(corpus) {
  content <- character(length(corpus))
  if (any(class(corpus) %in% c("VCorpus", "Corpus","SimpleCorpus"))) {
    for (i in 1:length(corpus)) content[i] <- corpus[[i]]$content
  }
  content
}


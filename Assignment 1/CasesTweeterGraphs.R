
#provide your Twitter screen name (just an example)
my_name <- "suysthomas"
#provide the Twitter screen name of 4 of your followers

#get the user- object

  
  
  
  # Install and load packages
  if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)

#Make sure you have your authorization keys available 
#Do this with your own access tokens
source('tokensandkeys.R')

#for my_name #(make sure we dont bump into extraction limits)
#lookup_users does not have a retryonratelimit
status <- FALSE
while (status==FALSE) {
  rate <- rate_limits(get_token(), "lookup_users")
  status <- as.numeric(rate$remaining) > 50
  if (status) {
    
    cat('Extracting...')
    userInfo <- lookup_users(my_name)
  } else {
    cat("Waiting...")
    Sys.sleep(600) # wait 10 minutes
  }
}

#-# Extracting...

# this results in a user object (userInfo) with all kinds of information
glimpse(userInfo)

# get user- objects for all followers of my_name
# get_followers function has a retryonratelimit option, so you don't need the loop
# at this point we extract all followers and not just the ones
# This function returns the user IDs
firstdegree1 <- get_followers(
  my_name, n = userInfo$followers_count, retryonratelimit = TRUE
)

#If you want to get the users names you have to look up the IDs (NOT NEEDED!)
followers <- lookup_users(firstdegree1$user_id) %>% 
  pull(screen_name)

# this can take a while, depending on the size of the network

#this results in a tbl with rows equal to our number of followers.
#Let's check if the length is correct.
length(followers)
# 1549

userInfo$followers_count
# 1549

#Take a look at the followers
head(followers)

#now select indices of our 4 followers in follower_names
#and retrieve their user objects to get the second degree followers
#(If you are afraid of bumping into the rate limits, use the above loop)


##### Cretating MULTIPLE  followers list to Check randomness in the graph produced, When Seconddegree followers are set differently ######

follower_names1 <- followers[10:13]
follower_names2 <- followers[20:23]
follower_names3 <- followers[30:33]
follower_names4 <- followers[40:43]


seconddegree1 <- lookup_users(follower_names1)
seconddegree2 <- lookup_users(follower_names2)
seconddegree3 <- lookup_users(follower_names3)
seconddegree4 <- lookup_users(follower_names4)





###### Case 1  ##############


#see if we can find these followers
ind <- which(followers %in% seconddegree1$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree1)) {
  
  cat('... Scraping: ', seconddegree1$screen_name[i], '\n')
  
  followersseconddegree1 <- character(seconddegree1$followers_count[i]) #pre allocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree1 <- get_followers(seconddegree1$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree1) %>% pull(screen_name)
  
}

#now we have all the followers of four of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree1$screen_name,userInfo$screen_name)


#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

myCorpus <- Corpus(VectorSource(mm))
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

# Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

if (ncol(A) > 500) A <- A[1:500,1:500]
#Create a graph object based on the adjacency matrix & remove loop edges
g1 <- graph.adjacency(A, weighted=TRUE,
                     mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g1)$label <- V(g1)$name
V(g1)$degree <- igraph::degree(g1)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g1)

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g1)$name %in%
                                                   names(igraph::degree(g1)[tail(order(igraph::degree(g1)),5)]) ==TRUE,1,
                                                 ifelse(V(g1)$name %in%
                                                          names(igraph::degree(g1)[tail(order(igraph::betweenness(g1)),10)]) ==TRUE,2,3))])


net1<-network(A,matrix.type='adjacency')
summary(network(A,matrix.type='adjacency')) # density 0.36 , edges 91114
transitivity(g1) #0.951

### pareto approach

quantile_point_degree <- quantile(sort(degree(g1)),0.2)  ### 196
pareto_degree_followers1 <-which(degree(g1)>quantile_point_degree)
par_user<-as.matrix(userfollower[,pareto_degree_followers1])
A1 <- t(as.matrix(par_user)) %*% as.matrix(par_user)
par_g1 <- graph.adjacency(A1, weighted=TRUE,
                      mode ='undirected') %>% simplify()

par_net1<-network(A1,matrix.type='adjacency')
summary(par_net1) # density 0.76 
transitivity(par_g1) #0.99


plot(par_g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(par_g1)$name %in%
                                                   names(igraph::degree(par_g1)[tail(order(igraph::degree(par_g1)),5)]) ==TRUE,1,
                                                 ifelse(V(par_g1)$name %in%
                                                          names(igraph::degree(par_g1)[tail(order(igraph::betweenness(par_g1)),10)]) ==TRUE,2,3))])






############ Second Case    ########


#see if we can find these followers
ind <- which(followers %in% seconddegree2$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree2)) {
  
  cat('... Scraping: ', seconddegree2$screen_name[i], '\n')
  
  followersseconddegree2 <- character(seconddegree2$followers_count[i]) #pre allocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree2 <- get_followers(seconddegree2$user_id[i], 
                                          retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree2) %>% pull(screen_name)
  
}

#now we have all the followers of four of our followers
#let's add our followers to that list

l[[length(l)+1]] <- followers
names(l) <- c(seconddegree2$screen_name,userInfo$screen_name)


#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

myCorpus <- Corpus(VectorSource(mm))
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

# Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

if (ncol(A) > 500) A <- A[1:500,1:500]
#Create a graph object based on the adjacency matrix & remove loop edges
g1 <- graph.adjacency(A, weighted=TRUE,
                      mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g1)$label <- V(g1)$name
V(g1)$degree <- igraph::degree(g1)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g1)

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g1)$name %in%
                                                   names(igraph::degree(g1)[tail(order(igraph::degree(g1)),5)]) ==TRUE,1,
                                                 ifelse(V(g1)$name %in%
                                                          names(igraph::degree(g1)[tail(order(igraph::betweenness(g1)),10)]) ==TRUE,2,3))])

### metrics 
net2<-network(A,matrix.type='adjacency')
summary(net2) # 0.3658998  , edges 91292 
transitivity(g1) #0.8846498



### pareto approach

quantile_point_degree <- quantile(sort(degree(g1)),0.2) 
pareto_degree_followers1 <-which(degree(g1)>quantile_point_degree)
par_user<-as.matrix(userfollower[,pareto_degree_followers1])
A1 <- t(as.matrix(par_user)) %*% as.matrix(par_user)
par_g1 <- graph.adjacency(A1, weighted=TRUE,
                          mode ='undirected') %>% simplify()

par_net1<-network(A1,matrix.type='adjacency')
summary(par_net1) # density 0.48 , edges 74708   # vertices 393
transitivity(par_g1) #0.969


plot(par_g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(par_g1)$name %in%
                                                   names(igraph::degree(par_g1)[tail(order(igraph::degree(par_g1)),5)]) ==TRUE,1,
                                                 ifelse(V(par_g1)$name %in%
                                                          names(igraph::degree(par_g1)[tail(order(igraph::betweenness(par_g1)),10)]) ==TRUE,2,3))])


correlation_plot(par_user)


############ Third Case   #############

############ Third Case
#see if we can find these followers
ind <- which(followers %in% seconddegree3$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree3)) {
  
  cat('... Scraping: ', seconddegree3$screen_name[i], '\n')
  
  followersseconddegree3 <- character(seconddegree3$followers_count[i]) #pre allocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree3 <- get_followers(seconddegree3$user_id[i], 
                                          retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree3) %>% pull(screen_name)
  
}

#now we have all the followers of four of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree3$screen_name,userInfo$screen_name)


#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

myCorpus <- Corpus(VectorSource(mm))
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

# Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

if (ncol(A) > 500) A <- A[1:500,1:500]
#Create a graph object based on the adjacency matrix & remove loop edges
g1 <- graph.adjacency(A, weighted=TRUE,
                      mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g1)$label <- V(g1)$name
V(g1)$degree <- igraph::degree(g1)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g1)

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g1)$name %in%
                                                   names(igraph::degree(g1)[tail(order(igraph::degree(g1)),5)]) ==TRUE,1,
                                                 ifelse(V(g1)$name %in%
                                                          names(igraph::degree(g1)[tail(order(igraph::betweenness(g1)),10)]) ==TRUE,2,3))])

### metrics 
net3<-network(A,matrix.type='adjacency')
summary(net3) # 0.736  , edges 183854 
transitivity(g1) #0.9746498


### pareto approach

quantile_point_degree <- quantile(sort(degree(g1)),0.2)
pareto_degree_followers1 <-which(degree(g1)>quantile_point_degree)
par_user<-as.matrix(userfollower[,pareto_degree_followers1])
A1 <- t(as.matrix(par_user)) %*% as.matrix(par_user)
par_g1 <- graph.adjacency(A1, weighted=TRUE,
                          mode ='undirected') %>% simplify()

par_net1<-network(A1,matrix.type='adjacency')
summary(par_net1) # density 1 , edges vertices 431
transitivity(par_g1) #1

##### PAREto approach doesnt seem to have worked for this one ---> lowering further the qunatiles

quantile_point_degree <- quantile(sort(degree(g1)),0.05)
pareto_degree_followers1 <-which(degree(g1)>quantile_point_degree)
par_user<-as.matrix(userfollower[,pareto_degree_followers1])
A1 <- t(as.matrix(par_user)) %*% as.matrix(par_user)
par_g1 <- graph.adjacency(A1, weighted=TRUE,
                          mode ='undirected') %>% simplify()

par_net1<-network(A1,matrix.type='adjacency')
summary(par_net1) # density = 0.91 , vertices 437
transitivity(par_g1) #0.991
plot(par_g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(par_g1)$name %in%
                                                   names(igraph::degree(par_g1)[tail(order(igraph::degree(par_g1)),5)]) ==TRUE,1,
                                                 ifelse(V(par_g1)$name %in%
                                                          names(igraph::degree(par_g1)[tail(order(igraph::betweenness(par_g1)),10)]) ==TRUE,2,3))])


correlation_plot(par_user)




### Case 4

############ Third Case
#see if we can find these followers
ind <- which(followers %in% seconddegree4$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree4)) {
  
  cat('... Scraping: ', seconddegree4$screen_name[i], '\n')
  
  followersseconddegree4 <- character(seconddegree4$followers_count[i]) #pre allocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree4 <- get_followers(seconddegree4$user_id[i], 
                                          retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree4) %>% pull(screen_name)
  
}

#now we have all the followers of four of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree4$screen_name,userInfo$screen_name)


#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

myCorpus <- Corpus(VectorSource(mm))
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

# Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

if (ncol(A) > 500) A <- A[1:500,1:500]
#Create a graph object based on the adjacency matrix & remove loop edges
g1 <- graph.adjacency(A, weighted=TRUE,
                      mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g1)$label <- V(g1)$name
V(g1)$degree <- igraph::degree(g1)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g1)

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g1)$name %in%
                                                   names(igraph::degree(g1)[tail(order(igraph::degree(g1)),5)]) ==TRUE,1,
                                                 ifelse(V(g1)$name %in%
                                                          names(igraph::degree(g1)[tail(order(igraph::betweenness(g1)),10)]) ==TRUE,2,3))])

### metrics 
net4<-network(A,matrix.type='adjacency')
edges(net4) #edges 92606 
summary(net4)  # 0.3961381
transitivity(g1) #0.805



### pareto approach

quantile_point_degree <- 155#quantile(sort(degree(g1)),0.2) # 155
pareto_degree_followers1 <-which(degree(g1)>quantile_point_degree)
par_user<-as.matrix(userfollower[,pareto_degree_followers1])
A1 <- t(as.matrix(par_user)) %*% as.matrix(par_user)
par_g1 <- graph.adjacency(A1, weighted=TRUE,
                          mode ='undirected') %>% simplify()

par_net1<-network(A1,matrix.type='adjacency')
summary(par_net1) # density 0.61 , edges vertices 336
transitivity(par_g1) #0.88



plot(par_g1, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(par_g1)$name %in%
                                                   names(igraph::degree(par_g1)[tail(order(igraph::degree(par_g1)),5)]) ==TRUE,1,
                                                 ifelse(V(par_g1)$name %in%
                                                          names(igraph::degree(par_g1)[tail(order(igraph::betweenness(par_g1)),10)]) ==TRUE,2,3))])


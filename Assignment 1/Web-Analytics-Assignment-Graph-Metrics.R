###########################################################################
######## Web Analytics Assignment#########
rm(list=ls()) #Clean the environment
install.packages("twitteR") #install package
library(twitteR) #load package
library(network)
library(igraph)



# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)
 
#Do this with your own access tokens
source('tokensandkeys.R')

#provide your Twitter screen name (just an example)
my_name <- "suysthomas"
#provide the Twitter screen name of 4 of your followers
follower_names <- c("aube_schoelinck",'edith_lust','NasosTseros','AvetBirgit')
#get the user- object

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
## First degree users
firstdegree <- get_followers(
  my_name, n = userInfo$followers_count, retryonratelimit = TRUE
)

#If you want to get the users names you have to look up the IDs (NOT NEEDED!)
followers <- lookup_users(firstdegree$user_id) %>% 
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

seconddegree <- lookup_users(follower_names)

#this should be four:
nrow(seconddegree)

#see if we can find these followers
ind <- which(followers %in% seconddegree$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers
l <- list()
for (i in 1:nrow(seconddegree)) {
  
  cat('... Scraping: ', seconddegree$screen_name[i], '\n')
  
  followersseconddegree <- character(seconddegree$followers_count[i]) #pre allocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree <- get_followers(seconddegree$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree) %>% pull(screen_name)
  
  #If you would solely work with the IDs, one line of code is enough
  #l[[i]] <- get_followers(seconddegree$user_ids[i], 
   #                       retryonratelimit = TRUE) %>% pull(user_id)
}

#let's have a look
glimpse(l)

#now we have all the followers of four of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree$screen_name,userInfo$screen_name)

glimpse(l)

#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

### Loading the initial file of users we worked with 


load('followersoffollowers.rdata') # saved as mm



############### Process data to create adjacency matrix ###########

#Install and load the text mining package to preprocess the data
p_load(SnowballC, tm)

# transform that vector using the tm package to structure the unstructered data
# we will dive deeper in the tm package when discussing 'the message' on SM
myCorpus <- Corpus(VectorSource(mm))

#inspect the result
inspect(myCorpus)

# this creates a matrix, in which the rows are our sources of interest and the columns are the friends (see later for a discussion of this matrix)
# This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

#we can also look at the actual matrix
inspect(userfollower)


########################## Network Analysis ##############################
#Targeting social network users using restricted network information
#load the required packages
p_load(igraph)

# Compute the adjacency matrix using matrix multiplication.
# Note that this is not a sparse matrix format, so using larger datasets it could run slowly
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

#What does it look like?
A[1:10,1:10]

# Limit the dimensions
if (ncol(A) > 500) A <- A[1:500,1:500]

#Create a graph object based on the adjacency matrix & remove loop edges
g <- graph.adjacency(A, weighted=TRUE,
                     mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g)
#Give the graph lots of room

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g)$name %in%
                                                   names(igraph::degree(g)[tail(order(igraph::degree(g)),5)]) ==TRUE,1,
                                                 ifelse(V(g)$name %in%
                                                          names(igraph::degree(g)[tail(order(igraph::betweenness(g)),10)]) ==TRUE,2,3))])

# The top 5 vertices based on degree are in green
# The top 10 vertices based on betweenness (and not based on degree) are in red
# All the other vertices are in blue





###### PARETO APPROACH   ############

## We are going to apply the same plot but this time get as many colored points as 20% of the data based on degree
#pretty ,uch going for a pareto approach 

quantile_point_degree <- quantile(sort(degree(g)),0.2)
quantile_point_betweeness <-quantile(sort(betweenness(g)),0.8)

## Number of followers above the 80% quantile
length(which(degree(g)>quantile_point_degree)) # 99
length(which(betweenness(g)>quantile_point_betweeness)) # 100


windows()
mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g)$name %in%
                                                   names(igraph::degree(g)[degree(g)>quantile_point_degree]) ==TRUE,1,
                                                 ifelse(V(g)$name %in%
                                                          names(igraph::degree(g)[igraph::betweenness(g)>quantile_point_betweeness]) ==TRUE,2,3))])

### Getting the users who have a degree higher than the 20 % of the rest of the followers
## Making this approach so that 80% of the remaining followers will still be able to reconstruct the network without high change in the network metrics


quantile_point_degree <- quantile(sort(degree(g)),0.2) # 155
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






######################################################


### Sorting Followers by degree
sort(degree(g),decreasing = T)[1:10]
### Summary of the graph
summary(g)
### transitivity
transitivity(g) # 083




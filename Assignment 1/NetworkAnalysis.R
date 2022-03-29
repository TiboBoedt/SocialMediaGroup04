###########################################################################
######## NETWORK ANALYSIS #########
###########################################################################

rm(list=ls()) #Clean the environment

# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)

#Make sure you have your authorization keys available 
#Do this with your own access tokens
source('tokensandkeys.R')

get_token()
token

#provide your Twitter screen name 
my_name <- "suysthomas"


#get the user- object for my_name (make sure we don't bump into extraction limits)
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


userInfo$followers_count


#Take a look at the followers
head(followers)

#and retrieve their user objects to get the second degree followers
    #(If you are afraid of bumping into the rate limits, use the above loop)
seconddegree <- lookup_users(followers)

#check if this is correct compared to Twitter page
nrow(seconddegree)

#see if we can find these followers
ind <- which(followers %in% seconddegree$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree)) {
  
  cat('... Scraping: ', seconddegree$screen_name[i], '\n')
  
  followersseconddegree <- character(seconddegree$followers_count[i]) #preallocate vector
  
  followersseconddegree <- get_followers(seconddegree$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree) %>% pull(screen_name)
}

#let's have a look
glimpse(l)
save(l,file = 'l.Rdata')

#now we have all the followers of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree$screen_name,userInfo$screen_name)

glimpse(l)

#transform that list to a character vector.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

# save file
save(mm,file = "followers(offollowers).Rdata")

############### Process data to create adjacency matrix ###########

#Install and load the text mining package to preprocess the data and load the data
p_load(SnowballC, tm)
load("followers(offollowers).Rdata")

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

# if the adjacency matrix is too large, then you can consider to only take the first 500
# followers. This works good on a computer with 8GB RAM
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
                                                   names(igraph::degree(g)[tail(order(igraph::degree(g)),10)]) ==TRUE,1,
                                                 ifelse(V(g)$name %in%
                                                          names(igraph::degree(g)[tail(order(igraph::betweenness(g)),20)]) ==TRUE,2,3))])

influencers_degree <- names(igraph::degree(g)[tail(order(igraph::degree(g)),10)])
influencers_betweenness <- names(igraph::degree(g)[tail(order(igraph::betweenness(g)),10)])

# The top 10 vertices based on degree are in green
# The top 20 vertices based on betweenness (and not based on degree) are in red
# All the other vertices are in blue

# Rank vertices based on degree
degreetable <- as.data.frame(cbind(V(g)$label,V(g)$degree))
degreetable <- transform(degreetable, V2 = as.numeric(V2))
groundtruth <- (degreetable%>%arrange(desc(V2)))

############### Restrictiveness ###########

# step 1: get followers: followers
# step 2: get followers of followers: l
# step 3: get correlation for different levels of restrictiveness
size <- c(2:500)
ufmatrix <- as.matrix(userfollower)
correlation <- c()
set.seed(67)
correlationdf<- data.frame(row.names = size)
for(j in 1:10){
  randomsequence <- sample.int(500, 500)
  for(i in 2: 500) {  
      randomsubsequence <- randomsequence[1:i]
      ufm <- ufmatrix[,randomsubsequence]

    
    adj <- t(ufm) %*% ufm
    
    graph <- graph.adjacency(adj, weighted=TRUE,
                         mode ='undirected') %>% simplify()
  
    x <- V(graph)$name
    y <- igraph::degree(graph)
    degreesdf<- as.data.frame(cbind(x,y))
    degreesdf <- transform(degreesdf, y = as.numeric(y))
    currentranking <- (degreesdf%>%arrange(desc(y)))
    currentranking_degrees <- currentranking$y
    currentranking_names <- currentranking$x
    groundtruthtemp <- (groundtruth %>% filter(V1 %in% currentranking_names))$V2

    smcor <- cor(groundtruthtemp,currentranking_degrees,method="spearman")
    
    correlation <- c(correlation,smcor)
    if(i == 500) {
      colname <- paste("correlation_", j, sep = "")
      correlationdf[colname] <- correlation
      correlation <- c()
    }
  }
}

correlationdf$size <- size

p_load(ggplot2)

ggplot(correlationdf, aes(x=size)) + 
  geom_line(aes(y = correlation_1), color = "#000000") + 
  geom_line(aes(y = correlation_2), color = "#E69F00") +
  geom_line(aes(y = correlation_3), color = "#56B4E9") +
  geom_line(aes(y = correlation_4), color = "#009E73") +
  geom_line(aes(y = correlation_5), color = "#D55E00") +
  ylab("correlation") +
  theme_bw()




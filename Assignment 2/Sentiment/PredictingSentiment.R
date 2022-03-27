################################################################################
############################# SENTIMENT ML   ###################################
################################################################################

############################ Data Understanding ################################
################################################################################

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix)
setwd(dir ="/Users/xavierverbrugge/SocialMediaGroup04_2")
Bing_Dict <- read_csv("./Assignment 2/bing_updated")

SentimentReal <- read_csv("Tweets_And_Labels_2.csv")
Bitcoin <- read_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Scraping/Bitcoin.csv")
Encoding(SentimentReal$text) <- 'latin'
Encoding(Bitcoin$text) <- 'latin'

############################ Train and Test split ##############################
################################################################################

train <- SentimentReal
test <- Bitcoin

############################ Variables  ########################################
################################################################################

ExtractFeatures <- function(df){
  df$Nr_Exclemationmarks = countPunct(df, "!")
  df$Nr_QuestionMarks = countPunct(df, "?")
  df$Nr_OfPoints = countPunct(df, ".")
  df$Lexicon_Sentiment = getLexiconSentiment(df,Bing_Dict)
  
  df$Nr_OfPostiveUnigrams = countUnigramsSent(df,Bing_Dict ,"positive" )
  df$Nr_OfNegativeUnigrams = countUnigramsSent(df,Bing_Dict ,"negative" )
  df$Nr_Times_Bullish = lookupWordBinary(df, "Bullish")
  df$Nr_Times_Bearish = lookupWordBinary(df, "Bearish")
  df$Nr_Times_Moon = lookupWordBinary(df, "Moon")
  df$Nr_Times_HODL = lookupWordBinary(df, "HODL")
  df$Nr_Times_Pump = lookupWordBinary(df, "Pump")
  df$Nr_Times_Dump = lookupWordBinary(df, "Dump")
  df$Nr_Times_Bear = lookupWordBinary(df, "Bear")
  df$Nr_Times_Bull = lookupWordBinary(df, "Bull")
  df$Nr_Times_Buy = lookupWordBinary(df, "Buy")
  df$Nr_Times_Sell = lookupWordBinary(df, "Sell")
  df$Nr_Times_Whale = lookupWordBinary(df, "Whale")
  df$Nr_Times_FOMO = lookupWordBinary(df, "FOMO")
  df$Nr_Times_ATH = lookupWordBinary(df, "ATH")
  df$Nr_Times_Short = lookupWordBinary(df, "Short")
  df$Nr_Times_Long = lookupWordBinary(df, "Long")
  df$Nr_Times_Defi = lookupWordBinary(df, "Defi")
  #df$Nr_Times_Mooning = lookupWordBinary(df, "Mooning")
  df$Nr_Times_Decentralization = lookupWordBinary(df, "Decentralization")
  df$Nr_Times_centralization = lookupWordBinary(df, "Centralization")
  
  return(df)
}
  
train <- ExtractFeatures(train)
test <- ExtractFeatures(test)


############################ DTM Matrix ########################################
################################################################################

corpus_train <- Corpus(VectorSource(train$text))
corpus_test <- Corpus(VectorSource(test$text))

# N-Grams

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))

# Dtm

dtm_train <- DocumentTermMatrix(corpus_train, control = list(tokenize = Tokenizer,
                                                             weighting = function(x) weightTf(x),
                                                             RemoveNumbers=TRUE,
                                                             removePunctuation=TRUE,
                                                             stripWhitespace= TRUE))

dtm_test <- DocumentTermMatrix(corpus_test, control = list(tokenize = Tokenizer,
                                                           weighting = function(x) weightTf(x),
                                                           RemoveNumbers=TRUE,
                                                           removePunctuation=TRUE,
                                                           stripWhitespace= TRUE))

### Prepare test

prepareTest <- function (train, test) {
  Intersect <- test[,intersect(colnames(test), colnames(train))]
  diffCol <- dtm_train[,setdiff(colnames(train),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(train)]
}

dtm_test <- prepareTest(dtm_train, dtm_test)


# Convert to common sparse remix

dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

sm_train <- dtm.to.sm(dtm_train)
sm_test <- dtm.to.sm(dtm_test)


######################## Singular Value Deposition #############################
################################################################################

p_load(irlba)

# Set the k to 20.
trainer <- irlba(t(sm_train), nu=40, nv=40)
str(trainer)

tester <- as.data.frame(as.matrix(sm_test) %*% trainer$u %*% solve(diag(trainer$d)))
head(tester)

######################## Modelling and Evaluation ##############################
################################################################################

## Data gathering
x = as.data.frame(trainer$v)
x = cbind(x, train[,103:126])

test_combined = cbind(tester, test[,91:114])


y_train <- as.factor(train$label)

#Order columns in the df alphabetically
x = x %>% select(order(colnames(x)))
test_combined = test_combined  %>% select(order(colnames(test_combined )))

#Modelling

levels(y_train) =c(0, 1, 2,3, 4)
dtrain <- xgb.DMatrix(data =as.matrix(x), label = as.matrix((y_train)))

bstSparse <- xgboost(data = dtrain, max.depth = 5, eta = 0.01, nthread = 4, nrounds = 1000, num_class = 5 ,subsample = 0.8,objective = "multi:softmax")

pred <- predict(bstSparse, as.matrix(test_combined))

#Rescale predictions
#There is now data leakage if there are observations who are also in the bitcoin data set...
preds_rescaled = pred -4
Bitcoin$Sentiment_Label_Pred = preds_rescaled

write_csv(Bitcoin, file = "/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_And_Labels.csv")

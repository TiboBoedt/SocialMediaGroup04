################################################################################
############################# SENTIMENT ML   ###################################
################################################################################

############################ Data Understanding ################################
################################################################################

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix)
Bing_Dict <- read_csv("./Assignment 2/bing_updated")

SentimentReal <- read_twitter_csv("Tweets_And_Labels_2.csv")
Encoding(SentimentReal$text) <- 'latin'
SentimentReal %>% glimpse()

SentimentReal$text
############################ Variables  ########################################
################################################################################

SentimentReal$Nr_Exclemationmarks = countPunct(SentimentReal, "!")
SentimentReal$Nr_QuestionMarks = countPunct(SentimentReal, "?")
SentimentReal$Nr_OfPoints = countPunct(SentimentReal, ".")
SentimentReal$Lexicon_Sentiment = getLexiconSentiment(SentimentReal,Bing_Dict)

SentimentReal$Nr_OfPostiveUnigrams = countUnigramsSent(SentimentReal,Bing_Dict ,"positive" )
SentimentReal$Nr_OfNegativeUnigrams = countUnigramsSent(SentimentReal,Bing_Dict ,"negative" )
SentimentReal$Nr_Times_Bullish = lookupWordBinary(SentimentReal, "Bullish")
SentimentReal$Nr_Times_Bearish = lookupWordBinary(SentimentReal, "Bearish")
SentimentReal$Nr_Times_Moon = lookupWordBinary(SentimentReal, "Moon")
SentimentReal$Nr_Times_HODL = lookupWordBinary(SentimentReal, "HODL")
SentimentReal$Nr_Times_Pump = lookupWordBinary(SentimentReal, "Pump")
SentimentReal$Nr_Times_Dump = lookupWordBinary(SentimentReal, "Dump")
SentimentReal$Nr_Times_Bear = lookupWordBinary(SentimentReal, "Bear")
SentimentReal$Nr_Times_Bull = lookupWordBinary(SentimentReal, "Bull")
SentimentReal$Nr_Times_Buy = lookupWordBinary(SentimentReal, "Buy")
SentimentReal$Nr_Times_Sell = lookupWordBinary(SentimentReal, "Sell")
SentimentReal$Nr_Times_Whale = lookupWordBinary(SentimentReal, "Whale")
SentimentReal$Nr_Times_FOMO = lookupWordBinary(SentimentReal, "FOMO")
SentimentReal$Nr_Times_ATH = lookupWordBinary(SentimentReal, "ATH")
SentimentReal$Nr_Times_Short = lookupWordBinary(SentimentReal, "Short")
SentimentReal$Nr_Times_Long = lookupWordBinary(SentimentReal, "Long")
SentimentReal$Nr_Times_Defi = lookupWordBinary(SentimentReal, "Defi")
#SentimentReal$Nr_Times_Mooning = lookupWordBinary(SentimentReal, "Mooning")
SentimentReal$Nr_Times_Decentralization = lookupWordBinary(SentimentReal, "Decentralization")

############################ Train and Test split ##############################
################################################################################
library(RecordLinkage)
# Remove dubble tweets in order to avoid data leakage
count = 0
for (i in 1 : nrow(SentimentReal)){
  print(i)
  for (j in 1 : nrow(SentimentReal)){
    simularity = levenshteinSim(SentimentReal$text[i],SentimentReal$text[j])
    if(!is.na(simularity)){
      if (simularity >= 0.90){
        SentimentReal <- SentimentReal %>% slice(-c(i))
        count = count + 1
      }}
  }
}

SentimentReal$label <- as.factor(SentimentReal$Sentiment_label)
library(RecordLinkage)
set.seed(1000) 

ind <- sample(x = nrow(SentimentReal), size = nrow(SentimentReal), replace = FALSE)
train <- SentimentReal[1:floor(length(ind)*.60),]
val <- SentimentReal[(floor(length(ind)*.60)+1):floor(length(ind)*.80),]
test <- SentimentReal[(floor(length(ind)*.80)+1):(length(ind)),]

############################ DTM Matrix ########################################
################################################################################

corpus_train <- Corpus(VectorSource(train$text))
corpus_val <- Corpus(VectorSource(val$text))
corpus_test <- Corpus(VectorSource(test$text))

# N-Grams

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))

# Dtm

dtm_train <- DocumentTermMatrix(corpus_train, control = list(tokenize = Tokenizer,
                                                             weighting = function(x) weightTf(x),
                                                             RemoveNumbers=TRUE,
                                                             removePunctuation=TRUE,
                                                             stripWhitespace= TRUE))

dtm_val <- DocumentTermMatrix(corpus_val, control = list(tokenize = Tokenizer,
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

dtm_val <- prepareTest(dtm_train, dtm_val)
dtm_test <- prepareTest(dtm_train, dtm_test)


# Convert to common sparse remix

dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

sm_train <- dtm.to.sm(dtm_train)
sm_val <- dtm.to.sm(dtm_val)
sm_test <- dtm.to.sm(dtm_test)


######################## Singular Value Deposition #############################
################################################################################

p_load(irlba)

# Set the k to 20.
trainer <- irlba(t(sm_train), nu=40, nv=40)
str(trainer)

valer <- as.data.frame(as.matrix(sm_val) %*% trainer$u %*% solve(diag(trainer$d)))

tester <- as.data.frame(as.matrix(sm_test) %*% trainer$u %*% solve(diag(trainer$d)))
head(tester)

######################## Modelling and Evaluation ##############################
################################################################################

p_load(AUC, caret)
#trainsform labels into 

y_train <- as.factor(train$label)
y_val <- as.factor(val$label)


x = as.data.frame(trainer$v)
x = cbind(x, train[,103:123])

validation = cbind(valer, val[,103:123])

######################## Logistic Regression Binairy  ##################################
################################################################################

levels(train$label_binairy) =c("Postive" , "Negative")
LR <- glm(as.factor(train$label_binairy) ~., data = x,family = "binomial")
LR

preds <- predict(LR,validation,type = "response")
preds

AUC::auc(roc(preds,as.factor(val$label_binairy)))

library(pROC)
roc_qda <- roc(response = as.factor(val$label_binairy), predictor =preds)
plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
auc(roc_qda)

######################## Logistic Regression  ##################################
################################################################################

p_load(nnet)

multinom_model <- multinom(y_train ~., data = x)
multinom_model

# Building classification table

preds <- predict(multinom_model, newdata = validation, type = "probs")
preds

# AUC
# Starting validation code
library(pROC)
auc <- multiclass.roc(y_val,preds, levels = c(-2,-1,0, 1,2) )
auc


############################# Regularization ###################################
################################################################################


library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
x = as.data.frame(trainer$v)
y = y_train

cv.lasso <- cv.glmnet(x, y_train, alpha = 1, family = "multinomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "multinomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(diabetes ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)



############################# Random Forest ####################################
################################################################################

#train the model
library(caret)
library(MLeval)

#set up the cross-validation, which we will use to asses the performance
control <- trainControl(method='repeatedcv', number = 3, repeats = 1,
                        savePredictions = T, classProbs = T,,verbose =1)


data_rf = cbind(x,y_train)
levels(y_train) =c("VeryNegative", "Negative", "Neutral","Postive" , "VeryPostitive")
tunegrid <- expand.grid(.mtry = c(1:length(train)))
rf_model <- train(x = x, y = y_train, data = data_rf, method = "rf", 
                  trControl = control, preProcess = c("center","scale"),
                  ntree = 500, tuneGrid = tunegrid, metric = 'Accuracy')

# Fitting mtry = 96 on full training set
rf_model
varImp(rf_model)

#saveRDS(rf_model, "rf_spam_model.rds")
preds_rf <- predict(rf_model, test_data[, variables_to_use], type = "response")

x <- evalm(rf_model)
x$roc
x$stdres
table(preds_value, test_data$spam)


############################# XGboost ####################################
##########################################################################


p_load(xgboost)


# "binary:logistic"
levels(y_train) =c(0, 1, 2,3, 4)
dtrain <- xgb.DMatrix(data =as.matrix(train[,103:123]), label = as.matrix((y_train)))

bstSparse <- xgboost(data = dtrain, max.depth = 5, eta = 0.01, nthread = 4, nrounds = 1000, num_class = 5 ,subsample = 0.8,objective = "multi:softmax")

pred <- predict(bst, test$data)

# save model to binary local file
xgb.save(bst, "xgboost.model")

##### CV fold ####

numberOfClasses <- 5
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   metrics = list("auc"),
                   verbose = 2,
                   prediction = TRUE)

### View importance

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)

########################### Grid Search XGboost #######################################
#######################################################################################
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = c(0.5,0.2,0.1,0.01),
  max_depth = c(2,3,4),
  colsample_bytree = 1,
  min_child_weight = 1,
  gamma =1,
  subsample = c(1)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                       
  classProbs = TRUE,                                                           
  summaryFunction = multiClassSummary,
  allowParallel = TRUE,
)

# train the model for each parameter combination in the grid,
levels(y_train) =c("VeryNegative", "Negative", "Neutral","Postive" , "VeryPostitive")

library(future)
library(caret)
future::plan(multisession, workers = 4)

# train the model for each parameter combination in the grid and  evaluate using Cross Validation

xgb_train_2 = train(
  x = as.matrix(x),
  y = as.factor(y_train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_2$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")


cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   nrounds = 1000,
                   eta=0.01,
                   max_depth=5,
                   subsample = 0.8,
                   nfold = 5,
                   metrics = list("auc"),
                   verbose = 2,
                   prediction = TRUE)

# Change depth of trees (look for 5,6) / Change subsamples

# Change depth of trees (look for 5,6) / Change subsamples

# 0.702352
#Fitting nrounds = 100, max_depth = 5, eta = 0.1, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 1 on full training set

#71
#Fitting nrounds = 100, max_depth = 5, eta = 0.1, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8 on full training set

#71.18
#Fitting nrounds = 1000, max_depth = 5, eta = 0.01, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8 on full training set

######################@# NEURAL NETWORK (Code professor) ##############################
#######################################################################################



######################@# BERT  ##############################
#######################################################################################
#https://towardsdatascience.com/sentiment-analysis-in-10-minutes-with-bert-and-hugging-face-294e8a04b671
Sys.setenv(TF_KERAS=1) 
reticulate::py_config()

pretrained_path = '/Users/xavierverbrugge/Downloads/uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')

reticulate::py_module_available('keras_bert')

library(reticulate)
k_bert = import('keras_bert')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)


############################# NEURAL NETWORK ##########################################
#######################################################################################

# https://www.kaggle.com/code/taindow/deep-learning-with-r-sentiment-analysis/report

# ‘Deep Learning with R’ by Francois Chollet and J.J. Allaire

library(tidyverse) # importing, cleaning, visualising 
library(tidytext) # working with text
library(wordcloud) # visualising text
library(gridExtra) # extra plot options
library(grid) # extra plot options
library(keras) # deep learning with keras


# Combine 

train = train %>% mutate(Split = "train")
test = test %>% mutate(Split = "test")

full = data.frame(rbind(train %>% select(-Sentiment), test))

# Tokenizer -------------------------------------------------------------------

# Setup some parameters

max_words = 15000 # Maximum number of words to consider as features
maxlen = 32 # Text cutoff after n words


# Prepare to tokenize the text

texts = full$Phrase

tokenizer = text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(texts)

# Tokenize - i.e. convert text into a sequence of integers

sequences = texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index

# Pad out texts so everything is the same length

data = pad_sequences(sequences, maxlen = maxlen)


# Split back into train and test

train_matrix = data[1:nrow(train),]
test_matrix = data[(nrow(train)+1):nrow(data),]


# Prepare training labels (need to be binary matrices)

labels = train$Sentiment
labels = labels %>%  data.frame() %>%
  mutate(
    V0 = ifelse(labels == 0, 1, 0),
    V1 = ifelse(labels == 1, 1, 0),
    V2 = ifelse(labels == 2, 1, 0),
    V3 = ifelse(labels == 3, 1, 0),
    V4 = ifelse(labels == 4, 1, 0)
  ) %>% 
  select(
    V0,V1,V2,V3,V4
  ) %>% as.matrix()


# Prepare a validation set

training_samples = nrow(train_matrix)*0.80
validation_samples = nrow(train_matrix)*0.20

indices = sample(1:nrow(train_matrix))
training_indices = indices[1:training_samples]
validation_indices = indices[(training_samples + 1): (training_samples + validation_samples)]

x_train = train_matrix[training_indices,]
y_train = labels[training_indices,]

x_val = train_matrix[validation_indices,]
y_val = labels[validation_indices,]


# Embeddings 

# Dimensions

glove_wiki_embedding_dim = 300
glove_twitter_embedding_dim = 200
glove_crawl_embedding_dim = 300
fast_wiki_embedding_dim = 300
fast_crawl_embedding_dim = 300
word2vec_news_embedding_dim = 300

# Files (uploaded from local pc)

glove_wiki_weights = readRDS("../input/embedding-weights/glove_wiki_300d_32.rds")
glove_twitter_weights = readRDS("../input/embedding-weights/glove_twitter_200d_32.rds")
glove_crawl_weights = readRDS("../input/embedding-weights/glove_crawl_300d_32.rds")
fast_wiki_weights = readRDS("../input/embedding-weights/fasttext_wiki_300d_32.rds")
fast_crawl_weights = readRDS("../input/embedding-weights/fasttext_crawl_300d_32.rds")
word2vec_news_weights = readRDS("../input/embedding-weights/word2vec_news_300d_32.rds")

# Model Architecture -------------------------------------------------------------------

# Setup input

input = layer_input(
  shape = list(NULL),
  dtype = "int32",
  name = "input"
)

# Embedding layers

encoded_1 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = glove_wiki_embedding_dim, name = "embedding_1") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE) 

encoded_2 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = glove_twitter_embedding_dim, name = "embedding_2") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE) 

encoded_3 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = glove_crawl_embedding_dim, name = "embedding_3") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE)

encoded_4 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = fast_wiki_embedding_dim, name = "embedding_4") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE) 

encoded_5 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = fast_crawl_embedding_dim, name = "embedding_5") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE) 

encoded_6 = input %>% 
  layer_embedding(input_dim = max_words, output_dim = word2vec_news_embedding_dim, name = "embedding_6") %>% 
  layer_lstm(units = maxlen,
             dropout = 0.2,
             recurrent_dropout = 0.5,
             return_sequences = FALSE) 

# Concatenate

concatenated = layer_concatenate(list(encoded_1,encoded_2,encoded_3,encoded_4,encoded_5,encoded_6))


# Dense layers

dense = concatenated %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 5, activation = "softmax")


# Bring model together

model = keras_model(input, dense)

# Freeze the embedding weights initially to prevent updates propgating back through and ruining our embedding

get_layer(model, name = "embedding_1") %>% 
  set_weights(list(glove_wiki_weights)) %>% 
  freeze_weights()

get_layer(model, name = "embedding_2") %>% 
  set_weights(list(glove_twitter_weights)) %>% 
  freeze_weights()

get_layer(model, name = "embedding_3") %>% 
  set_weights(list(glove_crawl_weights)) %>% 
  freeze_weights()

get_layer(model, name = "embedding_4") %>% 
  set_weights(list(fast_wiki_weights)) %>% 
  freeze_weights()

get_layer(model, name = "embedding_5") %>% 
  set_weights(list(fast_crawl_weights)) %>% 
  freeze_weights()

get_layer(model, name = "embedding_6") %>% 
  set_weights(list(word2vec_news_weights)) %>% 
  freeze_weights()


# Compile

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = "categorical_crossentropy",
  metrics = "categorical_accuracy"
)

#Model Training #1

# Early stopping condition

callbacks_list = list(
  callback_early_stopping(
    monitor = 'val_loss', 
    patience = 10
  ))

# Train model 

history = model %>% fit(
  x_train,
  y_train,
  batch_size = 2048,
  validation_data = list(x_val, y_val),
  epochs = 500,
  view_metrics = FALSE,
  verbose = 0,
  callbacks =  callbacks_list
)

# Look at training results

print(history)



### DOC2VEC 
# https://www.r-bloggers.com/2017/02/twitter-sentiment-analysis-with-machine-learning-in-r-using-doc2vec-approach/

# loading packages
library(twitteR)
library(ROAuth)
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

##### loading classified tweets ######
# source: http://help.sentiment140.com/for-students/
# 0 - the polarity of the tweet (0 = negative, 4 = positive)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. If there is no query, then this value is NO_QUERY.
# 4 - the user that tweeted
# 5 - the text of the tweet

tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv',
                              col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))

# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

######### doc2vec ########


# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
glmnet:::auc(as.numeric(tweets_test$sentiment), preds)

# save the model for future using
saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
#######################################################


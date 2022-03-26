################################################################################
############################# SENTIMENT ML   ###################################
################################################################################

############################ Data Understanding ################################
################################################################################

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix)

SentimentReal <- read_csv("SentimentReal.csv")
Encoding(SentimentReal$message) <- 'latin'
SentimentReal %>% glimpse()

############################ Train and Test split ##############################
################################################################################

# Already done by the boyz

############################ DTM Matrix ########################################
################################################################################

corpus_train <- Corpus(VectorSource(train$message))
corpus_test <- Corpus(VectorSource(test$message))

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
dtm_test

# Convert to common sparse remix

dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

sm_train <- dtm.to.sm(dtm_train)
sm_test <- dtm.to.sm(dtm_test)


######################## Singular Value Deposition #############################
################################################################################

p_load(irlba)

# Set the k to 20.
trainer <- irlba(t(sm_train), nu=20, nv=20)
str(trainer)

tester <- as.data.frame(as.matrix(sm_test) %*% trainer$u %*% solve(diag(trainer$d)))
head(tester)

######################## Modelling and Evaluation ##############################
################################################################################

p_load(AUC, caret)
#trainsform labels into 

y_train <- as.factor(train$label)
y_test <- as.factor(test$label)


######################## Logistic Regression  ##################################
################################################################################

p_load(nnnet)


multinom_model <- multinom(y_train ~., data = as.data.frame(trainer$v))
multinom_model

# Building classification table

preds <- predict(multinom_model, newdata = test, "class")


# AUC

AUC::auc(roc(preds,y_test))
plot(roc(preds,y_test))

#Confusion Matrix 
preds_lab <- ifelse(preds > 0.5,1,0)
xtab <- table(preds_lab, y_test)
confusionMatrix(xtab)


############################# Regularization ###################################
################################################################################


library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
x = trainer$V
y = train$label

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "multinomial")
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
control <- trainControl(method='repeatedcv', number = 10, repeats = 3,
                        savePredictions = T, classProbs = T)

variables_to_use <- colnames(train_data)[!(colnames(train_data) %in% c("spam", "screen_name"))]
tunegrid <- expand.grid(.mtry = c(1:length(variables_to_use)))
rf_model <- train(x = train_data[, variables_to_use], y = train_data$spam, data = train_data, method = "rf", 
                  trControl = control, preProcess = c("center","scale"),
                  ntree = 500, tuneGrid = tunegrid, metric = 'Accuracy')

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
bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "multi:softmax")

pred <- predict(bst, test$data)

# save model to binary local file
xgb.save(bst, "xgboost.model")


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


### Predictive modeling 
setwd("/Users/thomassuys/Downloads/")

#import statements 
library(keras)
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(text2vec,Rtsne,scales,ggrepel,tidyverse,tm)


#load in data
tweets_data <- read_csv("/Users/thomassuys/Downloads/Tweets_And_Labels_2.csv")
text <- tweets_data %>% pull(text)
labels <- tweets_data %>% pull(Sentiment_label)

### WORD2VEC


### GLOVE

## TRY PRE-TRAINED EMBEDDING
maxlen <- 100                 # We will cut reviews after 100 words
training_samples <- 4003       # We will be training on 200 samples
test_samples <- 501
validation_samples <- 501   # We will be validating on 10000 samples
max_words <- 501            # We will only consider the top 10,000 words in the dataset

text <- text %>% tolower() %>% removePunctuation() %>% removeWords(words = stopwords()) %>% stripWhitespace()

tokenizer <- text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(text)

sequences <- texts_to_sequences(tokenizer, text)

word_index <- tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

data <- pad_sequences(sequences, maxlen = maxlen)
#labels <- as.array(labels)

cat("Shape of data tensor:", dim(data), "\n")
cat('Shape of label tensor:', dim(labels), "\n")



labels <- as.factor(labels)
labels[is.na(labels)] <- 0
labels <- to_categorical(labels, 5)
#train-test split
indices <- sample(1:nrow(data))

training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):(training_samples + validation_samples)]
train_val_indices <- indices[1:(training_samples + validation_samples)]
test_indices <- indices[(training_samples + validation_samples + 1): nrow(data)]

X_train <- data[training_indices,]
Y_train <- labels[training_indices,]
X_val <- data[validation_indices,]
Y_val <- labels[validation_indices,]
X_test <- data[test_indices,]
Y_test <- labels[test_indices,]
X_train_all <- data[train_val_indices,]
Y_train_all <- labels[train_val_indices,]

#pre process embeddings
glove_dir = "/Users/thomassuys/Downloads/6471382cdd837544bf3ac72497a38715e845897d265b2b424b4761832009c837"
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))

embeddings_index <- new.env(hash = TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}

cat("Found", length(embeddings_index), "word vectors.\n")

embedding_dim <- 100

embedding_matrix <- array(0, c(max_words, embedding_dim))

for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < max_words) {
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector))
      # Words not found in the embedding index will be all zeros.
      embedding_matrix[index+1,] <- embedding_vector
  }
}

#neural network
model <- keras_model_sequential() %>% 
  # We specify the maximum input length to our Embedding layer
  # so we can later flatten the embedded inputs
  layer_embedding(input_dim = max_words, output_dim = embedding_dim, 
                  input_length = maxlen) %>% 
  # We flatten the 3D tensor of embeddings 
  # into a 2D tensor of shape `(samples, maxlen * 8)`
  layer_flatten() %>% 
  # We add the classifier on top
  layer_dense(units = 128, activation = 'relu', kernel_initializer = 'he_uniform', kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dense(units = 64, activation = 'relu', kernel_initializer = 'he_uniform', kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dropout(rate = 0.6) %>%
  layer_dense(units = 32, activation = "relu", kernel_initializer = 'he_uniform', kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 5, activation = "softmax") 

#load embeddings in the model
get_layer(model, index = 1) %>% 
  set_weights(list(embedding_matrix)) %>% 
  freeze_weights()

#compile and run
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% keras::fit(
  X_train, Y_train,
  epochs = 100,
  batch_size = 128,
  validation_data = list(X_val, Y_val)
)

plot(history)


### TRY TASK-SPECIFIC EMBEDDING
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, output_dim = embedding_dim, 
                  input_length = maxlen) %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu", kernel_initializer = 'he_uniform') %>% 
  layer_dense(units = 32, activation = 'relu', kernel_initializer = 'he_uniform') %>%
  layer_dense(units = 5, activation = "softmax")

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "categorical_crossentropy",
  metrics = c("acc")
)

history <- model %>% keras::fit(
  X_train, Y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(X_val, Y_val)
)

plot(history)

### GRU
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, output_dim = embedding_dim, input_length = maxlen) %>% 
  layer_gru(units = 80) %>% 
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 5, activation = "softmax")

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% keras::fit(
  X_train, Y_train,
  epochs = 20,
  batch_size = 128,
  validation_data = list(X_val, Y_val)
)

plot(history)


### LSTM 
# BEST MODEL!!! OBTAINS ACCURACY OF 0.7075
max_features <- 1005
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 100) %>%
  layer_dropout(0.5) %>%
  bidirectional(
    layer_lstm(units = 32, return_sequences = T, recurrent_dropout = 0.35, kernel_initializer = 'he_uniform')
  ) %>% 
  layer_dropout(0.4) %>%
  bidirectional(
    layer_lstm(units = 64, recurrent_dropout = 0.35, kernel_initializer = 'he_uniform')
  ) %>% 
  layer_dropout(0.3) %>%
  layer_dense(units = 5, activation = "softmax")

#load embeddings in the model
get_layer(model, index = 1) %>% 
  set_weights(list(embedding_matrix)) %>% 
  freeze_weights()

model %>% compile(
  optimizer = optimizer_rmsprop(0.0005),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% keras::fit(
  X_train, Y_train,
  epochs = 40,
  batch_size = 128, 
  validation_data = list(X_val, Y_val)
)

plot(history)
### NORMAL LSTM
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 100) %>%
  layer_lstm(units = 32) %>%
  layer_dropout(0.3) %>%
  layer_lstm(units = 64) %>% 
  layer_dropout(0.45) %>%
  layer_lstm(units = 128) %>% 
  layer_dropout(0.6)
layer_dense(units = 5, activation = 'softmax')

model %>% compile(
  optimizer = optimizer_adam(learning_rate = .0001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% keras::fit(
  X_train, Y_train,
  epochs = 20,
  batch_size = 128,
  validation_data = list(X_val, Y_val)
)

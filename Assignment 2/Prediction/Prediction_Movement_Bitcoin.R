################################################################################
############################# PREDICTION MOVEMENT  ############################
################################################################################

# We have 17 days of data

History <- read_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_Price_History.csv")

Bitcoin <- read_twitter_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_Without_Spam_And_Labels.csv")

# Aggregate sentiment over the whole day

#dates <- lapply(Bitcoin$created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
#dates <- dates %>% reduce(c)
#Bitcoin$created_at <- dates

#Aggregrate Weighted
#Remove NA's
Bitcoin$Sentiment_Label_Pred = as.numeric(Bitcoin$Sentiment_Label_Pred)
Bitcoin$Sentiment_Label_Pred = Bitcoin$Sentiment_Label_Pred[is.na(Bitcoin$Sentiment_Label_Pred)] <- 0
Bitcoin$followers_count = Bitcoin$followers_count[is.na(Bitcoin$followers_count)] <- 0

getQuantile <- function(df_variable){
  output <- numeric(length(df_variable))
  q <- quantile(df_variable, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), na.rm = T) %>% reduce(c)
  for(i in 1:length(df_variable)){
    if(df_variable[i] < q[1]){
      output[i] = 1
    }
    else if(df_variable[i] >= q[1] & df_variable[i] < q[2]){
      output[i] = 2
    }
    else if(df_variable[i] >= q[2] & df_variable[i] < q[3]){
      output[i] = 3
    }
    else if(df_variable[i] >= q[3] & df_variable[i] < q[4]){
      output[i] = 4
    }
    else if(df_variable[i] >= q[4] & df_variable[i] < q[5]){
      output[i] = 5
    }
    else if(df_variable[i] >= q[5] & df_variable[i] < q[6]){
      output[i] = 6
    }
    else if(df_variable[i] >= q[6] & df_variable[i] < q[7]){
      output[i] = 7
    }
    else if(df_variable[i] >= q[7] & df_variable[i] < q[8]){
      output[i] = 8
    }
    else if(df_variable[i] >= q[8] & df_variable[i] < q[9]){
      output[i] = 9
    }
    else if(df_variable[i] >= q[9]){
      output[i] = 10
    }
  }
  return(output)
}


Bitcoin$followers_count_quantile <- getQuantile(as.numeric(Bitcoin$followers_count))

Bitcoin_followers_w <- Bitcoin %>% group_by(created_at, followers_count_quantile) %>% summarise(sentiment = mean(Bitcoin$Sentiment_Label_Pred))

Bitcoin_followers_w$weight_score <- Bitcoin_followers_w$followers_count_quantile*Bitcoin_followers_w$sentiment

Bitcoin_followers_w$weight_score <- Bitcoin_followers_w$weight_score/sum(seq(1:10))

score_daily_followers_w <- Bitcoin_followers_w %>% group_by(created_at) %>%
  summarise(sentiment_score = sum(weight_score))
score_daily_followers_w


#Filter irrelevant dates
Aggregated_Sentiment = df[9:19,]$x

History$Aggregated_Sentiment = Aggregated_Sentiment

############## Subsetting ###################
# We have 17 days of data, we will use 10 days as training data, 3 days as valdiation and 4 days as test data.
train <- History[1:10]
val <- History[10:13]
test <- History[13:17]

############### Scaling the features #########

ToBeScaled = History[c("Volume","Aggregated_Sentiment","")]
History_Scaled = scale(x, center = TRUE, scale = TRUE)



model <- keras_model_sequential()

model %>%
  layer_embedding(input_dim = History.shape[0]) %>%
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))

history <- model %>% fit(train_x, train_y,
                         epochs = 25,
                         batch_size = 128,
                         validation_split = 0.2)
plot(history)


# Build the LSTM Model #
model <- keras_model_sequential()
model %>%
  layer_lstm(units            = 24, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 24, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)
model %>% 
  compile(loss = 'mae', optimizer = 'adam')
model
for (i in 1:epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}
# Make Predictions #
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1]
# Retransform values #
pred_tbl <- tibble(
  Date   = lag_test_tbl$Date,
  High   = (pred_out * scale_history + center_history)^2
)
# Combine actual data with predictions #
tbl_1 <- df_trn %>%
  add_column(key = "actual")
tbl_2 <- df_tst %>%
  add_column(key = "actual")
tbl_3 <- pred_tbl %>%
  add_column(key = "predict")
# Create time_bind_rows() to solve dplyr issue #
time_bind_rows <- function(data_1, data_2, Date) {
  index_expr <- enquo(Date)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}
ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, Date = Date) %>%
  arrange(key, Date) %>%
  mutate(key = as_factor(key))
ret
# Determining Model Performance #
MSE<-mean((tbl_2$High - tbl_3$High)^2)
RMSE<-sqrt(MSE)
# Setup single plot function #
plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- RMSE
  
  g <- data %>%
    ggplot(aes(Date, High, color = key)) +
    geom_point(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )
  
  return(g)
}
ret %>% 
  plot_prediction(id = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")

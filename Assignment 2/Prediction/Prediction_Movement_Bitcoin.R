################################################################################
############################# SENTIMENT ML   ###################################
################################################################################

# We have 17 days of data

History <- read_csv("Bitcoin_Price_History.csv")
Sentiment<- read_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Tweets_And_Labels_2.csv")

# Aggregate sentiment over the whole day

dates <- lapply(Sentiment$created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)
Sentiment$created_at <- dates

# Aggregrate
aggregate(Sentiment$label, by=list(Sentiment$created_at), sum)

# Add to dataframe
Data <- cbind(History,Sentiment$Sentiment_label)

############## Subsetting ###########
# We have 17 days of data, we will use 10 days as training data, 3 days as valdiation and 4 days as test data.
train <- History[1:10]
val <- History[10:13]
test <- History[13:17]

############ 

model <- keras_model_sequential()


model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))

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

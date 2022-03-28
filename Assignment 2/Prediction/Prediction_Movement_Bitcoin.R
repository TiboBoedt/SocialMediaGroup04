################################################################################
############################# PREDICTION MOVEMENT  ############################
################################################################################

# We have 17 days of data

History <- read_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_Price_History_2.csv")

Bitcoin <- read_twitter_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_Without_Spam_And_Labels_2.csv")

# Aggregate sentiment over the whole day

Bitcoin$Sentiment_Label_Pred = as.numeric(Bitcoin$Sentiment_Label_Pred)

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

# Aggregate sentiment based on number of followers a creator has

#Create new df
Bitcoin_followers_w <- data.frame(Bitcoin$Sentiment_Label_Pred, Bitcoin$created_at)
names(Bitcoin_followers_w) =c("Sentiment","created_at")

Bitcoin_followers_w$followers_count_quantile <- getQuantile(as.numeric(Bitcoin$followers_count))
#Weigh the sentiment based on quantile value of creator number of followers
library(dict)
library(hash)
h <- hash() 
Weights = c(0.01,0.02,0.04, 0.05,0.08,0.1,0.12,0.15,0.18,0.25)
for(i in range(1:10)){
  h[[as.String(i)]] <- Weights[i]
}

list = list()
for(i in 1:nrow(Bitcoin_followers_w)){
  list = c(list,list(Bitcoin_followers_w$Sentiment[i]*h[[as.String(Bitcoin_followers_w$followers_count_quantile[i])]]))
}
list(Bitcoin_followers_w$Sentiment[1000]*h[[as.String(Bitcoin_followers_w$followers_count_quantile[100])]])

Bitcoin_followers_w$Sentiment[107980]
Bitcoin_followers_w$weight_score <- list

#Bitcoin_followers_w$weight_score <- Bitcoin_followers_w$followers_count_quantile*Bitcoin_followers_w$Sentiment
#Normalize the sentiment
#Bitcoin_followers_w$weight_score <- Bitcoin_followers_w$weight_score/sum(seq(1:10))

score_daily_followers_w <- Bitcoin_followers_w %>% group_by(created_at)%>%
  summarise(sentiment_score = sum(weight_score))
score_daily_followers_w 

#Add Aggregated sentiment to data

History$Aggregated_Sentiment = score_daily_followers_w$sentiment_score

############## Subsetting ###################
# We have 18 days of data, we will use 10 days as training data, 3 days as valdiation and 4 days as test data.
History = History[,2:20]
train <- History[1:11,]
val <- History[11:13,]
test <- History[14:17,]

#Select useful variables
train_x = subset(train ,select = c("volume","Close_Previous_Day","Aggregated_Sentiment"))
val_x = subset(val ,select = c("volume","Close_Previous_Day","Aggregated_Sentiment"))
test_x = subset(test ,select = c("volume","Close_Previous_Day","Aggregated_Sentiment"))

############### Scaling the features ############

# Can't use close of current day.

Train_mask <- subset(train ,select = c("volume","Close_Previous_Day","Aggregated_Sentiment"))
Train_means <- data.frame(as.list(Train_mask %>% apply(2, mean)))
Train_stddevs <- data.frame(as.list(Train_mask %>% apply(2, sd)))

col_names <- names(Train_mask)
for (i in 1:ncol(Train_mask)){
  train_x[,col_names[i]] <- (train[,col_names[i]] - Train_means[,col_names[i]])/Train_stddevs[,col_names[i]]
  val_x[,col_names[i]] <-  (val[,col_names[i]] - Train_means[,col_names[i]])/Train_stddevs[,col_names[i]]
  test_x[,col_names[i]] <-  (test[,col_names[i]] - Train_means[,col_names[i]])/Train_stddevs[,col_names[i]]
}

train_y <- train$Up_Down
val_y <- val$Up_Down
test_y <- test$Up_Down

############ XGboost #############

p_load(xgboost)

levels(train_y) =c(0, 1)
#Create DMatrix
dtrain <- xgb.DMatrix(data =as.matrix(train_x), label = as.matrix((train_y)))
#Model
bstSparse <- xgboost(data = dtrain, max.depth = 6, eta = 0.1, nthread = 2, nrounds = 1000 ,subsample = 0.8,objective = "binary:logistic")

xgb_params <- list("objective" = "binary:logistic")

pred <- predict(bstSparse, as.matrix(val_x), type="class")
preds = ifelse(pred>0.5 , 0 ,1)

table(preds,val_y)

cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   nrounds = 10,
                   eta=0.1,
                   max_depth=2,
                   subsample = 0.8,
                   nfold = 10,
                   metrics = list("auc"),
                   verbose = 2,
                   prediction = TRUE)


############ XGboost on test set #############

train_x = rbind(train_x, val_x)
train_y = c(train_y,val_y)
levels(train_y) =c(0, 1)

dtrain <- xgb.DMatrix(data =as.matrix(train_x), label = as.matrix((train_y)))

bstSparse <- xgboost(data = dtrain, max.depth = 6, eta = 0.1, nthread = 4, nrounds = 1000 ,subsample = 0.8,objective = "binary:logistic")

pred <- predict(bstSparse, as.matrix(test_x), type="Response")
preds = ifelse(pred>0.5 , 0,1)

table(preds,test_y)


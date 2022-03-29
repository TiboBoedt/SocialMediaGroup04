################################################################################
############################# SENTIMENT ML   ###################################
################################################################################

############################ Importing Data  ###################################
################################################################################

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix)
setwd(dir ="/Users/xavierverbrugge/SocialMediaGroup04_2")
Bing_Dict <- read_csv("./Assignment 2/bing_updated")

SentimentReal <- read_twitter_csv("Tweets_And_Labels_2.csv")
Encoding(SentimentReal$text) <- 'latin'
SentimentReal %>% glimpse()


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

#Only used for experimentation
SentimentReal$label_binairy <- ifelse(SentimentReal$Sentiment_label>0, 1, 0)
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

############################ Creating Variables  ###############################
################################################################################

p_load(AUC, caret)


y_train <- as.factor(train$label)
y_val <- as.factor(val$label)

#Aggregate the SVD's and created variables
x = as.data.frame(trainer$v)
x = cbind(x, train[,103:123])

validation = cbind(valer, val[,103:123])

######################## Modelling and Evaluation ##############################
################################################################################

######################## Logistic Regression Binairy  ##########################
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

## 0.7004787 AUC

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

# AUC multiclass 0.674

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

z <- evalm(rf_model)
z$roc
z$stdres
table(preds_value, test_data$spam)


############################# XGboost ####################################
##########################################################################


p_load(xgboost)

levels(y_train) =c(0, 1, 2,3, 4)
dtrain <- xgb.DMatrix(data =as.matrix(x), label = as.matrix((y_train)))

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

numberOfClasses <- 5
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   nrounds = 500,
                   eta=0.01,
                   max_depth=6,
                   subsample = 0.8,
                   nfold = 5,
                   metrics = list("auc"),
                   verbose = 2,
                   prediction = TRUE)


#72.23
#Fitting nrounds = 500, max_depth = 6, eta = 0.01, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8 on full training set


bstSparse <- xgboost(data = dtrain, max.depth = 6, eta = 0.01, nthread = 4, nrounds = 500, num_class = 5 ,subsample = 0.8,objective = "multi:softmax")

### View importance

importance_matrix <- xgb.importance(model = bstSparse)
print(importance_matrix)
xgb.ggplot.importance(importance_matrix)

library("SHAPforxgboost")
shap_values <- shap.values(xgb_model = bstSparse, X_train = as.matrix(x))

shap_long <- shap.prep(xgb_model = bstSparse, X_train = as.matrix(x))
# save model to binary local file
xgb.save(bst, "xgboost.model")

########################### LightGBM #######################################
#######################################################################################
pacman::p_load(pscl, ggplot2, ROCR, lightgbm, methods, Matrix, caret)
levels(y_train) =c(0, 1, 2,3, 4)
train_matrix = lgb.Dataset(data = as.matrix(x), label = as.matrix((y_train)))

params = list(max_bin = 70,learning_rate = 0.001,
              objective = "multiclass" ,max_depth =11, num_class = 5)
set.seed(10)
lgb.model.cv = lgb.cv(params = params, data = train_matrix, nrounds = 1500,early_stopping_rounds = 100, 
                      eval_freq = 20, eval = "auc_mu",nfold = 10, stratified = TRUE)


######################@# BERT  ##############################
#######################################################################################

# --------> See python file

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



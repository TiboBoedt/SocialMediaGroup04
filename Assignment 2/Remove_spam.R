#This file can be used to clean the tweets using the randomForest model trained in 
#the Bitcoin_SPAM.R file

#Load the model
setwd(dir ="/Users/xavierverbrugge/SocialMediaGroup04_2")
model <- readRDS("./Assignment 2/rf_spam_model.rds") #file is in the assingment2 folder

#Load the file
tweet_df <- read.csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin2.csv")
tweet_df <- tweet_df %>% distinct(text, .keep_all = TRUE)

#add the necessary variables

#nr_hashtags
tweet_df$nr_hashtags <- sapply(tweet_df$hashtags, function(x) if(!is.na(x)) length(str_split(x, " ")[[1]]) else 0)

#age_account_days
dates <- lapply(tweet_df$created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)
tweet_df$created_at <- dates

dates <- lapply(tweet_df$account_created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)
tweet_df$account_created_at <- dates

tweet_df$age_account_days <- as.numeric(tweet_df$created_at - tweet_df$account_created_at)

#Reputation
tweet_df$Reputation <- (tweet_df$friends_count)/((tweet_df$friends_count + tweet_df$followers_count))

#ld
#lexical density
getLD <- function(data){
  data_stopwords <- data %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
  data_no_stopwords <- str_split(data_stopwords, " ")[[1]][which(!str_split(data_stopwords, " ")[[1]] %in% stop_words$word)]
  denominator <- length(data_no_stopwords)
  nominator <- length(str_split(data_stopwords, " ")[[1]])
  ld <- denominator/nominator
  return(ld)
}

tweet_df$ld <- lapply(tweet_df$text, FUN = getLD) %>% reduce(c)

#ttr
getTTR <- function(data){
  data <- data %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
  nr_tokens <- length(str_split(data, " ")[[1]])
  unique_tokens <- length(unique(str_split(data, " ")[[1]]))
  ttr <- unique_tokens/nr_tokens
  return(ttr)
}

ttr <- lapply(tweet_df$text, FUN = getTTR)
ttr <- ttr %>% reduce(c)
tweet_df$ttr <- ttr

#MentionsRatio
getCharMentionsOverText <- function(data){
  output <- numeric(length = length(data$mentions_screen_name))
  for(i in 1:length(data$mentions_screen_name)){
    rel <- nchar(data$mentions_screen_name[i])/data$display_text_width[i]
    output[i] <- rel
  }
  output[is.na(output)] <- 0 #na means that there where no mentions thus a zero in the denominator
  return(output)
}
tweet_df$MentionsRatio <- getCharMentionsOverText(tweet_df)

#digitsInName
getDigitsInName <- function(data){
  output <- numeric(length(data))
  for(i in 1:length(data)){
    nr <- str_count(data[i], "\\d")
    output[i] = nr
  }
  return(output)
}

tweet_df$digitsInName <- getDigitsInName(tweet_df$screen_name)

#Signal Words
signal_words <- c("mining", "follow", "free", "join", "dm", "for free", "add", "whatsapp")

getSignalWordIndicator <- function(data){
  data <- data %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
  v <- str_split(data, " ")[[1]]
  for(word in v){
    if(word %in% signal_words){
      return(TRUE)
    }
  }
  return(FALSE)
}

tweet_df$signal_words <- sapply(tweet_df$text, FUN = getSignalWordIndicator) %>% reduce(c)
tweet_df$signal_words <- factor(tweet_df$signal_words)

#Activity
tweet_df$Activity <- tweet_df$statuses_count/tweet_df$age_account_days
tweet_df$age_account_days[which(tweet_df$Activity == Inf)]
#Inf means the nominator is a zero -> new account -> all activity is from one day, so let's set equal to statuses_count
tweet_df$Activity[which(tweet_df$Activity == Inf)] <- tweet_df$statuses_count[which(tweet_df$Activity == Inf)]

tweet_df <- tweet_df %>% drop_na(Reputation)#same problem as test data we remove them as we assume them to be spam anyway

#Prepare the dataset
variables_to_use <- c("display_text_width", "nr_hashtags", "age_account_days", 
                      "Reputation", "ld", "Activity", "followers_count", 
                      "friends_count", "ttr", "MentionsRatio", "digitsInName",
                      "signal_words", "retweet_count")
df_label <- predict(model, newdata = tweet_df[, variables_to_use])
df_label <- factor(df_label)
levels(df_label) <- c("quality", "spam")

tweet_df$label <- df_label

correct_tweet_df <- tweet_df %>% filter(tweet_df$label == "quality")
Bitcoin_Without_Spam = correct_tweet_df

# Delete unnecessairy variables
drops <- c("display_text_width", "nr_hashtags", "age_account_days", 
                      "Reputation", "ld", "Activity",
                      "friends_count", "ttr", "MentionsRatio", "digitsInName",
                      "signal_words")

DF = Bitcoin_Without_Spam[ , !(names(Bitcoin_Without_Spam) %in% drops)]

write_csv(DF, file = "/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Bitcoin_Without_Spam.csv")



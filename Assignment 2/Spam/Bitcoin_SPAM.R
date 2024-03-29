################################################################################
############################# BITCOIN SENTIMENT ################################
################################################################################

############################ Data Exploration ##################################
################################################################################
### LOAD THE BITCOIN FILE
bitcoin <- read_twitter_csv("Bitcoin.csv")
bitcoin <- bitcoin %>% distinct(text, .keep_all = TRUE)

### BASIC EXPLORATION

#How many different dates are available in the dataset
dates <- lapply(bitcoin$created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)

bitcoin$created_at <- dates

(unique_dates <- unique(bitcoin$created_at))

#analyse the different creators of the tweets
(nr_unique_creators <- length(unique(bitcoin$screen_name)))
paste(round((nr_unique_creators/length(bitcoin$screen_name))*100, 2), "%", sep = "")

tweets_per_creator <- count(group_by(bitcoin, screen_name))
colnames(tweets_per_creator) <- c("screen_name", "nr_tweets")
tweets_per_creator <- tweets_per_creator[order(-tweets_per_creator$nr_tweets),]

summary(tweets_per_creator$nr_tweets)
quantile(tweets_per_creator$nr_tweets, c(.8, .85, .9, .95, 0.975, 0.99))
#eventough some users seem to generate a lot of tweets (max = 364), we notice that
#75%, 80% and 85% of the creators has between 1 and 2 tweets, 90% between 1 and 3
#, 95% between 1 and 6, 97.5% between 1 and 11 and 99% between 1 and 23.
#We could opted to remove creators with to many tweets as this could be mere spam?

#let's do the same, but on a daily base. 
nr_unique_creators_daily <- numeric(length(unique_dates))
rel_unique_creators_daily <- character(length(unique_dates))
i = 1
for(d in as.POSIXct(unique_dates)){
  df <- bitcoin %>% filter(created_at == as_date(as_datetime(d)))
  unique_creators <- length(unique(df$screen_name))
  nr_unique_creators_daily[i] = unique_creators
  rel_unique_creators_daily[i] = paste(round(unique_creators/length(df$screen_name)*100, 2), "%", sep = "")
  i = i + 1
}
nr_unique_creators_daily
(nr_unique_creators_daily_df <- data.frame(created_at = unique_dates, nr_tweets = nr_unique_creators_daily, 
                                           rel_nr_tweets = rel_unique_creators_daily))

tweets_per_creator_daily <- bitcoin %>% group_by(created_at, screen_name) %>%
  count(screen_name)
colnames(tweets_per_creator_daily) <- c("created_at", "screen_name", "nr_tweets")

tweets_per_creator_daily <- tweets_per_creator_daily[order(-tweets_per_creator_daily$nr_tweets),]

#first summarize the nr_tweets from the entire dataframe to get a general idea of the 
#average tweets per day for the entire database, afterwards we can check these 
#numbers on a daily base
summary(tweets_per_creator_daily$nr_tweets)

#let's generate the number of tweets per day 
daily_min <- numeric(length(unique_dates))
daily_1 <- numeric(length(unique_dates))
daily_median <- numeric(length(unique_dates))
daily_mean <- numeric(length(unique_dates))
daily_3 <- numeric(length(unique_dates))
daily_max <- numeric(length(unique_dates))
i = 1
for(d in as.POSIXct(unique_dates)){
  df <- tweets_per_creator_daily %>% filter(created_at == as_date(as_datetime(d)))
  s <- summary(df$nr_tweets)
  daily_min[i] = s[1]
  daily_1[i] = s[2]
  daily_median[i] = s[3]
  daily_mean[i] = s[4]
  daily_3[i] = s[5]
  daily_max[i] = s[6]
  
  i = i + 1
}

(daily_tweets_per_creator_summary <- data.frame(Date = unique_dates, Min = daily_min, Q1 = daily_1,
                                                Median = daily_median, Mean = daily_mean,
                                                Q3 = daily_3, Max = daily_max))
#aside from the max (which we expect to be volitile, no big difference are present)

ggplot(daily_tweets_per_creator_summary, aes(x = Date, y = Mean))+
  geom_line(col = "purple")+
  geom_hline(yintercept = summary(tweets_per_creator_daily$nr_tweets)[4])+
  ylim(0, 2.5)+
  xlab("Date")+
  ylab("Average tweets per creator")+
  ggtitle("Average tweets per creator per day")+
  theme(legend.position = "bottom")

#Next we analyse the total amount of hashtags per tweet. A possible assumption could
#be that tweets containing a lot of hashtags are spam tweets. 
bitcoin$nr_hashtags <- sapply(bitcoin$hashtags, function(x) if(!is.na(x)) length(str_split(x, " ")[[1]]) else 0)
summary(bitcoin$nr_hashtags)

ggplot(bitcoin, aes(x = nr_hashtags)) +
  geom_bar(width = 1, fill = "dark red")+
  xlab("Number of hashtags in tweet")+
  ylab("Frequency")+
  ggtitle("Frequency of number of hashtags in a tweet")

#The amount of retweets or likes a tweets has could be an indication of the 
#creditability of the creator. An option would be to use the amount of retweets
#as a weight for the sensitivity score of day 

#retweets
summary(bitcoin$retweet_count)
ggplot(bitcoin, aes(x = retweet_count))+
  geom_bar(width = 1, fill = "dark red")+
  xlab("Number of retweets")+
  ylab("Frequency")+
  ggtitle("Frequency of Retweets")+
  ylim(0, 100)

#Check the percentage of tweets with at least x amount of retweets
x <- c(1,2,3,4,5)
for(i in x){
  print(paste("x = ", i, sep = ""))
  print(paste(round(length(bitcoin$retweet_count[which(bitcoin$retweet_count > i)])/length(bitcoin$retweet_count)*100, 2), "%", sep = ""))
}

#favorite_count
summary(bitcoin$favorite_count)

x <- c(1,2,3,4,5)
for(i in x){
  print(paste("x = ", i, sep = ""))
  print(paste(round(length(bitcoin$favorite_count[which(bitcoin$favorite_count > i)])/length(bitcoin$favorite_count)*100, 2), "%", sep = ""))
}

#amount of followers could also contain relevant information about the creditability
#of a creator
summary(bitcoin$followers_count)

ggplot(bitcoin, aes(x = followers_count)) +
  geom_bar(width = 100, fill = "dark red") +
  xlab("Number of followers (width = 100)")+
  ylab("Frequency")+
  ggtitle("Frequency of number of followers (width = 100)")+
  xlim(0, 10000)

ggplot(bitcoin, aes(x = log(followers_count), y = favorite_count))+
  geom_point()+
  facet_wrap(~factor(verified)) +
  xlab("Log of Number of followers")+
  ylab("Likes per tweet")+
  ggtitle("relations between followers of creator and favorites on a tweet")

#let's check how many accounts an user follows and more specificaly how this is 
#compared to how many accounts that follow them
bitcoin$followers_rel <- bitcoin$friends_count/bitcoin$followers_count
#if this number is 5, the user follows 5 times more accounts than accounts follow him
summary(bitcoin$followers_rel) #inf -> zero followers

#there is also something as "Reputation" which is equal to the amount of friends
#divided by the sum of friends and followers

bitcoin$Reputation <- (bitcoin$friends_count)/((bitcoin$friends_count + bitcoin$followers_count))
summary(bitcoin$Reputation)

ggplot(bitcoin, aes(x = Reputation, y = nr_hashtags))+
  geom_point(col = "dark red")+
  xlab("Reputation")+
  ylab("number of hashtags in tweets")+
  ggtitle("number of hashtags in a tweet in relation to the reputation of creator")

ggplot(bitcoin, aes(x = followers_count, y = nr_hashtags))+
  geom_point(col = "dark red")+
  xlab("Number of followers")+
  ylab("Total #'s in tweet")+
  ggtitle("Relation between followers of creator and hashtags in tweet")+
  xlim(0, quantile(bitcoin$followers_count, 0.9))


#how many creators are verified?
length(unique(bitcoin$screen_name[which(bitcoin$verified == T)]))
#181

#Age of an account
dates <- lapply(bitcoin$account_created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)
bitcoin$account_created_at <- dates

bitcoin$age_account_days <- as.numeric(bitcoin$created_at - bitcoin$account_created_at)
summary(bitcoin$age_account_days)

ggplot(bitcoin, aes(x = age_account_days)) +
  geom_bar(width = 100, fill = "dark red") +
  xlab("Age of account at posting of the tweet (days)")+
  ylab("Frequency")+
  ggtitle("Age of the account who tweeted in days")

#let's set it in years
bitcoin$age_account_years <- bitcoin$age_account_days/365
summary(bitcoin$age_account_years)

ggplot(bitcoin, aes(x = age_account_years)) +
  geom_bar(width = 0.5, fill = "dark red") +
  xlab("Age of account at posting of the tweet (years)")+
  ylab("Frequency")+
  ggtitle("Age of the account who tweeted in years")

quantile(bitcoin$age_account_years, c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8))
length(bitcoin$text[which(bitcoin$age_account_years >= 1)])
length(bitcoin$text[which(bitcoin$age_account_years >= 0.5)])

length(bitcoin$text[which(bitcoin$age_account_years <= 1/12)])

#accounts who only exist for a very short time could be spam accounts

#let's calculate the lexical richness of the tweets

#Type-token ratio
getTTR <- function(data){
  data <- data %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
  nr_tokens <- length(str_split(data, " ")[[1]])
  unique_tokens <- length(unique(str_split(data, " ")[[1]]))
  ttr <- unique_tokens/nr_tokens
  return(ttr)
}

ttr <- lapply(bitcoin$text, FUN = getTTR)
ttr <- ttr %>% reduce(c)
bitcoin$ttr <- ttr
summary(bitcoin$ttr)

#lexical density
getLD <- function(data){
  data_stopwords <- data %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
  data_no_stopwords <- str_split(data_stopwords, " ")[[1]][which(!str_split(data_stopwords, " ")[[1]] %in% stop_words$word)]
  denominator <- length(data_no_stopwords)
  nominator <- length(str_split(data_stopwords, " ")[[1]])
  ld <- denominator/nominator
  return(ld)
}


############################## Spam detection model ############################
################################################################################
#In the previous part (Data Exploration) we took a closer look at the available
#variables in the rtweet dataset resulting from scrapping tweets. 
#
#In the next phase we will develop a model that can detect spam tweets based on a 
#set of variables. To avoid having to label a part of our own tweets as spam or quality
#we will search for a only database similar in all aspects to our own database
#which already contains the correct labeling. From the resulting model of this data-
#base using Machine Learning techniques we will than filter our own database for spam. 

#Current available variables are:
#
#- number of followers
#- number of friends (following)
#- number of hashtags in the tweet
#- age of the account (available in days and years)
#- number of retweets
#- number of likes 
#- Reputation of the user ((friends)/(followers+friends))
#- Type-token ratio
#- lexical density
#
#
#Given that we don't find a suitable dataset online, we will have to create or own
#labels for the spam. We will do this as follows: 
#1) We select a subset from the current database which we will label as spam or quality.
#For the size of the selection we pick a 7 times (the number of variables + safety) ->
#we currently have 9 variables, let's add a safety of 3. This would make 12*7 = 840 ~ 850.
#We will select 850 random observations from the current dataset. 

#don't even think about touching this part of the code
################################################################################
#spam_size = 850

#labels <- numeric(850)
#1 = spam
#0 = quality

#set.seed(1)
#bitcoin_spam_subset <- sample_n(bitcoin, spam_size)
#write_csv(bitcoin_spam_subset, "bitcoin_spam_subset.csv")
#save(labels, file = "SpamLabelVector.rda")
#which labels have been analyzed: 850/850
#for(i in 851:851){
  #print(bitcoin_spam_subset$text[i])
  #print("Label: ")
  #labels[i] = readline()
#}

#All 850 tweets are labbeld
#in the next steps we first add the label row to the dataset and than create some 
#more variables that become clear during the labbeling! 

#bitcoin_spam_dataset <- data.frame(bitcoin_spam_subset, spam = as.numeric(labels))
#write.csv(bitcoin_spam_dataset,"bitcoin_spam_dataset.csv")

#while labbeling the tweets I noticed that a lot of the spam tweets contain words 
#like "free", "join now", "follow ...", "mining", "dm", "sign up" an much more. 
#We make a variable containing a logical value indicating the present of these words 
bitcoin_spam_dataset <- read_csv("bitcoin_spam_dataset.csv")
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

bitcoin_spam_dataset$signal_words <- sapply(bitcoin_spam_dataset$text, getSignalWordIndicator) %>% reduce(c)
summary(bitcoin_spam_dataset$signal_words)

#a second variable we add is the proportion of the text that are hashtags
getRelNumberOfHashtags <- function(data){
  output <- numeric(length = length(data$text))
  for(i in 1:length(data$text)){
    t <- data$text %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "")
    l <- length(str_split(t[i], " ")[[1]])
    nrh <- data$nr_hashtags[i] %>% reduce(c)
    rel <- nrh/l
    output[i] = rel
    print(i)
  }
  return(output)
}

bitcoin_spam_dataset$rel_hashtags <- getRelNumberOfHashtags(bitcoin_spam_dataset)
bitcoin_spam_dataset$ld <- lapply(bitcoin_spam_dataset$text, FUN = getLD) %>% reduce(c)
#looked for more variables in a paper and will put them in after feedback assignment HRM

#total number of digits in the screen name -> fake/spam accounts and thus tweets
#typacilly have numbers in their name
getDigitsInName <- function(data){
  output <- numeric(length(data))
  for(i in 1:length(data)){
    nr <- str_count(data[i], "\\d")
    output[i] = nr
  }
  return(output)
}

bitcoin_spam_dataset$digitsInName <- getDigitsInName(bitcoin_spam_dataset$screen_name)

#spam tweets typically mention a lot of names to generate a wider range. We therefor
#check the relationship between the number of characters in the mentions and the number
#(length) of the text

getCharMentionsOverText <- function(data){
  output <- numeric(length = length(data$mentions_screen_name))
  for(i in 1:length(data$mentions_screen_name)){
    rel <- nchar(data$mentions_screen_name[i])/data$display_text_width[i]
    output[i] <- rel
  }
  output[is.na(output)] <- 0 #na means that there where no mentions thus a zero in the denominator
  return(output)
}

bitcoin_spam_dataset$MentionsRatio <- getCharMentionsOverText(bitcoin_spam_dataset)

#Activity: how many tweets does the creators tweets on a daily base
bitcoin_spam_dataset$Activity <- bitcoin_spam_dataset$statuses_count/bitcoin_spam_dataset$age_account_days

bitcoin_spam_dataset$age_account_days[which(bitcoin_spam_dataset$Activity == Inf)]
#Inf means the nominator is a zero -> new account -> all activity is from one day, so let's set equal to statuses_count
bitcoin_spam_dataset$Activity[which(bitcoin_spam_dataset$Activity == Inf)] <- bitcoin_spam_dataset$statuses_count[which(bitcoin_spam_dataset$Activity == Inf)]
#we write the final and correct file and never touch it again. This way it will not 
#change!!!
#write_csv(bitcoin_spam_dataset, "bitcoin_spam_dataset.csv")
#Start of the Model!!
################################################################################
bitcoin_spam <- read_csv("bitcoin_spam_dataset.csv")
#subset the variables
bitcoin_spam <- bitcoin_spam[, c("screen_name","nr_hashtags", "friends_count", "followers_count", 
                                 "Reputation", "age_account_days", 
                                 "signal_words", "ttr", "ld", "spam", "display_text_width", "retweet_count",
                                 "Activity", "MentionsRatio", "digitsInName")]

#looking for and handling missing values
sum(is.na(bitcoin_spam))
apply(is.na(bitcoin_spam), 2, which) #NA's are in Reputation, because there are only two, we will remove them
bitcoin_spam <- bitcoin_spam %>% drop_na(Reputation)
#train and test set
#to avoid having a connection between the train and test set we make sure that no user
#in the train set is also present in the test set. Therefor we split train and test
#based on different users
size_split <- length(unique(bitcoin_spam$screen_name))
size_split

#train = 70% of unique screen_name and test 30%
set.seed(1)
names <- unique(bitcoin_spam$screen_name)
train_names <- sample(names, size_split*0.7)
test_names <- names[!(names %in% train_names)]
intersect(train_names, test_names) #test if intersect is empty as it should

train_data <- bitcoin_spam[which(bitcoin_spam$screen_name %in% train_names),]
train_data$signal_words <- as.factor(train_data$signal_words)
train_data$spam <- as.factor(train_data$spam)
levels(train_data$spam) <- c("quality", "spam")

test_data <- bitcoin_spam[which(bitcoin_spam$screen_name %in% test_names),]
test_data$signal_words <- as.factor(test_data$signal_words)
test_data$spam <- as.factor(test_data$spam)
levels(test_data$spam) <- c("quality", "spam")

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
preds <- predict(rf_model, test_data[, variables_to_use], type = "prob")
preds_value <- ifelse(preds$quality > 0.5, "quality", "spam")
x <- evalm(rf_model)
x$roc
x$stdres

table(preds_value, test_data$spam)
################################################################################
### Implementation of Spamfilter on full dataset!!
################################################################################
bitcoin
bitcoin_spam <- read_csv("bitcoin_spam_dataset.csv")

#before we can run the model, we perform two steps:
#1) take out the observations used to train and test the model (we already have those labels)
#2) add the newly added variables to the larger dataset (functions can be reused!)

bitcoin$MentionsRatio <- getCharMentionsOverText(bitcoin)
bitcoin$digitsInName <- getDigitsInName(bitcoin$screen_name)
bitcoin$signal_words <- sapply(bitcoin$text, FUN = getSignalWordIndicator) %>% reduce(c)
bitcoin$signal_words <- factor(bitcoin$signal_words)
bitcoin$Activity <- bitcoin$statuses_count/bitcoin$age_account_days
bitcoin$age_account_days[which(bitcoin$Activity == Inf)]
#Inf means the nominator is a zero -> new account -> all activity is from one day, so let's set equal to statuses_count
bitcoin$Activity[which(bitcoin$Activity == Inf)] <- bitcoin$statuses_count[which(bitcoin$Activity == Inf)]
bitcoin$ld <- lapply(bitcoin$text, FUN = getLD) %>% reduce(c)

#remove the tweets of which we already have the labels (manualy)
bitcoin %>% filter(!(bitcoin$text %in% bitcoin_spam$text))

sum(is.na(bitcoin[, variables_to_use]))
summary(bitcoin[, variables_to_use]) #same problem as test data we remove them as we assume them to be spam anyway
bitcoin <- bitcoin %>% drop_na(Reputation)

bitcoin_label <- predict(rf_model, newdata = bitcoin[, variables_to_use])

bitcoin$spam <- bitcoin_label
bitcoin$spam <- factor(bitcoin$spam)
bitcoin_spam$spam <- factor(ifelse(bitcoin_spam$spam == 1, "spam", "quality"))
bitcoin_spam <- subset(bitcoin_spam, select = -rel_hashtags)
bitcoin_spam <- bitcoin_spam[, colnames(bitcoin)] #make sure columns are in same order
bitcoin_no_spam <- rbind(bitcoin, bitcoin_spam)
bitcoin_no_spam <- bitcoin_no_spam %>% filter(spam == "quality")

#write_csv(bitcoin_no_spam, "bitcoin_no_spam.csv")

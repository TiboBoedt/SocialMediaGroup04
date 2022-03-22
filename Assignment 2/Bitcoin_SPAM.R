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
#we check the relation of followers_rel with the age of an account
ggplot(bitcoin, aes(y = followers_rel, x = age_account_years))+
  geom_point(col = "blue")+
  ylab("Relative number of followers")+
  xlab("Age of the account of in years")+
  ggtitle("Relation between relative followers and age of account")

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

### --- ###

### LEXICON APPROACH
#In this part of the code we analyse the sentiment around bitcoin on a daily base,
#using various lexicons. 

#load the dictionary provided in the lecture
head(dictionary, 10)

#reduce the 1 to 9 likert scale to -4 <--> 4 scale. 
#dictionary <- dictionary %>% mutate(across(where(is.numeric), function(x) x - 5))
head(dictionary, 10)


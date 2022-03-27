################################################################################
################################ BITCOIN SENTIMENT #############################
################################################################################
head(Bitcoin, 10) #file die uiteindelijk geschreven wordt hier nog schrijven

### UNSUPERVISED METHOD
################################################################################

################################################################################
## LEXICON APPROACH
################################################################################
#Let's start by using the lexicon approach. In the first step we look for different
#dictionaries available and make some changes to them if necessary. 
#dictionary 1: dictionary provided in the lecture
head(dictionary, 10)
#just as in the lecture we rescale the 1 to 9 likert scale to a -4 to 4 scale with
#<0: negative sentiment, =0: neutral sentiment and >0: positive sentiment
dictionary <- dictionary %>% mutate(across(where(is.numeric),function(x) x-5 ))
head(dictionary, 10)
#We check this dictionary for some important financial words: "bullish" and "bearish"
dictionary[which(dictionary$Word == "bullish"),]
dictionary[which(dictionary$Word == "bearish"),]
#both words are not present, let's check "sell" and "buy"
dictionary[which(dictionary$Word == "sell"),]
dictionary[which(dictionary$Word == "buy"),]
#-> bought words are present altough sell does have a positive sentiment which does 
#not make much sense in the context we want to use this dictionary. We thus look for 
#a more appropiate dictionary. 

#https://bookdown.org/Maxine/tidy-text-mining/the-sentiments-dataset.html
#on this website we find 4 more dictionaries: "AFINN", "bing", "nrc" and "loughran"
#of which "loughran" is supposed to be the one for financial sentiment. We do need 
#to be carefull as all four use different scoring mechanismens

#AFINN
afinn <- get_sentiments("afinn")
#again look for some important financial words
afinn[which(afinn$word == "bullish"),]
afinn[which(afinn$word == "bearish"),]
afinn[which(afinn$word == "buy"),]
afinn[which(afinn$word == "sell"),]
#afinn does not even contain the words buy and sell thus it does not seem a better option
#than the first dictionary

#bing
bing <- get_sentiments("bing")
head(bing, 10)
#again look for some important financial words
bing[which(bing$word == "bullish"),]
bing[which(bing$word == "bearish"),]
bing[which(bing$word == "buy"),]
bing[which(bing$word == "sell"),]
#the bing dictionary contains the words bearish and bullish, but not the words buy and
#sell, which are ofcourse important words for our analysis. But maybe we could add these
#words ourselves as there is no difficult scoringsystem used (pos/neg) or combine
#this dictionary with another one. 

#nrc
nrc <- get_sentiments("nrc")
summary(factor(nrc$sentiment))
#again look for some important financial words
nrc[which(nrc$word == "bullish"),]
nrc[which(nrc$word == "bearish"),]
nrc[which(nrc$word == "buy"),]
nrc[which(nrc$word == "sell"),]
#only the word bearish, but I'm affraid not necessarily in the context of finance. 

#loughran
loughran <- get_sentiments("loughran")
#again look for some important financial words
loughran[which(loughran$word == "bullish"),]
loughran[which(loughran$word == "bearish"),]
loughran[which(loughran$word == "buy"),]
loughran[which(loughran$word == "sell"),]

#No dictionary seems to be extremely helpful and thus we run a quick scan/analysis using
#them, yet not losing to much time on this topic. 

#for this quick analysis we use the bing dictionary and adapted a bit. This because 
#it came the closest to a dictionary we would desire. 
head(bing, 10)
#add new important words
#uni-grams
bing[which(bing$word == "bearish"),] #ok
bing[which(bing$word == "bullish"),] #ok
bing[which(bing$word == "bear"),] #add
bing[which(bing$word == "bull"),] #add
bing[which(bing$word == "buy"),] #add
bing <- rbind(bing, data.frame(word = c("buy", "bear", "bull"),
                               sentiment = c("positive", "negative", "positive")))
bing[which(bing$word == "sell"),] #add
bing <- rbind(bing, data.frame(word = "sell", sentiment = "negative"))
bing[which(bing$word == "up"),] #add
bing <- rbind(bing, data.frame(word = "up", sentiment = "positive"))
bing[which(bing$word == "down"),] #add
bing <- rbind(bing, data.frame(word = "down", sentiment = "negative"))
bing[which(bing$word == "decentralized"),] #add
bing[which(bing$word == "decentralised"),] #add
bing[which(bing$word == "centralized"),] #add
bing[which(bing$word == "centralised"),] #add
#eventough both words are more neutral words, we saw in the tweets that when 
#people tweet about these words it is almost always in the context of being against
#the current monetary system and thus in favour of bitcoin
bing <- rbind(bing, data.frame(word = c("decentralized", "decentralised",
                                        "centralized", "centralised"), sentiment = rep("positive", 4)))
bing[which(bing$word == "dump"),]
bing[which(bing$word == "pump"),]
bing <- rbind(bing, data.frame(word = "pump", sentiment = "positive"))
bing <- rbind(bing, data.frame(word = "dump", sentiment = "negative"))
bing[which(bing$word == "shorting"),] #add
bing[which(bing$word == "short"),] #add
bing[which(bing$word == "hodl"),] #add
bing[which(bing$word == "memecoin"),] #add
bing[which(bing$word == "meme"),] #add
bing <- rbind(bing, data.frame(word = c("shorting", "short", "hodl", "memecoin", "meme"),
                               sentiment = c("negative", "negative", "positive", "negative", "negative")))
bing[which(bing$word == "ath"),] #add
bing <- rbind(bing, data.frame(word = "ath", sentiment = "positive"))
bing[which(bing$word == "defi"),] #add
bing <- rbind(bing, data.frame(word = "defi", sentiment = "positive"))
bing[which(bing$word == "moon"),] #add
bing <- rbind(bing, data.frame(word = "moon", sentiment = "positive"))
#bi-grams
#next we add some important bi-grams. This are important combination of two words
#having a meaning (think about negations for example).
bing[which(bing$word == "altcoin season"),] #add
bing[which(bing$word == "smart contract"),] #add
bing <- rbind(bing, data.frame(word = c("altcoin season", "smart contract"), 
                               sentiment = rep("positive", 2)))
#we will use don't as negation, as this will be the most often used word to negate
bing_dont <- data.frame(word = paste("don't", bing$word, sep = " "), sentiment = 
                         ifelse(bing$sentiment == "positive", "negative", "positive"))

bing_not <- data.frame(word = paste("not", bing$word, sep = " "), sentiment = 
                          ifelse(bing$sentiment == "positive", "negative", "positive"))

bing <- rbind(bing, bing_dont, bing_not)
bing$sentiment_score <- ifelse(bing$sentiment == "positive", 1, -1)
table(bing$sentiment_score)
head(bing, 10)

#write_csv(bing, "bing_updated")
#Run through the lexicon
#we use a small subset to test the process as the whole file would take to much 
#time during the testing phase.
Bitcoin
Encoding(Bitcoin$text) <- "latin1"
Bitcoin$text <- iconv(Bitcoin$text,'latin1', 'ascii', sub = '')

score_lexicon <- numeric(nrow(Bitcoin))
for(i in 1:length(score_lexicon)){
  text <- Bitcoin$text[i] %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\n", " ") %>% str_replace_all("[[:digit:]]", "")
  
  text_unigram <- str_split(text, " ")[[1]]
  
  bigram <- rbind(text_unigram, c(text_unigram[2:length(text_unigram)], " "))
  text_bigram <- paste(bigram[1,1:ncol(bigram)], bigram[2, 1:ncol(bigram)])
  
  text_v <- c(text_unigram, text_bigram)
  
  m <- match(text_v, bing$word)
  
  p <- !is.na(m)
  
  present_score <- bing$sentiment_score[m[p]]
  
  score_lexicon[i] = sum(present_score, na.rm = T)/sum(p)
  
  #add amplifiers correction: whale, !, ?
  #as tweets about whales have a big impact on the creditability we amplify their
  #sentiment
  if(str_count(Bitcoin$text[i], "whale") > 0){
    score_lexicon[i] = score_lexicon[i] * 1.5
  }
  if(str_count(Bitcoin$text[i], "!") > 0){
    score_lexicon[i] = score_lexicon[i] * 1.5
  }
  
  if (is.na(score_lexicon[i])) score_lexicon[i] <- 0 else score_lexicon[i] <- score_lexicon[i]
}

df_score_lexicon <- data.frame(Bitcoin, sentiment_score = score_lexicon)
score_daily <- group_by(df_score_lexicon, created_at) %>% summarise(sentiment = mean(sentiment_score))

ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red") +
  geom_hline(yintercept = 0, col = "blue")+
  ylim(-0.1,0.30)+
  xlab("Dates")+
  ylab("Sentiment Score")+
  ggtitle("Sentiment Score on a daily base")

#on this graph we cleary se the movement of the sentiment over the days. The visualy 
#check the relation of this sentiment with the price movement of bitcoin, we add
#pricedata to the mix

getSymbols(Symbols = "BTC-USD", src = "yahoo")
bitcoin_price_df <- data.frame(`BTC-USD`)
head(bitcoin_price_df, 10)
bitcoin_price_df <- rownames_to_column(bitcoin_price_df)
names(bitcoin_price_df)[1] <- "Date"
bitcoin_price_df$Date <- as_date(bitcoin_price_df$Date)
#filter on only the dates availables in our data
bitcoin_price_df <- bitcoin_price_df %>% filter(bitcoin_price_df$Date %in%
                                                  c(unique(Bitcoin$created_at)[1] - 1, unique(Bitcoin$created_at)))
head(bitcoin_price_df, 10)

#data is subseted to correct dates. Next we check de increase of bitcoin between closing prices
rel_close <- numeric(nrow(bitcoin_price_df))
for(i in 1:(nrow(bitcoin_price_df)-1)){
  increase_close <- (bitcoin_price_df$BTC.USD.Close[i+1] - bitcoin_price_df$BTC.USD.Close[i])/
    bitcoin_price_df$BTC.USD.Close[i]
  rel_close[i] <- increase_close
}
bitcoin_price_df$rel_close <- rel_close
bitcoin_price_df$rel_close_1daylag <- c(NA, rel_close[1:length(rel_close)-1])

ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red") +
  #geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close))+
  geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close_1daylag), col = "dark green")+
  geom_hline(yintercept = 0, col = "blue")+
  ylim(-0.15,0.3)+
  xlab("Dates")+
  ylab("Sentiment Score")+
  ggtitle("Sentiment Score on a daily base")

#we now experiment with different weighing techniques for the sentiment in aggregating 
#the sentiment on a daily base. This because now we assumed each tweet to have the same
#impact on the overal sentiment of the day, where as it makes sense that some tweets
#will have a bigger impact on the sentiment as they reach more people. 

#a first logical why to weight the sentiment is based on the followers of the creator
#of the tweet
summary(df_score_lexicon$followers_count)
ggplot(df_score_lexicon, aes(x = followers_count))+
  geom_bar()+
  xlim(0,2000)

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

df_score_lexicon$followers_count_quantile <- getQuantile(df_score_lexicon$followers_count)
df_score_lexicon_followers_w <- df_score_lexicon %>% group_by(created_at, followers_count_quantile) %>% summarise(sentiment = mean(sentiment_score))
df_score_lexicon_followers_w$weight_score <- df_score_lexicon_followers_w$followers_count_quantile*df_score_lexicon_followers_w$sentiment
df_score_lexicon_followers_w$weight_score <- df_score_lexicon_followers_w$weight_score/sum(seq(1:10))
score_daily_followers_w <- df_score_lexicon_followers_w %>% group_by(created_at) %>%
  summarise(sentiment_score = sum(weight_score))

#let's now check the difference with the initial sentiment score without weighing the sentiment
ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red")+
  geom_line(data = score_daily_followers_w, aes(x = created_at, y = sentiment_score), col = "dark green")+
  geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close_1daylag), col = "purple")+
  xlab("Date")+
  ylab("Sentiment Score/price movement")
  
#we notice the sentiment to be higher for the tweets from accounts who's 
#number of followers are in the lower quantiles.

#a next option we look at, as the resulting sentiment if we were to remove the 
#neutral sentiments from the dataset, assuming that neutral sentiments don't realy 
#affect public opinion. 

#how many tweets have a neutral score?
length(df_score_lexicon$sentiment_score[which(df_score_lexicon$sentiment_score == 0)])/
  length(df_score_lexicon$sentiment_score) * 100
#around 45% of the tweets have a neutral opinion and all those zero's have a big impact 
#on the average that is calculated

df_score_no_neutral <- df_score_lexicon %>% filter(sentiment_score != 0)
df_score_no_neutral_daily <- df_score_no_neutral %>% group_by(created_at) %>%
  summarise(sentiment_score = mean(sentiment_score))

ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red")+
  geom_line(data = df_score_no_neutral_daily, aes(x = created_at, y = sentiment_score), col = "dark green")+
  geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close_1daylag), col = "purple")+
  xlab("Date")+
  ylab("Sentiment Score/price movement")

#overal sentiment becomes much more positive. We do notice the shape of the graph
#doesn't changes, only the location. 

#An other option we could consider is the favorites a tweet has gotten, only we should
#be carefull with this as some tweets were posted earlier on the day. 
summary(Bitcoin$favorite_count)
quantile(Bitcoin$favorite_count, c(0.7, 0.8, 0.9, 0.95, 0.99))

summary(Bitcoin$verified)

#for the aggregate sentiment of a day we could also assume that the sentiment of the 
#day before will still play it's part. Lets start by going only one day back and give
#the days a weight of 0.7 for today and 0.3 for today - 1
tic()
one_day_back_w <- numeric(length(score_daily$sentiment))
for(i in 1:length(score_daily$sentiment)){
  if(i == 1){
    one_day_back_w[i] = score_daily$sentiment[i]
  }
  else{
    one_day_back_w[i] = score_daily$sentiment[i]*0.7 + score_daily$sentiment[i-1]*0.3
  }
}
toc()
score_daily$sentiment_tminus1day_7_3 <- one_day_back_w

ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red")+
  geom_line(aes(y = sentiment_tminus1day_7_3), col = "dark green")+
  xlab("Date")+
  ylab("Sentiment Score")

tic()
two_day_back_w <- numeric(length(score_daily$sentiment))
for(i in 1:length(score_daily$sentiment)){
  if(i == 1){
    two_day_back_w[i] = score_daily$sentiment[i]
  }
  else if(i == 2){
    two_day_back_w[i] = score_daily$sentiment[i]*0.7 + score_daily$sentiment[i-1]*0.3
  }
  else{
    two_day_back_w[i] = score_daily$sentiment[i]*0.7 + score_daily$sentiment[i-1]*0.2 +
      score_daily$sentiment[i - 2]*0.1
  }
}
toc()

score_daily$sentiment_tminus2days_7_2_1 <- two_day_back_w

ggplot(score_daily, aes(x = created_at, y = sentiment))+
  geom_line(col = "dark red")+
  #geom_line(aes(y = sentiment_tminus1day_7_3), col = "dark green")+
  geom_line(aes(y = sentiment_tminus2days_7_2_1), col = "purple")+
  xlab("Date")+
  ylab("Sentiment Score")
#we notice that it smoothens the curve 
#let go back 2 days in time and give a 70%-20%-10% weight


################################################################################
## SENTIMENTR
################################################################################

sentimentR_text <- Bitcoin %>% pull(text)

#let's check sentiment without any pre-processing of the text nor using different type
#of weights 
sentimentr_dirt <- sentimentR_text %>% get_sentences() %>% sentiment_by()
#By default, the sentiment_by function downweights the zero for averaging. 
#The reason is that you don’t want the neutral sentences to have a strong influence.
#We check the impact of different averaging techniques
sentimentr_dirt_average_weighted_mixed_sentiment <- sentimentR_text %>% get_sentences()%>%
  sentiment_by(averaging.function = average_weighted_mixed_sentiment)

sentimentr_dirt_average_mean <- sentimentR_text %>% get_sentences() %>% 
  sentiment_by(averaging.function = average_mean)

df_score_sentimentr_dirt <- data.frame(date = Bitcoin$created_at, sentimentr_dirt = sentimentr_dirt$ave_sentiment,
                                  sentimentr_dirt_average_mean = sentimentr_dirt_average_mean$ave_sentiment, 
                                  sentimentr_dirt_average_weighted_mixed_sentiment = sentimentr_dirt_average_weighted_mixed_sentiment$ave_sentiment)

df_score_sentimentr_dirt_daily <- df_score_sentimentr_dirt %>% group_by(date) %>%
  summarise(sentiment1 = mean(sentimentr_dirt), sentiment2 = mean(sentimentr_dirt_average_mean),
            sentiment3 = mean(sentimentr_dirt_average_weighted_mixed_sentiment))

ggplot(df_score_sentimentr_dirt_daily, aes(x = date))+
  geom_line(aes(y = sentiment1, colour = "sentiment1"), col = "dark red", show.legend = T)+
  geom_line(aes(y = sentiment2, colour = "sentiment2"), col = "dark green", show.legend = T)+
  geom_line(aes(y = sentiment3, colour = "sentiment3"), col = "dark blue", show.legend = T)+
  labs(x = "Date", y = "Sentiment Score", colour = "legend")

#let's now check the results of the sentimentR package if we clean the text before
#we run the sentiment 

sentimentR_text <- iconv(sentimentR_text, from = "latin1", to = "ascii", sub = "byte")

sentimentR_text <- sentimentR_text %>%   replace_emoji() %>% replace_emoticon() %>% 
  replace_contraction() %>% replace_internet_slang() %>% replace_kern() %>% replace_word_elongation()

cleanText <- function(text) {
  clean_texts <- text %>%
    str_replace_all("<.*>", "") %>%                         # remove remainig emojis
    str_replace_all("&amp;", "") %>%                        # remove &
    str_replace_all("(RT|via)((?:\\b\\W*@\\w+)+)", "") %>%  # remove retweet entities
    str_replace_all("@\\w+", "") %>%                        # remove @ people, replace_tag() also works
    str_replace_all('#', "") %>%                            #remove only hashtag, replace_hash also works
    str_replace_all("[[:punct:]]", "") %>%                  # remove punctuation
    str_replace_all("[[:digit:]]", "") %>%                  # remove digits
    str_replace_all("http\\w+", "") %>%                     # remove html links replace_html() also works
    str_replace_all("[ \t]{2,}", " ") %>%                   # remove unnecessary spaces
    str_replace_all("^\\s+|\\s+$", "") %>%                  # remove unnecessary spaces
    str_trim() %>% 
    str_to_lower()
  return(clean_texts)
}
sentimentR_text <- cleanText(sentimentR_text)

lemma_dictionary_hs <- make_lemma_dictionary(sentimentR_text, engine = 'hunspell')
sentimentR_text <- lemmatize_strings(sentimentR_text, dictionary = lemma_dictionary_hs)

sentiment_clean_1 <- sentimentR_text %>% get_sentences() %>% sentiment_by()

sentiment_clean_2 <- sentimentR_text %>% get_sentences() %>% sentiment_by(averaging.function = average_weighted_mixed_sentiment)

sentiment_clean_3 <- sentimentR_text %>% get_sentences() %>% sentiment_by(averaging.function = average_mean)

df_score_sentimentR <- data.frame(date = Bitcoin$created_at, sentiment_score_1 = 
                                    sentiment_clean_1$ave_sentiment, sentiment_score_2 = 
                                    sentiment_clean_2$ave_sentiment, sentiment_score_3 =
                                    sentiment_clean_3$ave_sentiment)

df_score_sentimentR_daily <- df_score_sentimentR %>% group_by(date) %>%
  summarise(sentiment_score_1 = mean(sentiment_score_1), sentiment_score_2 = mean(sentiment_score_2),
            sentiment_score_3 = mean(sentiment_score_3)) 

ggplot(df_score_sentimentr_dirt_daily, aes(x = date, y = sentiment1))+
  geom_line(col = "dark green")+
  geom_line(data = df_score_sentimentR_daily, aes(x = date, y = sentiment_score_1), col = "dark red")+
  xlab("Date")+
  ylab("Sentiment Score")

ggplot(df_score_sentimentr_dirt_daily, aes(x = date, y = sentiment2))+
  geom_line(col = "dark green")+
  geom_line(data = df_score_sentimentR_daily, aes(x = date, y = sentiment_score_2), col = "dark red")+
  xlab("Date")+
  ylab("Sentiment Score")

ggplot(df_score_sentimentr_dirt_daily, aes(x = date, y = sentiment3))+
  geom_line(col = "dark green")+
  geom_line(data = df_score_sentimentR_daily, aes(x = date, y = sentiment_score_3), col = "dark red")+
  xlab("Date")+
  ylab("Sentiment Score")

#Let's now check it's relation with the price movements of bitcoin
ggplot(df_score_sentimentR_daily, aes(x = date, y = sentiment_score_1))+
  geom_line(col = "dark red")+
  geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close_1daylag), col = "dark green")+
  ggtitle("Movement of sentiment compared to price")
#we notice that the text clean made the sentiment much more negative, less take a look
#at the tweets and see what made them negative

sentiment_clean_1 %>% highlight()
#reading tweets from the highligth show that the more negative sentiment seems to make
#sense. Let's look at the same tweets without the text cleaning
sentimentr_dirt %>% highlight()
#some make sense, some don't. It seems like both with text cleaned and text not cleaned
#sentimentR is able to identify some of the positive and negative tweets, however in general
#I feel like there is a lot of improvement possible. 

#comparing the results from the sentimentR package with the lexicon approach is a bit 
#difficult because the are on a different scale. This is not the case however for the 
#Vader package, which is also on a -1 <-> 1 scale. Thus we now check the resulting
#sentiment from the Vader package as a conclusion for this file. 

vader_text <- Bitcoin %>% pull(text)

vader_dirt <- vader_text %>% vader_df()
vader_dirt$compound

vader_text <- vader_text %>% replace_emoji() %>% replace_emoticon() %>% 
  replace_contraction() %>% replace_internet_slang() %>% replace_kern() %>% replace_word_elongation()

vader_text <- cleanText(vader_text)

vader_clean <- vader_text[1:200] %>% vader_df()

vader_clean$compound

#run the vader package takes to much time! 
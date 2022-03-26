################################################################################
################################ BITCOIN SENTIMENT #############################
################################################################################
head(Bitcoin, 10) #file die uiteindelijk geschreven wordt hier nog schrijven

### UNSUPERVISED METHOD
################################################################################

## LEXICON APPROACH
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
bing[which(bing$word == "shorting"),] #add
bing[which(bing$word == "short"),] #add
bing[which(bing$word == "hodl"),] #add
bing[which(bing$word == "memecoin"),] #add
bing[which(bing$word == "meme"),] #add
bing <- rbind(bing, data.frame(word = c("shorting", "short", "hodl", "memecoin", "meme"),
                               sentiment = c("negative", "negative", "postive", "negative", "negative")))
bing[which(bing$word == "ath"),] #add
bing <- rbind(bing, data.frame(word = "ath", sentiment = "positive"))
bing[which(bing$word == "defi"),] #add
bing <- rbind(bing, data.frame(word = "defi", sentiment = "positive"))

#bi-grams
#next we add some important bi-grams. This are important combination of two words
#having a meaning (think about negations for example).
bing[which(bing$word == "altcoin season"),] #add
bing[which(bing$word == "smart contract"),] #add
bing <- rbind(bing, data.frame(word = c("altcoin season", "smart contract"), 
                               sentiment = rep("positive", 2)))
#we will use don't as negation, as this will be the most often used word to negate
bing_neg <- data.frame(word = paste("don't", bing$word, sep = " "), sentiment = 
                         ifelse(bing$sentiment == "positive", "negative", "positive"))

bing <- rbind(bing, bing_neg)
bing$sentiment_score <- ifelse(bing$sentiment == "positive", 1, -1)
table(bing$sentiment_score)
head(bing, 10)
#Run through the lexicon
#we use a small subset to test the process as the whole file would take to much 
#time during the testing phase.
#subset <- Bitcoin[sample(nrow(Bitcoin), 50000),]
subset <- Bitcoin
Encoding(subset$text) <- "latin1"
subset$text <- iconv(subset$text,'latin1', 'ascii', sub = '')

subset_score <- numeric(nrow(subset))
for(i in 1:length(subset_score)){
  text <- subset$text[i] %>% str_to_lower() %>% str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\n", " ") %>% str_replace_all("[[:digit:]]", "")
  
  text_unigram <- str_split(text, " ")[[1]]
  
  bigram <- rbind(text_unigram, c(text_unigram[2:length(text_unigram)], " "))
  text_bigram <- paste(bigram[1,1:ncol(bigram)], bigram[2, 1:ncol(bigram)])
  
  text_v <- c(text_unigram, text_bigram)
  
  m <- match(text_v, bing$word)
  
  p <- !is.na(m)
  
  present_score <- bing$sentiment_score[m[p]]
  
  subset_score[i] = sum(present_score, na.rm = T)/sum(p)
  
  #add amplifiers correction: whale, !, ?
  #as tweets about whales have a big impact on the creditability we amplify their
  #sentiment
  if(str_count(subset$text[i], "whale") > 0){
    subset_score[i] = subset_score[i] * 1.5
  }
  if(str_count(subset$text[i], "!") > 0){
    subset_score[i] = subset_score[i] * 1.5
  }
  
  if (is.na(subset_score[i])) subset_score[i] <- 0 else subset_score[i] <- subset_score[i]
}

df_score <- data.frame(subset, sentiment_score = subset_score)
score_daily <- group_by(df_score, created_at) %>% summarise(sentiment = mean(sentiment_score))

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
                                                  c(unique(subset$created_at)[1] - 1, unique(subset$created_at)))
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
  #geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close * 10))+
  geom_line(data = bitcoin_price_df, aes(x = Date, y = rel_close_1daylag), col = "dark green")+
  geom_hline(yintercept = 0, col = "blue")+
  ylim(-0.15,0.3)+
  xlab("Dates")+
  ylab("Sentiment Score")+
  ggtitle("Sentiment Score on a daily base")

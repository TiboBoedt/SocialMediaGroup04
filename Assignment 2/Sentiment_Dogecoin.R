################################################################################
############################# BITCOIN SENTIMENT ################################
################################################################################


#################################################################################
# Lexicon based approach
#################################################################################

#read in the csv file
Dogecoin <- read_twitter_csv("Dogecoin.csv")
#remove duplicate text 
Dogecoin <- Dogecoin %>% distinct(text, .keep_all = TRUE)


#############################  Preprocessing #########################

#Pre-process text
Dogecoin_text <- Dogecoin %>% pull(text)
Dogecoin_text = Dogecoin_text %>% replace_emoticon() %>% replace_word_elongation()

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
Dogecoin_clean <- cleanText(Dogecoin_text)

############################  Sentiment R #######################
p_load(tidyverse,textclean, textstem, sentimentr, lexicon)

Dogecoin_Sentiment = Dogecoin_clean %>% get_sentences() %>% sentiment_by()

# Change valence of certain words
valence_shifters_updated <-update_valence_shifter_table(key = hash_valence_shifters,
                                                        x = data.frame(x = 'exclamation', y = 2))

Dogecoin_Sentiment_Adj = Dogecoin_clean %>% 
  str_replace_all('!', ' exclamation') %>% 
  get_sentences() %>% 
  sentiment_by(valence_shifters_dt = valence_shifters_updated)

############################  Vader ##############################

# Create lemmatization

# Source = https://github.com/michmech/lemmatization-lists

#Open the dictionairy
library(readr)
file = read.delim('/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/lemmatization-lists-master/lemmatization-en.txt', header = FALSE, sep = "\t")
library(Dict)
dict = Dict$new(file$V1 , file$V2) # Nog fixen

# Create own dict
lemma_dictionary_hs <- make_lemma_dictionary(text_clean, engine = 'hunspell')
text_final <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)

# Create final sentiment_vader
sentiment_vader <- text_final %>% get_sentences() %>% sentiment_by()
# sentiment_vader %>% highlight() # Cool addition


############################  Machine Learning Approach #######################

# Devide into training and test set

set.seed(1000) 

ind <- sample(x = nrow(Dogecoin_clean), size = nrow(Dogecoin_clean), replace = FALSE)
train <- SentimentReal[1:floor(length(ind)*.60),]
test <- SentimentReal[(floor(length(ind)*.60)+1):(length(ind)),]

#### Create training/ test corpora

corpus_train <- Corpus(VectorSource(train$message))
corpus_test <- Corpus(VectorSource(test$message))

# Tokenizer

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))


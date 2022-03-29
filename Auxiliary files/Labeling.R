Sentiments = read_csv("/Users/xavierverbrugge/Downloads/tweets_sentiment_toLabel.csv")

#the right file has to be uploaded here, tweet_df results out of the remove_spam file
dates <- lapply(tweet_df$created_at, function(x) if(is.na(as.numeric(x))) as_date(x) else as_date(as_datetime(as.numeric(x))))
dates <- dates %>% reduce(c)
bitcoin$created_at <- dates
(unique_dates <- unique(tweet_df$created_at))
day1 = correct_tweets%>%filter(created_at == "2022-03-11")
day2= correct_tweets%>%filter(created_at == "2022-03-12")
day3 = correct_tweets%>%filter(created_at == "2022-03-13")
day4 = correct_tweets%>%filter(created_at == "2022-03-14")
day5 = correct_tweets%>%filter(created_at == "2022-03-15")
day6 = correct_tweets%>%filter(created_at == "2022-03-16")
day7 = correct_tweets%>%filter(created_at == "2022-03-17")
day8 = correct_tweets%>%filter(created_at == "2022-03-18")
day9 = correct_tweets%>%filter(created_at == "2022-03-19")
day10 = correct_tweets%>%filter(created_at == "2022-03-20")
day11= correct_tweets%>%filter(created_at == "2022-03-21")
day12= correct_tweets%>%filter(created_at == "2022-03-22")
day13= correct_tweets%>%filter(created_at == "2022-03-23")

set.seed(23)
day1_sample =day1[sample(nrow(day1),385),]
day2_sample =day2[sample(nrow(day2),385),]
day3_sample =day3[sample(nrow(day3),385),]
day4_sample =day4[sample(nrow(day4),385),]
day5_sample =day5[sample(nrow(day5),385),]
day6_sample =day6[sample(nrow(day6),385),]
day7_sample =day7[sample(nrow(day7),385),]
day8_sample =day8[sample(nrow(day8),385),]
day9_sample =day9[sample(nrow(day9),385),]
day10_sample =day10[sample(nrow(day10),385),]
day11_sample =day11[sample(nrow(day11),385),]
day12_sample =day12[sample(nrow(day12),385),]
day13_sample =day13[sample(nrow(day13),385),]

tweets_sentiment_toLabel = rbind(day1_sample, day2_sample, day3_sample,day4_sample,day5_sample,day6_sample,day7_sample,day8_sample, day9_sample,day10_sample, day11_sample, day12_sample, day13_sample)
#write_csv(tweets_sentiment_toLabel, "tweets_sentiment_toLabel.csv")





#labels <- numeric(850)
#2
#1 
#0 = Neutral
#-1
#-2

#set.seed(1)
Data <- Sentiments[2931: 5005,]
labels_first = list()

# (Start from 2942)
for(i in 1:11){
print(Data$text[i])
print("Label: ")
labels_first[i] = readline()
}


#labels_Saved = list()

labels_total_2931_5005 = labels_total_2931_5005[1:2075]


save(labels, file = "SentimentLabelVector2_2931_5005.rda")
save(labels_total, file = "/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/SentimentLabelVector2_2931_5005.rda")

View(labels)

labels_601_1500[106] = 0
labels_2501_2930 = as.list(read_csv("/Users/xavierverbrugge/Downloads/Labels/labels_1")["x"])$x
labels_601_1500 = as.list(read_csv("/Users/xavierverbrugge/Downloads/Labels/labels_2")["x"])$x
labels_1701_2500 = labels
labels_1501_1700 = as.list(read_csv("/Users/xavierverbrugge/Downloads/Labels/tweets_labeled_1501_1700.csv")["sentiment_label"])$sentiment_label
labels_1_600 = as.list(read_csv("/Users/xavierverbrugge/Downloads/Labels/tweets_labeled_1_600.csv")["sentiment_label"])$sentiment_label

labels_total = c(labels_1_600,labels_601_1500,labels_1501_1700,labels_1701_2500,labels_2501_2930,labels_total_2931_5005)
labels_total 

Sentiments$Label
library(dplyr)
Sentiments = subset(Sentiments, select = -c(Label,Sentimen_label) )

label_int = type.convert(labels_total)
Sentiments$Label = label_int
Sentiments$Sentiment_label = labels_total
Sentiments$Sentiment_label
Sentiments <- apply(Sentiments,2,as.character)

write.csv(Sentiments, file ="Tweets_And_Labels_2.csv")


labels_total[which(labels_total == "°")]
which(is.na(labels_total))
labels_total[1136] = 0




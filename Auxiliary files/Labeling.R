Sentiments = read_csv("/Users/xavierverbrugge/Downloads/tweets_sentiment_toLabel.csv")



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




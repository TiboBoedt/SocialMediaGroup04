################################################################################
################################ BITCOIN SENTIMENT #############################
################################################################################
head(Bitcoin, 10) #file die uiteindelijk geschreven wordt hier nog schrijven

### UNSUPERVISED METHOD
################################################################################
#For the unsupervised method we make use of the Vader package to get the sentiment.
#For these we first look if there are different options to clean the initial text and 
#it's influence on the sentiment resulting from the Vader Package. Once we have 
#the different sentiments we look for different weighing techniques to give more 
#weight to the sentiment of tweets that have a bigger reach, as their sentiment would 
#have a bigger impact on the general sentiment of a given day. 

## VADER PACKAGE

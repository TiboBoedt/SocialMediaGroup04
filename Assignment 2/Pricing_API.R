################################################################################
############################# PRICE's     ######################################
################################################################################

#install_github('andreacirilloac/updateR')
#library(updateR)
#updateR()

# Installing Crypto package via github
devtools::install_github("https://github.com/sstoeckl/crypto2")

# Get list of all crypto's
library(crypto2)
library(tidyverse)
library(lubridate)

coins <- crypto_list(only_active = T)

#Select desired coins
coins = coins %>% filter((symbol == "BTC") | (symbol == "ETH") | (symbol == "SAND") | (symbol == "SHIB") | (symbol == "ADA") | (symbol == "DOGE"))
coins

# Get first time we scraped a tweet
setwd("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Scraping")
Bitcoin <- read_twitter_csv("Bitcoin.csv")
lastdate = Bitcoin$created_at[1]

#Get in correct form
#Human form
#Date <- lubridate::as_datetime(as.integer(lastdate))
#Date

#Form for crypto his


# Get prices of crypto's starting from certain date.

history = crypto_history(coins, start_date="20220311", end_date="20220321")
history

# Export result
#write.table(history , file = "your_path\\df.csv")


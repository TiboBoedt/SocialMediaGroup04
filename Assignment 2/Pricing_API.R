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
coins = coins %>% filter((symbol == "BTC"))
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

history = crypto_history(coins, start_date="20220310", end_date="20220327")

# Get close of previous day
Close_Previous_Day = history$close
Close_Previous_Day = c(0,Close_Previous_Day)
Close_Previous_Day = Close_Previous_Day[1:17] #Delete last element in array
history$Close_Previous_Day = Close_Previous_Day

history$Up_Down = ifelse(history$close > history$Close_Previous_Day , 1, 0)

#remove first day 
history <- history %>% slice(-c(1))

#Export result
write.csv(history , file ="Bitcoin_Price_History.csv")


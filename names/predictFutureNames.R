

nameslist <-read.csv("data/nameslist.csv")

library(stringr)
#look at trend in name length over the years
#look at first letters trend over years





#--------------------------------------
#predict the count by year and _ for each name

#first create a training and test set????
#set.seed(1)
#train_rows <- (1:3000000)
train <- filter(nameslist, Year < 1990)
test <- filter(nameslist, Year > 1990)


#then do lm like follows
summary(lm(Count~Year, data=train))


how much did we change from this year to last year




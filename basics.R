# R Basics

# Run R from the terminal. To run the commands stored in an R file;
# (assumes the R program was started in the same dir as the file)
# source("basic.R")

# Object assignment.
my_object <- 2

# Creating a vector: basic data structure.
vec <- c(1, 2, 3, 4, 5)

# Show objects in the specified environment.
ls()

mean(vec)

# load Coronavirus Tweets
load("data/coronavirus_tweets.Rdata")

# Some other relevant commands when working with data
# setwd("~/Developer/uva-measuring-meaning/")

# Output: The entire vector
covid-tweets

# Output: The column twitter_handle
covid-tweets$twitter_handle

# Output: The first row for the column twitter_handle
# R is indexed 1->N 
covid_tweets$tweet_text[1]

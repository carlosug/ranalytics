# Predict gender from tweet meta information 
# https://therbootcamp.github.io/ML_2019Oct/_sessions/Models/Models_practical.html

# setup
library(tidyverse)
library(caret)

# import data set
tweets <- read.csv("tweets.csv")
summary(tweets)
# change character to factor
tweets <- tweets %>% mutate_if(is.character, as.factor)

# split dataset into training and test sets
train_index <- createDataPartition(tweets$gender, p = .20, list=FALSE)
tweets_train <- tweets %>% slice(-train_index)
tweets_test <- tweets %>% slice(train_index)

# test criterion
criterion <- as.factor(tweets_test$gender)

# remove unwanted features
# 1. select predictors
tweets_train_x <- tweets_train %>% select(-gender) 
tweets_train_y <- tweets_train %>% pull(gender)

# check correlations
corr_matrix <- cor(tweets_train_x) # doesnt work cause numeric one variables

# fit a logistic model
tweets_train_glm <- train(form= gender~.,
                          data = tweets_train,
                          method='glm')
summary(tweets_train)
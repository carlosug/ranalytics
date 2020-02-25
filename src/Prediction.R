# https://therbootcamp.github.io/ML_2019Oct/_sessions/Prediction/Prediction_practical.html
# Learning outputs:
# - Fit regression, decision trees and random forests to training data
# - evaluate model fitting and prediction performance in test set
# - compare the fitting and prediction performance of two models
# - explore the effects of features on model predictions

# Objective:
# - predict the grades from college dataset

# A. SETUP
library(tidyverse)
library(caret)
install.packages("party")
library(party)
install.packages("partykit")
library(partykit)

# datasets
college_train <- read.csv(file = "college_train.csv")
college_test <- read.csv(file = "college_test.csv")

# convert all characters to factor
college_train <- college_train %>%
  mutate_if(is.character, factor)
college_test <- college_test %>%
  mutate_if(is.character, factor)

# B. Fitting: predict grad.rate
ctrl_none <- trainControl(method = 'none')

# Regression
grad_gml <- train(form = Grad.Rate~.,
                  data = college_train,
                  method='glm',
                  trControl=ctrl_none)
grad_gml$finalModel
summary(grad_gml)

# use predict to save the model fit values
glm_fit <- predict(grad_gml)
glm_fit[0:5]
summary(glm_fit)

# Decision Trees
grad_rpart <- train(form = Grad.Rate~.,
                    data = college_train,
                    method='rpart',
                    tuneGrid = expand.grid(cp=.01)) # set complexity parameter
grad_rpart$finalModel
# plot the rpart model
plot(as.party(grad_rpart$finalModel))
# save fitted values
rpart_predfit <- predict(grad_rpart)


# RANDOM FOREST
grad_rf <- train(form=Grad.Rate~.,
                 data = college_train,
                 method='rf',
                 trControl=ctrl_none,
                 tuneGrid=expand.grid(mtry=2)) # set n of features randomly selected
rf_fit <- predict(grad_rf)

# Assess accuracy
criterion_train <- college_train$Grad.Rate

# calculate fitting accuracies for each model
# posResample(pred,obs)
postResample(glm_fit,criterion_train)
# decision tree
postResample(rpart_predfit, criterion_train)
# random forest
postResample(rf_fit, criterion_train)
#****************************************************************
accuracy <- tibble(criterion_train = criterion_train,
                   Regression = glm_fit,
                   DecisionTrees = rpart_predfit,
                   RandomForest = rf_fit) %>%
  gather(model, prediction, -criterion_train) %>%
  # Add error measures
  mutate(se = prediction - criterion_train,
         ae = abs(prediction - criterion_train))

# Calculate summaries
accuracy_agg <- accuracy %>%
  group_by(model) %>%
  summarise(mae = mean(ae))   # Calculate MAE (mean absolute error)
# Plot A) Scatterplot of truth versus predictions
ggplot(data = accuracy,
       aes(x = criterion_train, y = prediction)) +
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ model) +
  labs(title = "Predicting college_train$Grad.Rate",
       subtitle = "Black line indicates perfect performance")
#**********************************************************************************************

# Plot violin of absolute errors
ggplot(data = accuracy, 
       aes(x = model, y = ae, fill = model)) + 
  geom_violin() + 
  geom_jitter(width = .05, alpha = .2) +
  labs(title = "Prediction Absolute Errors",
       subtitle = "Numbers indicate means",
       x = "Model",
       y = "Absolute Error") +
  guides(fill = FALSE) +
  annotate(geom = "label", 
           x = accuracy_agg$model, 
           y = accuracy_agg$mae, 
           label = round(accuracy_agg$mae, 2))

# HOUSE PRICES IN KING COUNTRY, WASHINGTON
house_train <- read.delim("house_train.txt", header = TRUE, sep = ',')
house_test <- read.delim("house_test.txt",header = TRUE, sep = ',')
names(house_test)
# convert all character columns in factor
house_train <- house_train %>%
  mutate_if(is.character, factor)
house_test <- house_test %>%
  mutate_if(is.character, factor)
# fitting
# goal is to predict price, the selling price of WA
ctrl_none <- trainControl(method = "none")
# Regression
ml_glm <- train(form=price~.,
                data=house_train,
                method='glm',
                trControl=ctrl_none)
summary(ml_glm)
ml_glm$finalModel

# save fitted values of regression model
predict_glm <- predict(ml_glm)
predict_glm[1:10]
hist(predict_glm)

# decision tree
ml_dt <- train(form= price~.,
               data = house_train,
               method='rpart',
               trControl=ctrl_none,
               tuneGrid=expand.grid(cp=0.01))
summary(ml_dt)
plot(as.party(ml_dt$finalModel))
predict_dt <- predict(ml_dt)
hist(predict_dt)

# random forest
ml_rf <- train(form= price~.,
               data=house_train,
               method='rf',
               trControl=ctrl_none,
               tuneGrid=expand.grid(mtry=2)) # set num features
predict_rf <- predict(ml_rf)
ml_rf[1:10]
hist(predict_rf)

#Assess accuracy
criterion_train <- house_train$price

# use postResample(), determine performance of each model separately.
postResample(predict_glm,criterion_train)
postResample(predict_dt, criterion_train)
postResample(predict_rf,criterion_train)

# PREDICTION
#save the criterion values from test dataset
criterion_test <- house_test$price
criterion_test[1:10]
# save the pred values per model
# regression
ml_glm_pred <- predict(ml_glm, newdata = house_test)
# decision tree
ml_dt_pred <- predict(ml_dt, newdata = house_test)
# random forest
ml_rf_pred <- predict(ml_rf, newdata = house_test)

#Determine the prediction performance for each model against test criterion
postResample(ml_glm_pred,criterion_test)
postResample(ml_dt_pred,criterion_test)
postResample(ml_rf_pred,criterion_test)

# Exploring model tuning parameters
# dt with complexity parameter larger
# decision tree
ml2_dt <- train(form= price~.,
               data = house_train,
               method='rpart',
               trControl=ctrl_none,
               tuneGrid=expand.grid(cp=0.02))
summary(ml2_dt)
plot(as.party(ml2_dt$finalModel))
# info aboit the rpart
?rpart.control


# random forest

ml2_rf <- train(form= price~.,
               data=house_train,
               method='rf',
               trControl=ctrl_none,
               tuneGrid=expand.grid(mtry=5)) # set num features
predict_rf2 <- predict(ml2_rf)
ml_rf[1:10]
hist(predict_rf2)
postResample(predict_rf,criterion_train)

# how do the number of tree affects random forest?
ml3_rf <- train(form= price~.,
                data=house_train,
                method='rf',
                trControl=ctrl_none,
                ntree= 1000, # instead 500 by default
                tuneGrid=expand.grid(mtry=5)) # set num features
predict_rf3 <- predict(ml3_rf)
ml3_rf[1:10]
hist(predict_rf3)
postResample(predict_rf3,criterion_train)

# Challenges
#add a model just with three features, bedrooms, bathrooms and sqft_living
price_glm <- train(form=price ~ bedrooms + bathrooms + sqft_living,
                   data = house_train,
                   method= 'glm',
                   trControl=ctrl_none)
glm_fit <- predict(price_glm)

price_rpart <- train(form=price~bedrooms+bathrooms+sqft_living,
                     data = house_train,
                     method= 'rpart',
                     trControl=ctrl_none,
                     tuneGrid=expand.grid(cp=.01))
rpart_predfit <- predict(price_rpart)

price_rf <- train(form=price~bedrooms+bathrooms+sqft_living,
                  data = house_train,
                  method='rf',
                  trControl=ctrl_none,
                  tuneGrid=expand.grid(mtry=2))
rf_fit <- predict(price_rf)

# check goodness of fit
#regression
postResample(glm_fit, criterion_train)
# decision tree
postResample(rpart_predfit, criterion_train)
# random forest
postResample(rf_fit, criterion_train)
?postResample

# get predictions
# regression
glm_pred <- predict(price_glm,
                    newdata = house_test)
# decision tree
rpart_pred <- predict(price_rpart,
                      newdata = house_test)
# random forest
rf_pred <- predict(price_rf,
                   newdata = house_test)
# get goodness of fit for test data
postResample(glm_pred, criterion_test)
postResample(rpart_pred, criterion_test)
postResample(rf_pred, criterion_test)


# Repeat the modelling process, but now do make a classification. Predict whether or not a house sells for at least
# 1,000,000 dollars

# add a millon col that indicates whether or not a house sells for price 1,000,000 dollars
house_train <- house_train %>%
  mutate(million = factor(price>1000000))
house_train[1:20]

house_test <- house_test %>%
  mutate(million = factor(price>1000000))

# create classification model
million_glm <- train(form= million~. -price,
                     data = house_train,
                     method='glm',
                     trControl=ctrl_none)
glm_fit <- predict(million_glm)

million_rpart <- train(form=million~.-price,
                       data = house_train,
                       method='rpart',
                       trControl=ctrl_none,
                       tuneGrid=expand.grid(cp=.01))
rpart_predfit <- predict(million_rpart)

million_rf <- train(form=million~.-price,
                    data = house_train,
                    method='rf',
                    trControl=ctrl_none,
                    tuneGrid=expand.grid(mtry=2))
rf_fit <- predict(million_rf)

criterion_train <- house_train$million

# Get goodness of fit indices for training set
# Regression
confusionMatrix(data=glm_fit,
                reference = criterion_train)
# decision tree
confusionMatrix(rpart_predfit,criterion_train)
# random forest
confusionMatrix(rf_fit, criterion_train)

# Get predictions
criterion_test <- house_test$million

# regression
glm_pred <- predict(million_glm, house_test)
# decision tree
rpart_pred <- predict(million_rpart, house_test)
# random forest
rf_pred <- predict(million_rf, house_test)

# Goodness
# regression
confusionMatrix(glm_pred, criterion_test)
# decision tree
confusionMatrix(rpart_pred, criterion_test)
# random forest
confusionMatrix(rf_pred, criterion_test)

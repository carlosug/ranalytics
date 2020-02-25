# source: https://therbootcamp.github.io/ML_2019Oct/_sessions/Optimization/Optimization_practical.html

# Learning outcomes:
# use cross-validation to select optimal model tuning parameters for decision tree and random forest
# compare standard regression with lasso and ridge penalised regression
# use cross-validation to estimate future test accuracy
# basketball players observation from ISLR repository from 1986 1987.


# Basketball Salaries
# predict the Salary of players from hitter dataset

# A. Setup
library(tidyverse)
library(caret)
library(party)
library(partykit)
hitters_train <- read.delim('hitters_train.txt', header = TRUE, sep = ',')
hitters_test <- read.delim("hitters_test.txt", header = TRUE, sep = ',')
hitters_train <- hitters_train %>%
  mutate_if(is.character,factor)
hitters_test <- hitters_test %>%
  mutate_if(is.character, factor)

# B. Setup trainControl: setup training by specifying crtl_cv as 10-fold-cross-validation
ctrl_cv <- trainControl(method = 'cv',
                        number = 10)
# cross validation is a technique to evaluate predictive models by partitioning the original sample into a training set
# to train the model, and a test set to evaluate it.


# C. Regression (Standard)
salary_glm <- train(form=Salary~.,
                    data = hitters_train,
                    method= "glm",
                    trControl=ctrl_cv)
salary_glm$finalModel
coef(salary_glm$finalModel)
summary(salary_glm)

# D. Ridge Regression: fit a optimized regression model
# Ridge regression is a technique to analyze multiple regression data that suffer from multicoliniarity, the least squares
# estimates are with a large variance and are far from true value.
# Ridge and Lasso regression can be seen as Bayesian linear model.
# !! Ridge and Lasso regression to reduce model complexity and prevent over-fitting. !!
# Linear regression looks for minimizing the cost function = (Yreal - Yest)'2
# In Ridge regression cost function (regularized loss) is altered by adding a penalty equivalent to square 
# of the magnitude of the coefficient (= square regression weight betas)
# Ridge regression puts constraint of coeff (betas). Penalty term as lambda
# if the coefficient betas take large values, the optimization function is penalized
# Ridge regression helps to shrink coefficient and reduce the model complexity and multicolinearity


# Specify the lambda
lambda_vec <- 10 ^ (seq(-4,4,length=100))

# Ridge Regression ------------------------
salary_ridge <- train(form = Salary ~.,
                      data = hitters_train,
                      method="glmnet",
                      trControl=ctrl_cv,
                      preProcess=c("center","scale"), #standardise
                      tuneGrid= expand.grid(alpha=0,  # Ridge penalty
                                            lambda=lambda_vec))
salary_ridge
plot(salary_ridge)

# Print the best regularization parameter
salary_ridge$bestTune$lambda
summary(salary_glm)

# what are the coeff with the best lambda value
coef(salary_ridge$finalModel,
     salary_ridge$bestTune$lambda)

# The coefficient looks very unlike cause the preprocess step as standardized and centred data.

# E- LASSO REGRESSION
# Lasso can help in reducing over-fitting but also feature selection
# Default value of lambda is equal to 1
# specify which values of lambda penalty parameter. create a vector containing 100 values between 0 and 1.
lambda_vec <- 10^seq(from=-4, to=4, length=100)
salary_lasso <- train(form= Salary ~ .,
                      data = hitters_train,
                      method="glmnet",
                      preProcess=c("center","scale"),
                      tuneGrid=expand.grid(alpha=1, # Lasso Penalty
                                           lambda=lambda_vec))

salary_lasso
plot(salary_lasso)

# print the best lambda regularization parameter
salary_lasso$bestTune$lambda

# what was your final regression coef for best lambda value
coef(salary_lasso$finalModel,
     salary_lasso$bestTune$lambda)
summary(salary_glm)


# F - DECISION TREE
# It has a complexity parameter called cp
# cp < leads to complex tree // cp > leads to simple tree

# Determine the possible values of cp
cp_vec <- seq(from=0, to=1, length=100)
salary_rpart <- train(form= Salary ~.,
                      data = hitters_train,
                      method='rpart',
                      trControl=ctrl_cv,
                      tuneGrid=expand.grid(cp=cp_vec))
salary_rpart
plot(salary_rpart)
#print best value cp
salary_rpart$bestTune$cp
#visualise tree
plot(as.party(salary_rpart$finalModel))

# Random Forest
# random forest has diversity paramenter called mtry: this controls how many features are randomly considered
# at each split of the trees.

# tuneGrid settings: small mtry = 1 = diverse forest = less complex
                    # large mtry > 5 = similar forest = more complex


# Determine possible diversity parameter
mtry_vec <- 1:10

salary_rf <- train(form= Salary~.,
                   data = hitters_train,
                   method='rf',
                   trControl = ctrl_cv,
                   tuneGrid=expand.grid(mtry=mtry_vec))

salary_rf
plot(salary_rf)


# H - Estimate prediction accuracy from training folds
salary_resamples <- resamples(list(glm = salary_glm,
                                   dt = salary_rpart, 
                                   rf = salary_rf))
# Print a summary of cross-validation accuracy
summary(salary_resamples)


salary_resamples <- resamples(list(glm = salary_glm,
                                   ridge = salary_ridge, 
                                   lasso = salary_lasso, 
                                   dt = salary_rpart, 
                                   rf = salary_rf))


# I - Calculate prediction accuracy
# save salaries of players in test dataset
criterion_test <- hitters_test$Salary

# save predictions for glm, lasso, ridge and the others
glm_pred <- predict(salary_glm,hitters_test)
ridge_pred <- predict(salary_ridge, hitters_test)
lasso_pred <- predict(salary_lasso, hitters_test)
rpart_pred <- predict(salary_rpart, hitters_test)
rf_pred <- predict(salary_rf,hitters_test)

# use postsample to calculate agg prediction accuracy for each model
y <- hitters_test$Salary
postResample(glm_pred,y)
postResample(ridge_pred,y)
postResample(lasso_pred,y)
postResample(rpart_pred,y)
postResample(rf_pred,y)


# Z - Challenges:
# Repeat cross validation with repeated many times
# create training control ctrl_cv_rep
# train model again instead of ctrl_cv


# Repeat cross validation
# Folds = 10
# repeats = 5
ctrl_cv_rep <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 5)

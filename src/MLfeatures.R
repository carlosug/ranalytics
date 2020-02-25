#Source: https://therbootcamp.github.io/ML_2019Oct/_sessions/Features/Features_practical.html
# Feature Practical:
# 1. Understand the importance of the curse of dimensionality
# 2. know how to eliminate unwanted variables
# 3. Explore and use feature importance
# 4. use dimensionality reduction

# pima_diabetes is a subset of pimaIndiansDiabetes2 dataset in the mbench package. It contains demographic of Pima Indian
# murders_crime, violent_crimes, and nonviolent_crime dataset from Communities and Crime Unnormalized dataset: 


# load neccesary packages
library(tidyverse)
library(caret)



# Pima Indians diabetes
pima_diabetes <- read.delim("pima_diabetes.txt", header = TRUE, sep = ',')
# non-violent statistics dataset
violent_crime <- read.delim("violent_crime.txt", header = TRUE, sep = ',')
nonviolent_crime <- read.delim("nonviolent_crime.txt", header = TRUE, sep = ',')
# murder crime statstics
murders_crime <- read.delim("murders_crime.txt", header = TRUE, sep = ',')
summary(pima_diabetes)
names(pima_diabetes)

# B - Pima Indians diabetes
# explore feature selection for the Pima Indians diabetes
# Pima are the group of Native Americans living in Arizona
# A genetic predisposition to surive normally to a diet poor of carbonhydrates for years.
# Shift from traditional crops to proccessd foods, together with less pysical activity, made them develop the
# highest prevalence of type 2 diabetes


# Split data train and test dataset using createdDataPartition(). Select only 15% of cases for training set
# split index
train_index <- createDataPartition(pima_diabetes$diabetes, p=.15, list = FALSE) # select randomly samples from pima
train_index

# train and test sets
pima_train <- pima_diabetes %>% slice(train_index)
pima_test <- pima_diabetes %>% slice(-train_index)

# test criterion
criterion <- as.factor(pima_test$diabetes)

# remove unwanted features
# 1. split training data into a data frame holding the predictors,x and a vector holding the criterion,y

# 1. select predictors, x
pima_train_pred <- pima_train %>% select(-diabetes)
pima_train_crit <- pima_train %>% pull(diabetes)

# check correlations
corr_maxtrix <- cor(pima_train_pred)
# find exclusively correlated variables
findCorrelation(corr_maxtrix)

# check if any near zero variance features
nearZeroVar(pima_train_pred)

# Feature importance
# fit a regression model for featuring engine
pima_glm <- train(diabetes~.,
                  data=pima_train,
                  method= 'glm')

# Evaluate var importance with VarImp()
varimp_glm <- varImp(pima_glm, scale = TRUE)
plot(varimp_glm)


# Model comparison

#create model with only imp varaibles
pima_glm4 <- train(diabetes~glucose+mass+pregnant+pedigree,
                   data = pima_train,
                   method='glm')

# Predict criterion and evaluate with confusionmatrix
pima_glm_pred <- predict(pima_glm, newdata = pima_test)
pima_glm4_pred <- predict(pima_glm4, newdata = pima_test)

# evaluate the results
confusionMatrix(pima_glm_pred,criterion)
confusionMatrix(pima_glm4_pred, criterion)


# MURDERS: combine socioeconomic data from US 90', law enforcement data and crime data.
# the goal is to predict whether murders have been commited (murder is the criterion)
summary(murders_crime)

# splitting and set test criterion
train_index <- createDataPartition(murders_crime$murders, p = .25, list = FALSE)
murders_train <- murders_crime %>% slice(train_index)
murders_test <- murders_crime %>% slice(-train_index)
criterion <- as.factor(murders_test$murders)

# remove unwanted features
# before starting modeling, first split training data into predictors x and ys criterion
murders_train_pred <- murders_train %>% select(-murders)
murders_train_crit <- murders_train %>% select(murders) %>% pull()

# test correlations
corr_maxtrix <- cor(murders_train_pred)
findCorrelation(corr_maxtrix)

# remove excesivelly correlated features from training predictors
murders_train_pred <- murders_train_pred %>% select(-findCorrelation(corr_maxtrix))
names(murders_train_pred)

# find features near to zero variance
nearZeroVar(murders_train_pred)

# clean training set
murders_train_clean <- murders_train_pred %>%
  add_column(murders=murders_train_crit)

# Model Comparison
# fit glm
murders_glm <- train(murders~.,
                     data=murders_train,
                     method='glm')
murders_glm_clean <- train(murders~.,
                           data = murders_train_clean,
                           method='glm')

# evaluate performance. which set of features predict better?
murders_pred <- predict(murders_glm,newdata = murders_test)
murders_clean_pred <- predict(murders_glm_clean, newdata = murders_test)
confusionMatrix(murders_pred, criterion)
confusionMatrix(murders_clean_pred, criterion)


# Data Compression with PCA: given high correlation, compress with PCA
# fit glm with preproccessed features: add pca as preprocess and thresh = .8 as only retain features that capture
# 80% of the original variance
murders_glm_pca <- train(murders ~.,
                         data = murders_train,
                         method= 'glm',
                         preProcess=c('pca'),
                         trControl=trainControl(preProcOptions = list(thresh=0.8)))
# compare prediction performance with previous model
murders_pca <- predict(murders_glm_pca, newdata = murders_test)
# evaluate results
confusionMatrix(murders_pca,criterion)

# Feature Importance
# which feature is the most important to predict murder
varimp_glm <- varImp(murders_glm)
varimp_glm_clean <- varImp(murders_glm_clean)
varimp_pca <- varImp(murders_glm_pca)

varimp_glm
varimp_glm_clean
varimp_pca
plot(varimp_glm)


# Violent & non violent data
# predict the number of violent crimes per 100k inhabitants (violentCrimesPerPop) or non violent crimes
# those criteria are numeric, not classification problem

# another approach for feature selection is to try automatically select subset of features that lead to the best
# possible cross-validation performance with recursive feature elimination

# train split
train_index <- createDataPartition(violent_crime$ViolentCrimesPerPop,
                                   p=.8,
                                   list = FALSE)
# train and test set
violent_train <- violent_crime %>% slice(train_index)
violent_test <- violent_crime %>% slice(-train_index)

# remove extreme correlations
predictors <- violent_train %>% select(-ViolentCrimesPerPop)
predictors <- predictors %>% select(-findCorrelation(cor(predictors)))
violent_train_clean <- predictors %>%
  add_column(ViolentCrimesPerPop = violent_train$ViolentCrimesPerPop)

# Feature elimination settings
ctrl_rfe <- rfeControl(functions = lmFuncs, # linear model
                       method = 'cv',
                       verbose = FALSE,
                       rerank = FALSE)
# run feature elimination
profile <- rfe(x=violent_train %>% select(-ViolentCrimesPerPop),
               y=violent_train$ViolentCrimesPerPop,
               size = 1:(ncol(violent_train_clean)-1), # features set sizes
               rfeControl = ctrl_rfe)
plot(profile)

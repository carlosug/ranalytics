# this practical session will be for:
#1. fit a regression model to training data
#2. explore fit object with generic functions
#3. evaluate the model fitting performance using accuracy measures as MSE and MAE
#4. explore the effect of adding new features.

# Dataset colleague.csv contains colleague data such as US colleagues from 1995 


# load specific packages
install.packages("caret")
library(tidyverse)
library(caret)

# load data from repositary data folder
college_train <- read.csv(file = 'college_train.csv')
head(college_train)
dim(college_train)
names(college_train)
summary(college_train)
View(college_train)
#data cleaning, all character cols to factors
college_train <- college_train %>%
  mutate_if(is.character, factor)

#Determine sampling procedure
# set trainning resampling method to none to keep everything super simple
ctrl_none <- trainControl(method = 'none')

# fit regression model predicting graduate rate as function of phD feature
# criterion: grad.Rate
# features: PhD

Grad.Rate_glm <- train(form = Grad.Rate ~ PhD,
                       data = college_train,
                       method = "glm",
                       trControl = ctrl_none)
summary(Grad.Rate_glm)

# for every increase of one unit phD, the expected graduation rate will increase on .3301

# save the predict values
glm_fit <- predict(Grad.Rate_glm)

#plot distribution gml_fit
hist(glm_fit)

# evaluate accuracy
# define criterion vector as actual grad.rates
criterion <- college_train$Grad.Rate
# quantify model fitting results, regression fitting accuracy
postResample(pred = glm_fit, # fitted values
             obs = criterion) # criterion values
# MAE: Mean abs error; on average, how far how predicted points are from actual values?
# D- Evaluate Accuracy
# create two dataaset for acc level eval
# accuracy with raw absolute erros
# accuracy_agg as agg mean abs error

accuracy <- tibble(criterion=criterion,
                   Regression=glm_fit) %>%
  gather(model, prediction, -criterion) %>%
  #add error measures
  mutate(ae=abs(prediction-criterion))

accuracy_agg <- accuracy %>%
  group_by(model) %>%
  summarise(mae=mean(ae)) #calculate the MAE

head(accuracy)
# create scatterplot showing the relationship between true criterion values and model fits

# Plot A) Scatterplot of criterion versus predictions
ggplot(data = accuracy,
       aes(criterion, prediction))+
  geom_point(alpha=.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(title= 'Regression: One Feature',
       subtitle='Line indicates perfect performance',
       x='True Graduation Rate',
       y='Fitted Graduation Rate')+
       xlim(0,200)+
       ylim(0,200)

# create a violin plot showing the distribution of abs errors of the model
# plot B) Violin plot of abs errors
ggplot(data =accuracy,
       aes(x=model, y=ae, fill=model))+
  geom_violin() +
  geom_jitter(width = .05, alpha=0.2)+
  labs(title='Distr of Abs fitted erros',
       subtitle='num indicates means',
       x='Model',
       y='Abs error') +
  guides(fill=FALSE)+
  annotate(geom = 'label',
           x= accuracy_agg$model,
           y=accuracy_agg$mae,
           label= round(accuracy_agg$mae,2))
# on average the model fits are 12 away from the true criterion values but also quite variability

# add more features
# criterion: Grad
# features: phD, Room.Board, Terminal, S.F. Ratio
Grad.Rate_glm <- train(form= Grad.Rate ~ PhD + Room.Board + Terminal + S.F.Ratio,
                       data = college_train,
                       method= 'glm',
                       trControl= ctrl_none)
summary(Grad.Rate_glm)

# save prediction
glm_fit <- predict(Grad.Rate_glm)

# new model fitting accuracy
postResample(glm_fit, criterion)

# create two dataaset for acc level eval
# accuracy with raw absolute erros
# accuracy_agg as agg mean abs error

# create a scatterplot showing the relationship between new model fits and true values
accuracy <- tibble(criterion=criterion,
                   Regression=glm_fit) %>%
  gather(model, prediction, -criterion) %>%
  #add error measures
  mutate(ae=abs(prediction-criterion))

accuracy_agg <- accuracy %>%
  group_by(model) %>%
  summarise(mae=mean(ae)) #calculate the MAE

head(accuracy_agg)
# create scatterplot showing the relationship between true criterion values and model fits

# Plot A) Scatterplot of criterion versus predictions
ggplot(data = accuracy,
       aes(x = criterion, y = prediction)) +
  geom_point(alpha = .2) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Regression: Four Features",
       subtitle = "Line indicates perfect performance",
       x = "True Graduation Rates",
       y = "Fitted Graduation Rates") +
  xlim(0, 120) + 
  ylim(0, 120)

# create a violin plot showing the distribution of abs errors of the model
# plot B) Violin plot of abs errors
ggplot(data = accuracy, 
       aes(x = model, y = ae, fill = model)) + 
  geom_violin() + 
  geom_jitter(width = .05, alpha = .2) +
  labs(title = "Distributions of Fitting Absolute Errors",
       subtitle = "Numbers indicate means",
       x = "Model",
       y = "Absolute Error") +
  guides(fill = FALSE) +
  annotate(geom = "label", 
           x = accuracy_agg$model, 
           y = accuracy_agg$mae, 
           label = round(accuracy_agg$mae, 2))

# F. Use all the features
Grad.Rate_glm <- train(form=Grad.Rate~ .,
                       data = college_train,
                       method='glm',
                       trControl=ctrl_none)
summary(Grad.Rate_glm)
#save prditions
glm_fit <- predict(Grad.Rate_glm)
# new model fitting
postResample(glm_fit,criterion)


## CLASSIFICATION TASK: as predicting a category, not a continuous number. 
# Predict whether or not a college
# is Private or Public
# look at the class of variable
# make sure the criterion is a factor
class(college_train$Private)
# criterion
criterion <- college_train$Private
# fit a classification model
Private_glm <- train(form=Private  ~ .,
                     data = college_train,
                     method='glm',
                     trControl=ctrl_none)
summary(Private_glm)

# Access classification model accuracy
#get fitted values
glm_fit <- predict(Private_glm)
plot(glm_fit)

# show acc of glm fit model
confusionMatrix(glm_fit,criterion)

# visualise the accuracy level of classification models

# Get overall accuracy from regression model
glm_accuracy <- confusionMatrix(data =  glm_fit,  
                                reference = criterion)$overall[1]

# Combine results into one table
accuracy <- tibble(Regression = glm_accuracy) %>%
  gather(model, accuracy)
# Sensitivity: of those colleague that truly are private, the model fits are correct 89%
# Plot the results!
ggplot(accuracy, aes(x = model, y = accuracy, fill = model)) + 
  geom_bar(stat = "identity") +
  labs(title = "Is a college private or public?",
       subtitle = "Fitting classification accuracy",
       y = "Overall Accuracy") +
  ylim(c(0, 1)) +
  annotate(geom = "label", 
           x = accuracy$model, 
           y = accuracy$accuracy, 
           label = round(accuracy$accuracy, 2))

### CHALLENGES
# conduct a regression analysis predicting the % alumni who donate to the college

mod <- train(form=perc.alumni ~ .,
             data = college_train,
             method='glm',
             trControl=ctrl_none)
summary(mod)
mod_predictions <- predict(mod)
hist(mod_predictions)
postResample(mod_predictions,college_train$perc.alumni)

# conduct a classification analysis predicting whether or not a school is hot (at least 10000 applications)
college_train <- college_train %>%
  mutate(hot=factor(Apps>10000))
mod_hot <- train(form = hot ~ ., 
                 data = college_train,
                 method = "glm",
                 trControl = ctrl_none)
summary(mod_hot)
mod_predictions <- predict(mod_hot)
plot(mod_predictions)
# solve issue max num iterations
mod_hot <- train(form = hot ~ ., 
                 data = college_train,
                 method = "glm",
                 trControl = ctrl_none,
                 control = list(maxit = 75))

summary(mod_hot)
# get correlation matrix of numeric variables
cor(college_train[,sapply(college_train, is.numeric)])

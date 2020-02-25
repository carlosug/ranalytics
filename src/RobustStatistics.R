### SOURCE ### -- https://therbootcamp.github.io/SwR_2019Apr/_sessions/RobustStats/RobustStats_practical.html

### OVERVIEW ###
# Explore issues sourrounding robust statistics using sales avocados in EEUU from 2015 to 2018
# 1. How to evaluate regressions
# 2. Run non parametric tests
# 3. Run bootstrap analyses

### TASKS ###
# Setup: load neccesary package for this script
library(tidyverse)
library(lubridate)
library(lme4)
install.packages("rsq")
library(rsq)
install.packages("moments")
library(moments)
library(boot)
install.packages("quantreg")
library(quantreg)
install.packages("Rfit")
library(Rfit)
# load csv from local data folder
avocado <- read.csv(file = "Inputs/avocado.csv")
avocado_cali <- read.csv(file = "Inputs/avocado_cali.csv")
# take a look few rows from dataset and print it on the console
summary(avocado)
summary(avocado_cali)

# Assumptions: data set containing different prices of avocados per EEUU regions over time. The initial
# - goal is to predict, whether sales volume is function of price avocado
# regress volume on average price
m1 <- lm(formula = volume ~ price_per_avocado, data = avocado_cali)
summary(m1)
# according with output, price strongly predict volume sales
# create plot to visualise the relationship between variables
plot(avocado_cali$price_per_avocado, avocado_cali$volume)
abline(m1)
# the plot revealed regression is not good. Confirm it by plotting residuals against predicted values
# this model fit badly with linear regression: violation assumptions as linearity, normality and 
# - homoscedascity. The cause of this violation is missing variables
m2 <- lm(formula = volume ~ price_per_avocado + type, data = avocado_cali)
plot(predict(m2), residuals(m2))
# compare correlation of predicted values & residuals 
cor(predict(m1), residuals(m1))
cor(predict(m2), residuals(m2))
# problem is the skewed distribution of volume
hist(avocado_cali$volume)
# check another variable 
library(e1071)
hist(avocado_cali$volume_index)
skewness(avocado_cali$volume)
skewness(avocado_cali$volume_index)
# regress volume_index on average price and type
m3 <- lm(volume_index ~ price_per_avocado + type, data = avocado_cali )
plot(predict(m3), residuals(m3))
summary(m3)

## VARIANCE INFLACTION
m4 <- lm(volume_index ~ price_per_avocado + type + as.numeric(date) + season + temperature + humidity + precipitation,
         data = avocado_cali)
# compare m3 and m4: is the effect of price still significant? how the estimate changed and std. error?
summary(m3)
summary(m4)
# check the residual variance. Variance Inflaction factor: how much variance increased cuz the colinearity
var_res1 <- var(residuals(m3))
var_res2 <- var(residuals(m4))
var_res1
var_res2

# predit price avocado
m_price1 <- lm(price_per_avocado  ~ type, avocado_cali)
m_price2 <- lm(price_per_avocado ~ type + date + season + temperature + humidity + precipitation, avocado_cali)
# calculate the R square and VIF
rsq1 <-rsq(m_price1)
rsq2 <-rsq(m_price2)
rsq1
rsq2
# VIF
vif1 <- 1/(1-rsq1)
vif1
vif2 <- 1/(1-rsq2)
vif2

# calculate standard errors
var_x = var(avocado_cali$price_per_avocado)
n <- nrow(avocado_cali)

# residual variance and std errors for m3
var_beta1 <- (var_res1/(var_x* (n-1))) * vif1
sqrt(var_beta1)

# residual variance and std. error for m4
var_beta2 <- (var_res2/(var_x*(n-1))) * vif2
sqrt(var_beta2)

# .......now with corrections
var_cor_res1 <- sum(residuals(m3)**2)/(n-2-1)
var_beta1c <- (var_cor_res1/(var_x*(n-1)))*vif1
sqrt(var_beta1)
# variance and standard error of price in m4 with correction
var_cor_res2 <- sum(residuals(m4)**2) / (n - 9 - 1)
var_beta2c <- (var_cor_res2 / (var_x * (n-1))) * vif2 
sqrt(var_beta2)

# the above illustrates the double-edged sword: adding new variables improve the overall ability pred
# of the model, however it also increase our uncertainly of the true values of the regression weights
# by increasing the standard error and variance.

#### POLYNOMINAL EFFECTS
# so far it assumes linear relationship. it seems plausible effect of price can be quadratic or polynominal
# linear and quadratic effect of price
m5 <- lm(volume_index ~ type + price_per_avocado + I(price_per_avocado^2), avocado_cali) 
summary(m5)

# test whether the inclusion of quadratic effect by testing m5 and m3 using anova
anova(m3,m5)

# yet, the quadratic effect improve the model, substantial downside; standard error of linear effect increases
#- Due to high correlation of each variable. To prevent this, quadratic terms are typically centrered 

m6 <- lm(volume_index ~ type + price_per_avocado + I((price_per_avocado - mean(price_per_avocado))^2),
avocado_cali)
summary(m6)
## could be improved by adding cubic effect

#### NON PARAMETRIC TESTS: look at the development of avocado consumption over time.

# Cali avocado volume 2016 & 2017
avocado_cali_2016 <- avocado_cali %>% 
  filter(year == 2016)

avocado_cali_2017 <- avocado_cali %>% 
  filter(year == 2017, 
         week(date) %in% week(avocado_cali_2016$date))

# now check consumption has raised 2016 -2017 with ttest
t.test(avocado_cali_2016$volume,
       avocado_cali_2017$volume, paired = TRUE)
# compare Cali avocado volume_index 2016 & 2017
t.test(avocado_cali_2016$volume_index,
       avocado_cali_2017$volume_index, paired = TRUE)

# this differences may be due to skewness of volume data. test Wilcoxon

wilcox.test(avocado_cali_2016$volume,avocado_cali_2017$volume, paired = TRUE)
wilcox.test(avocado_cali_2016$volume_index,avocado_cali_2017$volume_index, paired = TRUE)

# test robustly with sign test
#sign test preparations
signs= avocado_cali_2017$volume > avocado_cali_2016$volume
n_plus = sum(signs)
n = length(signs)

# sign test: p of receiving a larger n_plus than observed
pbinom(n_plus, size = n, prob = .5)

#####ROBUST REGRESSION#####



#quantile regression
m3_q <- rq(formula = volume_index ~ price_per_avocado + type, data = avocado_cali)

#rank-based regression
m3_rb <- rfit(formula = volume_index ~ price_per_avocado + type, 
              data = avocado_cali)

##### BOOSTRAPPING: need to specify the function that calculates the statistic
# 1. boostrap function for linear regression
boot_lm <- function(data, indices){
  data <- data[indices,] # select obs. in bootstrap sample
  m <- lm(volume_index ~ price_per_avocado + type,
          data = data)
  coefficients(m)
}

# 2. run the bootstrap mechanism using boot function from boot package
# bootstrap sampling results
library(boot)
B <- boot(data = avocado_cali,
          statistic = boot_lm,
          R = 100)
# Output explanation: col ORIGINAL = reg model // col BIAS = avg simulated parameter - original param
# - stand. error are std error of the bootstrap sampling results.
B
m3
#### CREATE Mixted linear models
mm <- lmer(volume_index ~ price_per_avocado + type + (1|region), avocado)
mm
plot(predict(mm), residuals(mm))

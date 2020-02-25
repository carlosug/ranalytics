### NEW STATISTICS
# Source: https://therbootcamp.github.io/SwR_2019Apr/_sessions/NewStats/NewStats_practical.html
### OVERVIEW ####
## By the end of this session you will know:
# 1. P-hack
# 2. Determine appropriate sample size
# 3. compute confiance intervals
# 4. Run simple Bayesian analyses

# Install packages, libraries and functions
library(tidyverse)
install.packages("pwr")
library(pwr)
install.packages("rstanarm")
install.packages("stats")
library(rstanarm)
library(stats)
install.packages("BayesFactor")
library(BayesFactor)

# import data from local folder
ps1 <- read.csv(file= 'Inputs/psi_exp1.csv')
ps2 <- read.csv(file= 'Inputs/psi_exp2.csv')
summary(ps1)
View(ps1)
View(ps2)

# C. Power Analysis
# Bem's first analysis is whether the hit rate for erotic pics is on average larger than 50%. We obseved
# - on average proportion 53%. What does it mean in term of Cohen's effect size, which is calculated
# - dividing average deviance from H0

# extract erotic hit rate
hit_rate_erotic <- ps1$hit_rate[ps1$condition == 'erotic']
# calculate the deviance from H0
hit_rate_erotic_delta <- hit_rate_erotic - 50
# calculate d
d <- mean(hit_rate_erotic_delta)/sd(hit_rate_erotic)
d
# d =.2 small but meaninfull effect. We would like to conduct experiment with low false positives (alpha <0.5
# and equally low chance of missing effect beta = 0.05 implying 1 - beta = 0.95. How many obs we have to make
# in order to conduct such test?

# N to test d = .25, alpha = 0.05, power = 0.95
pa <- pwr.t.test(d=.2513,sig.level = .05, power = 0.95, alternative = "greater")
pa
# shows 346 individuals

# sample size plot
plot.power.htest(pa)

# how large the power (i.e. prob detect an effect given that is truly there to detect) Bem study were.
# Ben post-hoc power
pa_bem <- pwr.t.test(n = length(hit_rate_erotic),
                     d= .25,
                     sig.level = 0.05,
                     alternative = 'greater')
pa_bem

# with d = 0.1, what large Bens power have been? how large sample size was neccesary for power = .95
pa_bem_1 <- pwr.t.test(n = length(hit_rate_erotic),
                       d= 0.1,
                       sig.level = 0.05,
                       alternative = 'greater')
pa_bem_1

# sample size
pa.1 <- pwr.t.test(d = 0.1,
                   power = 0.95,
                   sig.level = 0.05,
                   alternative = 'greater')
pa.1


#### D. CONFIANCE INTERVALS

# extract hit rates
hit_rate_erotic <- ps1$hit_rate[ps1$condition == 'erotic']
hit_rate_neutral <- ps1$hit_rate[ps1$condition !='erotic']

# compute t-test
test <- t.test(hit_rate_erotic,hit_rate_neutral, paired = TRUE)
names(test)
test$conf.int

# Recreate confiance interval

# average diff between groups
hit_rate_diff <- hit_rate_erotic - hit_rate_neutral

# mean differences
hit_rate_diff_mean <- mean(hit_rate_diff)

# differences
hit_rate_diff_se <- sd(hit_rate_diff/sqrt(length(hit_rate_diff)))

# determine t values for p = 2.5% and p = 97.5%
t.25 <- qt(p = .025, df= length(hit_rate_diff)-1)
t97.5 <- qt(p = 0.975, df=length(hit_rate_diff)-1) 
# construct confiance interval
hit_rate_lowerb <- hit_rate_diff_mean + t.25*hit_rate_diff_se
hit_rate_upperb <- hit_rate_diff_mean + t97.5*hit_rate_diff_se

###### Advanced: Bayesian statistics ########
# bayesian regression predicting hit_rate by gender and condition
bm1 <- stan_glm(hit_rate ~ gender + condition, data = ps1)
bm1
posterior_interval(bm1)

# run same analysis using BayerFactor using other package
ps1$gender_dummy <- as.numeric(ps1$gender == 'Male')
ps1$condition_dummy <- as.numeric(ps1$condition =='erotic')
bm2 <- regressionBF(hit_rate ~ gender_dummy+condition_dummy,data = ps1)
bm2

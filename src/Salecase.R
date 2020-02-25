# In this case study we perform data wrangling and merge the two datasets merged in one retailer_sales.csv
# how large the fluctuation of sales were per store
# whether sales number go up or down in holidays
# how well we can predict sales numbers from other variables, time series analysis predict future turnovers
# Because there are many timepoints per store, we aggregate the values to interpret p-values using mixed effect models.
# check by my own |lmer| function from lmer4 package to learn about mixed effect methods.
# tasks: load data, wrangle data, run regressions.
# source: https://therbootcamp.github.io/BaselRBootcamp_2018July/_sessions/CaseStudies/Sales_Data_Case_Study_Answers.html

# Install necessary packages and functions
install.packages("tidyverse")
install.packages("lme4")
install.packages("ggplot2")
library(tidyverse)
library(lme4)
library(magrittr)
library(dplyr)
library(ggplot2)
# load data from local dataset to R project
sales <- read.csv("inputs/sales.csv")
stores <- read.csv("inputs/stores.csv")

# Explore the data
summary(sales)
head(stores)


# Filter varaibles of interest, Store, Assortment, CompetitionDistance, CompetitionOpenSinceYear, Promo2
stores <- stores %>%
  select(Store, Assortment, CompetitionDistance, CompetitionOpenSinceYear, Promo2) %>%
  mutate(Assortment = case_when(Assortment == "a" ~ "basic",
                                Assortment == "b" ~ "extra",
                                Assortment == "c" ~ "extended"))

# Join the store data with sales data
sales <- left_join(sales, stores, by = "Store")

#the sales file contains an error in state_holiday varaible. there are NA where there should be 1
sales <- sales %>%
    mutate(StateHoliday = case_when(is.na(StateHoliday) ~ 1,
                                    TRUE ~ 0))

# Rename the variables to be lowercase and with underscore between words. 
names(sales) <- c("store", "week_day", "date", "sales", "customers", "open",
                  "promo", "state_holiday", "school_holiday", "assortment",
                  "competition_distance", "competitiom_open_since", "store_promo")

# Save the prepared data and as "retailer_sales.csv"
write.csv(sales, "outputs/retailer_sales.csv")

# Statistics - Fluctuation over days
# to get an average of fluctuation over days, get a subsample of a few stores. We plot individual trajectories
# and add a mean line. We can also get a repeated measures test, to have a statistical test of the stability
# correlation between points or agg sales data of store for each time point and run a regression
# !! in both methods is violated the assumption of independence of the data, we cant interpret p-values


# visual impression of how the fluctuation are

# first create a variable called "days" that is the counter for the number of days and will be easier to use
# than the variable date.

store_ids <- unique(sales$store)
sales$days <- 0

for (i in store_ids){
  sales$days[sales$store == i] <- seq_len(sum(sales$store == i))
}

# take a subsample to plot
sales_sub <- sales[sales$store %in% sample(1:1115, 30),]

# get rid of dates where the store were close
sales_sub <- filter(sales_sub, sales >0)

# create a plot using ggplot

ggplot(sales_sub, aes(x = days, y = sales)) +
  geom_line(aes(group = store), col = 'grey', alpha= .4) +
  stat_smooth(lwd = 1.5)+ # add an average line
  theme_bw()

# correlation between two of the timepoints
r_ds <- sample(sales$date, 2)
cor(sales$sales[sales$days==r_ds[1]], sales$sales[sales$days == r_ds[2]])

# summarize the sales data over date (for each day take the mean) and store it in an object called sales_agg
# then run a regression lm()

# first summarize the data
sales_agg <- sales %>%
  group_by(date) %>%
  summarise(
    sales = mean(sales)
  )

# run the regression
mod <- lm(sales ~ as.numeric(date), data = sales_agg)
summary(mod)

# compute the coefficent of variation (i.e. std dv scaled on the mean (sd/mean) of each store turnovers (sales variable))
sales_cv <- sales %>%
  filter(open != 0) %>%
  group_by(store) %>%
  summarise(
    cv=sd(sales)/mean(sales)
  )

# plot the distribution of the coeff of variation:

ggplot(sales_cv, aes(cv))+
  geom_histogram() + # function to create a histogram
  xlim(c(0,1))+
  xlab("Coefficient of Variation") +
  ylab("Frequency") +
  theme_bw()

# Statistics
#to test the effect of state holidays, filter dataset to only include date on which the given score was open.
# Then average the sales of the holidays and the non holidays for each store

# aggregate data to do the statistical test
sales_agg <- sales %>%
  filter(open != 0) %>%
  group_by(store, state_holiday) %>%
  summarise(
    sales = mean(sales)
  )

#check means
tapply(sales_agg$sales, sales_agg$state_holiday, mean)

# get rid of stores that weren't open on any state holiday
sales_agg <- sales_agg %>%
  filter(store %in% store[state_holiday == 1])

# check the means again
tapply(sales_agg$sales, sales_agg$state_holiday, mean)

# run a paired t.test
t.test(sales ~ state_holiday,
       data = sales_agg,
       paired = TRUE)

# REPEAT FOR SCHOOL HOLIDAYS

# aggregate data to do statistical test
sales_agg <- sales %>%
  filter(open != 0) %>%
  group_by(store, school_holiday) %>%
  summarise(
    sales = mean(sales)
  )
tapply(sales_agg$sales, sales_agg$school_holiday, mean)

# get rid of stores that werent open on any state holidays
sales_agg <- sales_agg %>%
  filter(store %in% store[school_holiday==1])

# check the mean again
tapply(sales_agg$sales, sales_agg$school_holiday, mean)

# run a paired t-test (compare the mean bewtween holidays, no holidays sales)
t.test(sales ~ school_holiday,
       data = sales_agg,
       paired = TRUE)

# Predict the number of sales from other variables
# we need to aggregate the sales over time per store and period

# 1. agg data per store, customers, store promo, competition distance and assortment

sales_agg <- sales %>%
  filter(open != 0) %>%
  group_by(store, customers, store_promo, competition_distance, assortment) %>%
  summarise(
    sales = mean(sales)
  )

# run a regression to test the influence of the variables
mod <- lm(sales ~ ., data = sales_agg)
summary(mod)

# Linear mixed effect model: because we have repeated measures in the data, we use mixed effects models
#  where we can account for the dependece structure of the data with random effects.
library(lme4)

# same prediction as before but not taking the agg value. Add the date as a random effect

sales_open <- sales %>%
  filter(open != 0)

me_mod <- lmer(sales ~ customers + store_promo + competition_distance + assortment + (1 | store),
               data = sales_open)

summary(me_mod)

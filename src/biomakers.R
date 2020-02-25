# Source: https://therbootcamp.github.io/R4DS_2019Feb/_sessions/CaseStudies/Clinical_Data_Case_Study.html
library("tidyverse")
library("broom")

trial_df = read.table("dimarta_trial.csv", header= TRUE, sep = ";")
demographics_df= read.csv("dimarta_demographics.csv", header = TRUE, sep = ";")
biomaker_df= read.csv("dimarta_biomarker.csv", header = TRUE, sep = ";")

# check the data sets
head(trial_df)
summary(trial_df)
summary(demographics_df)
summary(biomaker_df)

# data wrangling
trial_df <- trial_df %>%
  rename(StudyArm=arm)
# using the table function to change I, II and III to placebo, adiclax y dimarta
trial_df <- trial_df %>%
  mutate(StudyArm = case_when(
    StudyArm == "I" ~ "placebo",
    StudyArm == "II" ~ "adiclax",
    StudyArm == "III" ~ "dimarta"
  ))

demographics_df <- demographics_df %>%
  mutate(gender_c = case_when(
    gender == 0 ~ "male",
    gender == 1 ~ "female"
  ))

# create new object that combine trial + demographics
dimarta_df <- trial_df %>%
  left_join(demographics_df)

# convert biomaker_df to a wide format using spread
biomaker_wide_df <- biomaker_df %>%
  spread(Biomarker, BiomarkerStatus)


dimarta_df <- dimarta_df %>%
  left_join(biomaker_wide_df)

dimarta_df
head(dimarta_df)
summary(dimarta_df)
mean(dimarta_df$age)
# outliers 
dimarta_df <- dimarta_df %>%
  filter(!is.na(histamine_change),
          histamine_start < 200,
         histamine_start > - 20)
)

# create a table showing how many males and females were in the trial
dimarta_df %>%
  group_by(dimarta_df$gender_c) %>%
  summarise(
    Counts = n()
  )

dimarta_df %>%
  group_by(dimarta_df$StudyArm) %>%
  summarise(
    Counts = n()
  )
# create a table that shows how many men/women were assigned to each study arm.

dimarta_df %>%
  group_by(StudyArm, gender_c) %>%
  mutate(Counts = n())

# add a column that shows the change of histamine levels from the start to the end of the trial
dimarta_df <- dimarta_df %>%
  mutate(
    histamine_change = histamine_end - histamine_start
  )

# same but with qol

dimarta_df <- dimarta_df %>%
  mutate(
    qol_change = qol_end-qol_start)
  )

dimarta_df %>% select(qol_change)

# calculate percentage of patient who tested positive for each of the three biomakers
dimarta_df %>%
  summarise(
    dw_mean = mean(dw),
    ms_percentage = mean(ms),
    np_percentage = mean(np)
  )

# were there different distributions of age in different trial sites?
# separately calculate mean and std. dv of patient age

dimarta_df %>%
  group_by(site) %>%
  summarise(
    age_mean = mean(age),
    age_stdv = sd(age)
  )

# Calculate the mean change in histamine for each site
dimarta_df %>%
  group_by(site) %>%
  summarise(
    mean_change = mean(histamine_change, na.rm = TRUE)
  )
dimarta_df$histamine_change

# Calculate the mean change in histamine results for each study arm

dimarta_df %>%
  group_by(StudyArm) %>%
  summarise(
    mean_change = mean(histamine_change, na.rm = TRUE)
  )
summary(dimarta_df)

# create boxplot study arms and histamine change
ggplot(data = dimarta_df,
       mapping = aes(x= StudyArm,
                     y= histamine_change)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  labs(title= "Histamine change",
       subtitle = "Dimarta",
       caption = "I love R")

# same boxplot but analysing gender

ggplot(data = dimarta_df,
       mapping = aes(x=factor(gender),
                     y=histamine_change)) +
  geom_boxplot() +
  geom_jitter(width = .1)+
  labs(title="histamine change",
       subtitle = "Dimarta",
       x= "Gender")
# create a plot analysing both gender and study arms

ggplot(data = dimarta_df,
       mapping = aes(x = StudyArm, 
                     y = histamine_change,
                     col = factor(gender))) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  labs(title = "Histamine change",
       subtitle = "Dimarta",
       caption = "I love R!",
       x = "Gender")

# is there any correlation between patient starting and hending levels? create a scatter plot with a regression line
ggplot(data = dimarta_df,
       aes(x = histamine_start, 
           y = histamine_end)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Histamine start and end correlation",
       x = "Histamine Start", 
       y = "Histamine End") +
  theme_minimal()
# create boxplot but different colours per study arms
ggplot(data=dimarta_df,
       aes(x=histamine_start,
           y=histamine_end,
           col=StudyArm))+
  geom_point() +
  geom_smooth(col="black") +
  labs(title="Histamine start and end correlation",
  caption = "Dimarta")

# create another plot using facet_wrap() to have different studies in different plot panels

ggplot(data = dimarta_df,
       aes(x=histamine_start,
           y=histamine_end)) +
  geom_point() +
  geom_smooth(col="black")+
  facet_wrap(~ StudyArm)+
  labs(title="Histamine start and end correlation",
       caption = "Dimarta")


#D-Statistics
# create a regression model predicting final histamine levels as a function of all variables that make clinical sense
full_glm <- glm(formula = histamine_end ~ age + StudyArm + gender + site + histamine_start,
                data = dimarta_df)
summary(full_glm)
tidy(full_glm)
library(broom)
tidy(full_glm)
names(full_glm)
# add the residuals from this model as a new col called residuals_full
dimarta_df <- dimarta_df %>%
  mutate(residuals_full = full_glm$residuals)
ggplot(full_glm,
       aes(x = abs(full_glm$residuals))) +
  geom_histogram()

# what is the mean value of the absolute values of the residuals?

dimarta_df %>%
  summarise(
    residuals_abs_full_mean = mean(abs(full_glm$residuals))
  )

# create a regression model predicting histamine change based only on the study arm. Then add if you can the residuals to the dataframe

arm_glm <- glm(formula = histamine_end ~ StudyArm, data = dimarta_df)

# calculate the mean of the raw and absolute residuals from arm_glm model

mean(arm_glm$residuals)
mean(abs(arm_glm$residuals))
sum(arm_glm$residuals)


# X - Challenges

# was there a correlation between change in qol and histamine change? answer this by creating the appropriate summary statistics, visualisation and hypthesis test
cor.test(formula= ~ histamine_change + qol_change,
         data = dimarta_df)

# p values of the test is 0.022 which is greater than significance level of 0.05, we can conclude that the correlation
# is not statistically significant

# there appears to be a slight negative correlation between histamine change and qol
ggplot(data = dimarta_df,
       aes(x= histamine_change, y=qol_change))+
  geom_point() +
  labs(x= "histamine change",
       y= "qol",
       title="qol and histamine change")+
  theme_minimal() +
  geom_smooth(method = "lm")

# Did the patients in a late disease state tend to responde better to dimarta compared to patients in Early disease state?

dimarta_df %>%
  filter(diseasestatus %in% c("Early", "Late") & StudyArm == "dimarta") %>%
  group_by(diseasestatus) %>%
  summarise(histamine_change_mean = mean(histamine_end-histamine_start))

ggplot(data = dimarta_df %>% 
         filter(diseasestatus %in% c("Early", "Late") & StudyArm == "dimarta"), 
       aes(x = diseasestatus, y = histamine_change)) +
  geom_violin() +
  stat_summary(geom = "point", 
               fun.y = "mean", 
               col = "red", size = 4) +
  labs(title = "Histamine change and disease status",
       subtitle = "Patients given Dimarta only",
       y = "Histamine Change",
       x = "Disease Status")

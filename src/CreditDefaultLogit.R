database <- readRDS("loan_data_ch1.rds")
tail(database[1:8], n =4)
install.packages("gmodels")
library(gmodels)
CrossTable(database$loan_status)
CrossTable(database$home_ownership)
CrossTable(database$home_ownership, database$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(database$grade, database$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
install.packages("xts")
library(xts)
data_locf <- na.locf(database)
head(data_locf[1:6], n = 5) # last observation carry out forward
data_nocb <- na.locf(database, fromLast = TRUE)
head(data_nocb[1:6], n = 5) # next observation carried backwards


database$int_bin <- rep(NA, length(database$int_rate))

database$int_bin[which(database$int_rate <= 7)] <- "0-7"
database$int_bin[which(database$int_rate > 7 & database$int_rate <= 9)] <- "7-9"
database$int_bin[which(database$int_rate > 9 & database$int_rate <= 11)] <- "9-11"
database$int_bin[which(database$int_rate > 11 & database$int_rate <= 13.5)] <- "11-13.5"
database$int_bin[which(database$int_rate > 13.5)] <- "13.5+"
database$int_bin[which(is.na(database$int_rate))] <- "Missing"
database$int_bin <- as.factor(database$int_bin)
plot(database$int_bin)
database$int_rate <- NULL
database$emp_length <- NULL
summary(database)
plot(database$age, ylab = "age")
# Save the Position of all Ages above 100
index_highage <- which(database$age > 100)
# Delete every Row with Ages above 100
database <- database[-index_highage, ]
max(database$age)
index_highincome <- which(database$annual_inc > 1000000)
database <- database[-index_highincome, ]
max(database$annual_inc)
# Create the training-set, that contains the first 20000 customers
training <- database[1:20000,]
test <- database[20000:29092,]
model_age <- glm(loan_status ~ age, family = "binomial", data = training)
model_age
coefficient <- model_age$coefficients   # age coefficient
e <- exp(1)  # e equals the exponantional function from 1, do you remember from school math?
Percentage <- e^coefficient
Percentage
model_all <- glm(loan_status ~ ., family = "binomial", data = training)
summary(model_all)
Final_Model <- glm(loan_status ~ loan_amnt + grade + annual_inc + int_bin, family = "binomial", data = training)
model_age$coefficients
DefaultProp <- 1/(exp(-(-1.905870516+0*(-0.009138289))))
DefaultPropperson1 <- 1/(exp(-(-1.905870516+25*(-0.009138289))))
DefaultProp_Person2_Formula <- 1 / (exp(-(-1.905870516+26*(-0.009138289))))
DefaultProp_Person2_Formula
test[1,]
prediction_1st <- predict(Final_Model, newdata=test[1,], type = "response")
prediction_1st
prediction_all <- predict(Final_Model, newdata=test,type = "response")
cutoff <- 1.00
pred_cutoff_0 <- ifelse(prediction_all > cutoff,1,0)
table(test$loan_status, pred_cutoff_0)
cutoff <- 0.05
pred_cutoff_5 <- ifelse(prediction_all > cutoff,1,0)
table(test$loan_status,pred_cutoff_5)

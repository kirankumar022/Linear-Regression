library(readr)
library(readxl)
calorie=read_csv("E:\\Assignments\\Assignment week 11\\linear\\Assignment\\calories_consumed.csv")
View(calorie)
summary(calorie)
attach(calorie)
#Data visualization
plot(calorie$`Calories Consumed`,calorie$`Weight gained (grams)`)
boxplot(calorie$`Weight gained (grams)`,horizontal = TRUE)
boxplot(calorie$`Calories Consumed`,horizontal = TRUE)
qqnorm(calorie$`Weight gained (grams)`)
qqline(calorie$`Weight gained (grams)`)
qqnorm(calorie$`Calories Consumed`)
qqline(calorie$`Calories Consumed`)
trans=log(calorie$`Weight gained (grams)`)
colnames(calorie)=cbind("Weight gained (grams)","Calories Consumed")
hist(calorie$`Weight gained (grams)`)
hist(calorie$`Calories Consumed`)
attach(calorie)
#normal model
cor(`Weight gained (grams)`,`Calories Consumed`)
cov(`Weight gained (grams)`,`Calories Consumed`)
library(Hmisc)
reg=lm(`Weight gained (grams)`~`Calories Consumed`,data=calorie)
reg
confint(reg, level = 0.95)
pred <- predict(reg, interval = "confidence")
pred <- as.data.frame(pred)
pred
library(ggplot2)
ggplot(data = calorie, aes(x = `Weight gained (grams)`, y = `Calories Consumed`)) + geom_point(color = 'blue') +geom_line(color = 'red', data = calorie, aes(x = `Weight gained (grams)`, y = pred$fit))
cor(pred$fit, calorie$`Calories Consumed`)
rmse <- sqrt(mean(reg$residuals^2))
rmse
#log of calories consumed
plot(`Weight gained (grams)`,log(`Calories Consumed`))
reg_log=lm(`Weight gained (grams)`~log(`Calories Consumed`),data=calorie)
reg_log
confint(reg_log,level = 0.95)
pred1<- predict(reg_log, interval = "confidence")
pred1
pred1<- as.data.frame(pred1)
cor(pred1$fit,calorie$`Calories Consumed`)
rmse=sqrt(mean(reg_log$residuals^2))
rmse
ggplot(data = calorie, aes(log(`Calories Consumed`),`Weight gained (grams)`) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

ggplot(data = calorie, aes(x = log(`Calories Consumed`), y =`Weight gained (grams)`)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = calorie, aes(x = log(`Calories Consumed`), y = pred1$fit))


####****log of weight gained
plot(log(`Weight gained (grams)`),`Calories Consumed`)
reg_logw=lm(log(`Weight gained (grams)`)~`Calories Consumed`,data = calorie)
summary(reg_logw)
predlogw <- predict(reg_logw, interval = "confidence")
predlogw
predlogw <- as.data.frame(predlogw)
reg$residuals
sqrt(mean(reg_logw$residuals^2))
pred <- exp(predlogw)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, calorie$`Weight gained (grams)`)
res_log1 = `Weight gained (grams)` - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse
# Alternate way
ggplot(data =calorie, aes(x = `Calories Consumed`, y = log(`Weight gained (grams)`))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = calorie, aes(x = `Calories Consumed`, y = predlogw$fit))



# Data Partition

# Random Sampling
n <- nrow(calorie)
n1 <- n * 0.8
n2 <- n - n1
train_ind <- sample(1:n, n1)
train <- calorie[train_ind, ]
test <-  calorie[-train_ind, ]
train <- calorie[1:10, ]
test <-calorie[11:14, ]
plot(log(`Weight gained (grams)`),`Calories Consumed`)
plot(test$`Weight gained (grams)`, log(test$`Calories Consumed`))
model <- lm(lm(log(`Weight gained (grams)`)~`Calories Consumed`), data = train)
summary(model)
confint(model,level=0.95)


log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
summary(predict_original)
test_error <- test$`Weight gained (grams)` - predict_original$fit # calculate error/residual
test_error


test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$`Weight gained (grams)` - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse


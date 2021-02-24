library(readr)
library(readxl)
delivery=read.csv("E:/Assignments/Assignment week 11/linear/Assignment/delivery_time.csv")
summary(delivery)
attach(delivery)
plot(delivery$Sorting.Time,delivery$Delivery.Time)
hist(delivery$Delivery.Time)
hist(delivery$Sorting.Time)
qqnorm(delivery$Delivery.Time)
qqline(delivery$Delivery.Time)
qqnorm(delivery$Sorting.Time)
qqline(delivery$Sorting.Time)
# As we can see that sorting time is not normal ,so we transform the data
############################
library(Hmisc)
reg2 <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time), data = delivery)
summary(reg2)

predlog <- predict(reg2, interval = "confidence")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, delivery$Delivery.Time)
predlog=data.frame(predlog)
res2 = Delivery.Time - pred$fit
rmse <- sqrt(mean(res2^2))
rmse
ggplot(data = delivery, aes(x = Sorting.Time, y = log(Delivery.Time))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = delivery, aes(x = Sorting.Time, y = predlog$fit))
#Build our model

n <- nrow(delivery)
n1 <- n * 0.7
n2 <- n - n1
train=delivery[1:16,]
test=delivery[17:21,]
plot(train$Sorting.Time, log(train$Delivery.Time))
plot(test$Sorting.Time, log(test$Delivery.Time))
model=lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time),data = train)
summary(model)
confint(model,level=0.95)
log_res <- predict(model,interval = "confidence", newdata = test)
predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Delivery.Time - predict_original$fit # calculate error/residual
test_error
test_rmse <- sqrt(mean(test_error^2))
test_rmse


log_res_train <- predict(model, interval = "confidence", newdata = train)
predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Delivery.Time - predict_original_train$fit # calculate error/residual
train_error
train_rmse <- sqrt(mean(train_error^2))
train_rmse
library(readr)
library(readxl)
emp=read.csv("E:/Assignments/Assignment week 11/linear/Assignment/emp_data.csv")
summary(emp)
colnames(emp)
plot(emp$Salary_hike,emp$Churn_out_rate)
dotplot(emp$Salary_hike)
dotplot(emp$Churn_out_rate)
histogram(emp$Salary_hike)
histogram(emp$Churn_out_rate)
library(Hmisc)
attach(emp)
boxplot(emp$Salary_hike)
boxplot(emp$Churn_out_rate,horizontal = TRUE)
qqnorm(emp$Churn_out_rate)
qqline(emp$Churn_out_rate)
qqnorm(emp$Salary_hike)
qqline(emp$Salary_hike)
cor(Salary_hike,Churn_out_rate)
cov(Salary_hike,Churn_out_rate)
#%%%%%%%%%5

reg1=lm(log(Churn_out_rate)~Salary_hike,data=emp)
summary(reg1)


confint(reg1,level=0.95)
pred <- predict(reg1, interval = "confidence")
pred <- as.data.frame(pred)

reg1$residuals
sqrt(mean(reg1$residuals^2))

pred <- exp(pred)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, emp$Churn_out_rate)

res_log1 =Churn_out_rate - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse
library(ggplot2)
ggplot(data = emp, aes(x = Salary_hike , y = log(Churn_out_rate))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = emp, aes(x = Salary_hike, y = pred$fit))
#%%%%%%%%%%%%%%% building the model%%%%%%%%%%%%%%%
n <- nrow(emp)
n1 <- n * 0.6
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- emp[train_ind, ]
test <-  emp[-train_ind, ]

plot(train$Salary_hike,train$Churn_out_rate)
plot(test$Salary_hike,test$Churn_out_rate)

model=lm(log(Churn_out_rate)~Salary_hike,data=train)
summary(model)
confint(model,level = 0.95)
log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Churn_out_rate - predict_original$fit # calculate error/residual
test_error
test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)
predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Churn_out_rate - predict_original_train$fit # calculate error/residual
train_error
train_rmse <- sqrt(mean(train_error^2))
train_rmse


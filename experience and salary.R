library(readr)
hike=read.csv("E:/Assignments/Assignment week 11/linear/Assignment/Salary_Data.csv")
attach(hike)
summary(hike)
plot(hike$YearsExperience,hike$Salary)
library(lattice)
dotplot(hike$YearsExperience)
dotplot(hike$Salary)
histogram(hike$YearsExperience)
histogram(hike$Salary)
boxplot(hike$YearsExperience,horizontal = TRUE)
boxplot(hike$Salary,horizontal = TRUE)
qqnorm(hike$YearsExperience)
qqline(hike$YearsExperience)
qqnorm(hike$Salary)
qqline(hike$Salary)
hike["sal_log"]=log(hike$Salary)
drop(hike['log'])
plot(hike$YearsExperience,hike$sal_log)
hist(hike$sal_log)
qqnorm(hike$sal_log)
qqline(hike$sal_log)

#*******************************
library(Hmisc)
reg=lm(Salary~YearsExperience,data = hike)
summary(reg)
confint(reg,model="confidence")
pred <- predict(reg, interval = "confidence")
pred <- as.data.frame(pred)

ggplot(data = hike, aes(x = YearsExperience, y =Salary)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = hike, aes(x = YearsExperience, y = pred$fit))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n <- nrow(hike)
n1 <- n * 0.8
n2 <- n - n1
train_ind <- sample(1:n, n1)
train <- hike[train_ind, ]
test <-  hike[-train_ind, ]

plot(train$YearsExperience,train$Salary)
plot(test$YearsExperience,test$Salary)

model=lm(Salary~YearsExperience,data =hike)
summary(model)
confint(model,level = 0.95)
prediction=predict(model,interval = "confidence",data=train)
prediction=data.frame(prediction)
library(Metrics)
train_mape=mape(train$Salary,prediction$fit)
train_mape


log_res <- predict(model,interval = "confidence", newdata = test)
test_mape=mape(test$Salary,prediction$fit)
test_mape



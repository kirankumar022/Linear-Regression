import pandas as pd
# Importing necessary libraries
# deals with data frame  
import numpy as np 
import matplotlib.pyplot as plt
wcat=pd.read_csv("E:/Assignments/Assignment week 11/linear/Assignment/emp_Data.csv")
wcat.columns='Waist','AT'
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt

plt.hist(wcat.AT) #histogram
plt.boxplot(wcat.AT) #boxplot
plt.scatter(x = wcat['Waist'], y = wcat['AT'], color = 'green') 
# correlation
np.corrcoef(wcat.Waist, wcat.AT) 
cov_output = np.cov(wcat.Waist, wcat.AT)[0, 1]
cov_output

# wcat.cov()


# Import library
import statsmodels.formula.api as smf
from sklearn.model_selection import train_test_split

train, test = train_test_split(wcat, test_size = 0.2)

finalmodel = smf.ols('np.log(AT) ~ Waist ', data = train).fit()
finalmodel.summary()

# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
pred_test_AT = np.exp(test_pred)
pred_test_AT

# Model Evaluation on Test data
test_res = test.AT - pred_test_AT
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
pred_train_AT = np.exp(train_pred)

pred_train_AT

# Model Evaluation on train data
train_res = train.AT - pred_train_AT
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse



import pandas as pd
# Importing necessary libraries
# deals with data frame  
import numpy as np 
import matplotlib.pyplot as plt
wcat=pd.read_csv("E:/Assignments/Assignment week 11/linear/Assignment/delivery_time.csv")
wcat.columns='AT','Waist'
plt.scatter(x = wcat['Waist'], y = wcat['AT'], color = 'green') 
# correlation
np.corrcoef(wcat.Waist, wcat.AT) 

cov_output = np.cov(wcat.Waist, wcat.AT)[0, 1]
cov_output
import statsmodels.formula.api as smf
model4 = smf.ols('np.log(AT) ~ Waist ', data = wcat).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(wcat))
pred4_at = np.exp(pred4)
pred4_at
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = wcat.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)
plt.scatter(wcat.Waist, np.log(wcat.AT))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()













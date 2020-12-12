from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

monthly = pd.read_csv('monthly_data.csv')
monthly['date'] = pd.to_datetime(monthly['date'],format='%Y-%m-%d')
monthly.set_index('date',inplace=True)

quarterly = pd.read_csv('quarterly_data.csv')
quarterly['date'] = pd.to_datetime(quarterly['date'],format='%Y-%m-%d')
monthly['productivity'] = quarterly.set_index('date').resample('MS').interpolate(method='quadratic')



res = smf.ols('I(wage_growth-expected_inflation) ~ emratio',monthly).fit()
res.summary()

plt.plot(monthly['emratio'])
plt.figure(2)
plt.plot(monthly['wage_growth'])
plt.show()
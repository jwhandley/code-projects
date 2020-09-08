from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

df = pd.read_csv('seasonally_adjusted_lt_ts.csv')
df['date'] = pd.to_datetime(df['date'])
df.set_index('date',inplace=True)

df_index = df/df.loc['2015-01-01':'2015-12-01'].mean()*100

mhlw = pd.read_csv('mhlw_index.csv')
mhlw = mhlw.melt(id_vars='year',var_name='month',value_name='Total Hours')
mhlw['day'] = 1
mhlw['date'] = pd.to_datetime(mhlw[['year','month','day']])
mhlw.set_index('date',inplace=True)
mhlw.sort_index(inplace=True)

plt.plot(df_index['Total Hours'],label='My calculations')
plt.plot(mhlw['Total Hours'],label='MHLW data')
plt.legend()
plt.show()
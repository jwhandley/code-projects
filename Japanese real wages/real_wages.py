from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

df = pd.read_csv('real_wage_index.csv')
df = df.melt(id_vars='year',var_name='month',value_name='real_wage')
df['day'] = 1
df['date'] = pd.to_datetime(df[['year','month','day']])
df.set_index('date',inplace=True)

df['real_wage'].plot()
plt.show()
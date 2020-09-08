from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

df = pd.read_csv('wage_ts.csv')
df['date'] = pd.to_datetime(df[['year','month','day']])
df.set_index('date',inplace=True)

df_std = df.loc[df['employment_type']=='Standard']
std = df_std['total_hours']/df_std.loc['2015-01-01':'2015-12-01','total_hours'].mean()*100
df_pt = df.loc[df['employment_type']=='Part-time']
pt = df_pt['total_hours']/df_pt.loc['2015-01-01':'2015-12-01','total_hours'].mean()*100

standard = pd.read_csv('mhlw_index_standard.csv')

standard = standard.melt(id_vars='year',var_name='month',value_name='total_hours')
standard['day'] = 1
standard['date'] = pd.to_datetime(standard[['year','month','day']])
standard.set_index('date',inplace=True)
standard.sort_index(inplace=True)

part = pd.read_csv('mhlw_index_parttime.csv')
part = part.melt(id_vars='year',var_name='month',value_name='total_hours')
part['day'] = 1
part['date'] = pd.to_datetime(part[['year','month','day']])
part.set_index('date',inplace=True)
part.sort_index(inplace=True)

plt.figure(0)
std.rolling(12).mean().plot(label='My calculations')
standard['total_hours'].rolling(12).mean().plot(label='MHLW data')
plt.legend()

plt.figure(1)
pt.rolling(12).mean().plot(label='My calculations')
part['total_hours'].rolling(12).mean().plot(label='MHLW data')
plt.legend()
plt.show()
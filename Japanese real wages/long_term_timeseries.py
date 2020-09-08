from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
lowess = sm.nonparametric.lowess
seasonal = sm.tsa.seasonal_decompose

df = pd.read_csv('long_term_timeseries.csv')
df['date'] = pd.to_datetime(df['date'],format="%d/%m/%Y")
df.set_index('date',inplace=True)

cpi = pd.read_csv('cpi.csv')
cpi['date'] = pd.to_datetime(cpi['date'],format="%d/%m/%Y")
cpi.set_index('date',inplace=True)

columns = [i for i in df.columns]
names = [i.replace('_',' ').title() for i in df.columns]
units = 5*['10,000 yen at constant 2015 prices']+3*['Hours per month']+['Days per month']

df.rename(columns=dict(zip(columns,names)),inplace=True)

for i in df.columns:
    ts = df.dropna(subset=[i])
    df.loc[ts.index[0]:,i] = seasonal(ts[i],model='multiplicative',extrapolate_trend='freq').trend

earnings = df.filter(regex='Earnings').columns
for i in earnings:
    df.loc[:,i] = df.loc[:,i]/cpi.loc[:,'cpi']*100/10000


fig, axs = plt.subplots(3,3,figsize=(16,10),constrained_layout=True)

for i in range(len(names)):
    axs.flatten()[i].plot(df[names[i]],color='C{}'.format(i))
    axs.flatten()[i].set_title(names[i])
    axs.flatten()[i].set_ylabel(units[i])

fig.suptitle('Monthly earnings and hours for full-time workers at firms with 30 or more employees in Japan')
plt.savefig('long_term_timeseries_real.png',dpi=200)

df['Total hourly earnings'] = df['Cash Earnings']/df['Total Hours']*10000
df['Mandatory hourly earnings'] = df['Mandatory Earnings']/df['Total Hours']*10000
df['Scheduled hourly earnings'] = df['Scheduled Earnings']/df['Scheduled Hours']*10000

df.filter(regex='hourly').plot(subplots=True,layout=(1,3),figsize=(24,5),title=['Including bonuses and overtime','Excluding bonuses','Excluding bonuses and overtime'],legend=False)
plt.suptitle('Real hourly earnings for full-time workers at firms with over 30 employees in Japan')
plt.savefig('lt_ts_hourly.png',dpi=200)
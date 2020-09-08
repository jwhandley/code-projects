from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.api as sm

df = pd.read_csv('wage_ts.csv')
df['date'] = pd.to_datetime(df[['year','month','day']])
df.set_index('date',inplace=True)

cpi = pd.read_csv('cpi.csv')
cpi['date'] = pd.to_datetime(cpi['date'],format='%d/%m/%Y')
cpi.set_index('date',inplace=True)

employment_type = ['Standard','Part-time']

earnings = df.filter(regex='earnings').columns

for i in earnings:
    df.loc[:,i] = (df.loc[:,i]/cpi.loc[:,'cpi']*100).dropna()

for i in employment_type:
    for j in df.columns[4:]:
        df.loc[df['employment_type']==i,j] = sm.tsa.seasonal_decompose(df.loc[df['employment_type']==i,j],extrapolate_trend='freq').trend

columns = [i for i in df.columns[4:]]
names = [i.replace('_',' ').capitalize() for i in df.columns[4:]]
units = 5*['Yen in constant 2015 prices'] + 3*['Monthly hours worked'] + ['Monthly days worked']

cols = dict(zip(columns,names))
df.rename(columns=cols,inplace=True)

df.loc[df['employment_type']=='Standard',df.columns[4:]].to_csv('standard_ts.csv')
df.loc[df['employment_type']=='Part-time',df.columns[4:]].to_csv('part_time_ts.csv')

for i in employment_type:
    fig, axs = plt.subplots(3,3,figsize=(16,10),constrained_layout=True)
    for j in range(len(names)):
        axs.flatten()[j].plot(df.loc[df['employment_type']==i,names[j]])
        axs.flatten()[j].set_title(names[j])
        axs.flatten()[j].set_ylabel(units[j])
    plt.suptitle('Monthly earnings and hours for {} workers at firms with 30 or more employees in Japan'.format(i.lower()))
    plt.savefig('{}_worker_chart.png'.format(i),dpi=200)

fix,axs = plt.subplots(2,3,figsize=(16,10),constrained_layout=True)

df['Hourly cash earnings'] = df['Cash earnings']/df['Total hours']
df['Hourly mandatory earnings'] = df['Mandatory earnings']/df['Total hours']
df['Hourly scheduled earnings'] = df['Scheduled earnings']/df['Scheduled hours']

hourly = df.filter(regex='Hourly').columns

for i in range(len(hourly)):
    for j in range(len(employment_type)):
        axs[j,i].plot(df.loc[df['employment_type']==employment_type[j],hourly[i]],color='C{}'.format(j))
        axs[j,i].set_title('{} for {} workers'.format(hourly[i].split(' ',1)[1].capitalize(),employment_type[j].lower()))

plt.suptitle('Real hourly wage in Japan by income definition and worker type')
plt.savefig('hourly_wage.png',dpi=200)


plt.show()
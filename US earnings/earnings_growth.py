from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import scipy
import statsmodels.api as sm
lowess = sm.nonparametric.lowess
import statsmodels.formula.api as smf

# Read data
df = pd.read_stata('cps_00157.dta')
df['age'] = df['age'].cat.codes
df['educ'] = df['educ'].cat.codes
df['educ'] = (df['educ'] > 23).astype(int) + (df['educ']>17).astype(int)
df['educ'] = df['educ'].replace([0,1,2],['High school or less','Some college','Degree'])
cpi = pd.read_csv('cpi.csv')
cpi = pd.melt(cpi,id_vars='year',var_name='month',value_name='cpi_u_rs')
cpi['day'] = 1
cpi['date'] = pd.to_datetime(cpi[['year','month','day']])
cpi.set_index('date',inplace=True)

# Filter out observations not in universe
df = df[(df['earnwt']>0) & (df['paidhour']!='niu') & (df['earnweek']!=9999.99)]

# Recode to NAs
df['uhrsworkorg'].replace(999,np.nan,inplace=True)
df['uhrswork1'] = df['uhrswork1'].cat.codes.replace([997,999,0],3*[np.nan])
df['hourwage'].replace(999.99,np.nan,inplace=True)

# Compute new hours variable
df['hours'] = df['uhrsworkorg'].fillna(df['uhrswork1'])

# Calculate annual topcodes 
topcode = df.groupby('year')['earnweek'].max()
# Calculate annual means
mean = df.groupby('year')['earnweek'].mean()

# Put topcodes and means back into dataframe
df = df.set_index('year')
df['topcode'] = topcode
df['mean'] = mean

# Filter out data above 2 times the mean and below the topcode
fit_data = df.loc[(df['earnweek']>2*df['mean']) & (df['earnweek']<df['topcode']),'earnweek']

# Create array for fitted values
fit = np.empty([df.index.max()-df.index.min()+1,3])

# Fit the Pareto distribution to data for each year
for i in range(df.index.min(),df.index.max()+1):
    fit[i-df.index.min()] = scipy.stats.pareto.fit(fit_data.loc[i],floc=0)

# Assume the true alpha parameter is the same across all years
alpha = np.mean(fit[:,0])

# Use alpha to find the multiplier for the topcode, update earnweek, and compute wage variable
df['topcoded'] = df['earnweek'] == df['topcode']
df['impute'] = df['topcode']*alpha/(alpha-1)
df['earnweek'] = df['topcoded']*df['impute'] + (1-df['topcoded'])*df['earnweek']
df['wage'] = df['hourwage'].fillna(df['earnweek']/df['hours'])

df['wt_wage'] = df['wage']*df['earnwt']

df['month'] = df['month'].cat.codes+1
df['day'] = 1
df['year'] = df.index
df['date'] = pd.to_datetime(df[['year','month','day']])
df.set_index('date',inplace=True)

df['real_wage'] = df['wage']/cpi['cpi_u_rs']*cpi.loc['2019-12-01','cpi_u_rs'].values[0]


df['real_wt_wage'] = df['real_wage']*df['earnwt']
res = (df.groupby(['year','sex','educ'])['real_wt_wage'].sum()/df.groupby(['year','sex'])['earnwt'].sum()).reset_index().rename(columns={0:'real_wage'})
res.head()

table = res.pivot_table(index='year',columns=['sex','educ'],values='real_wage')

fig,ax = plt.subplots(1,2,sharey=True)
ax[0].plot(np.log(table['male']/table['male'].loc[1982]))
ax[0].set_title('Male')
ax[0].axhline(0,color='grey')
ax[1].plot(np.log(table['female']/table['female'].loc[1982]))
ax[1].set_title('Female')
ax[1].axhline(0,color='grey')
plt.legend(labels=df['educ'].unique())
plt.suptitle('Growth in real average hourly earnings in the US')
plt.tight_layout()
fig.show()
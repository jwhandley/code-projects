from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from linearmodels import FirstDifferenceOLS

df = pd.read_csv('combined_dataset.csv')
df['date'] = pd.to_datetime(df['date'],format='%d/%m/%Y')
ts = df.set_index(['country','date'])

countries = ['Korea','Japan','United Kingdom','United States']
data = df.groupby('country').mean()

fig, axs = plt.subplots(2,2,figsize=(16,10))

for country in countries:
    axs[0,0].plot(ts.loc[country,'stringency'],label=country)
axs[0,0].legend()
axs[0,0].set_title('Government restrictions index')

for country in countries:
    axs[0,1].plot(ts.loc[country,'new_cases_per_million'],label=country)
axs[0,1].set_title('New cases per million')
axs[0,1].legend()

data.loc[countries,'growth'].plot.bar(ax=axs[1,0])
axs[1,0].tick_params(labelrotation=0)
axs[1,0].set_title('IMF October 2020 growth projections')

data.loc[countries,'unemp'].plot.bar(ax=axs[1,1])
axs[1,1].tick_params(labelrotation=0)
axs[1,1].set_title('IMF October 2020 projected change in unemployment')
fig.show()

data['q1'] = df.loc[df.date<='2020-03-31'].groupby('country')['stringency'].mean()
data['q2'] = df.loc[(df.date<'2020-07-01') & (df.date>='2020-04-01')].groupby('country')['stringency'].mean()
data['q3'] = df.loc[(df.date<'2020-10-01') & (df.date>='2020-07-01')].groupby('country')['stringency'].mean()
data['total_cases_per_million'] = df.groupby('country')['total_cases_per_million'].last()

res = smf.ols('np.log(total_cases_per_million) ~ q1 + q2 + q3',data).fit()
res.summary()

plt.scatter(data.q1,np.log(data['new_cases_per_million']))
plt.show()

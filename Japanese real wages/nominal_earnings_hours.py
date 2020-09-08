from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
lowess = sm.nonparametric.lowess
seasonal = sm.tsa.seasonal_decompose

df = pd.read_csv('earnings_timeseries.csv')
df['date'] = pd.to_datetime(df['date'],format="%d/%m/%Y")
df.set_index('date',inplace=True)
df = df.loc['1993-01-01':'2019-12-01',]
df.rename(columns=dict(zip([i for i in df.columns],[i.replace('_',' ').title() for i in df.columns])),inplace=True)

for i in df.columns:
    df[i] = seasonal(df[i],model='multiplicative',extrapolate_trend='freq').trend

df.plot(subplots=True,layout=(3,3))
plt.show()
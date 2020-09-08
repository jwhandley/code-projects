from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import scipy.stats

df = pd.read_stata('earnings_org.dta')
df.dropna(subset=['earnweek'],inplace=True)
df = df[df.hours>0]
df['year'] = df['year'].astype(int)
df.set_index('year',inplace=True)

fit_data = df.loc[(df['earnweek']>df['threshold']) & (df['earnweek']<df['topcode']),'earnweek']

# Create array for fitted values
fit = np.empty([df.index.max()-df.index.min()+1,3])

# Fit the Pareto distribution to data for each year
for i in range(df.index.min(),df.index.max()+1):
    fit[i-df.index.min()] = scipy.stats.pareto.fit(fit_data.loc[i],floc=0)


alpha = pd.DataFrame(fit[:,0],index=df.index.unique(),columns=['fitted_alpha'])
df['alpha'] = alpha['fitted_alpha']

# Use alpha to find the multiplier for the topcode, update earnweek, and compute wage variable
df.reset_index(inplace=True)
df['impute'] = df['topcode']*df['alpha']/(df['alpha']-1)
df['earnweek'] = df['is_topcode']*df['impute'] + (1-df['is_topcode'])*df['earnweek']
df['wage'] = df['hourwage'].fillna(df['earnweek']/df['hours'])

df.to_stata('earnings_org_imputed.dta')
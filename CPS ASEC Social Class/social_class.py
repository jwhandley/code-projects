from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import time
from itertools import chain

# Clean up data
df = pd.read_stata('cps_00165.dta')
df = df[(df['ftotval']>0) & (df['ftotval']!=999999)]

# Define new income variable
df.loc[:,'income'] = df['ftotval']/(df['famsize'].cat.codes+1)
df.loc[:,'log_income'] = np.log(df['income'])

# Weighted average function
def wtd_mean(x,w):
    return np.sum(x*w)/np.sum(w)

# Weighted standard deviation function
def wtd_sd(x,w):
    diff = (x - wtd_mean(x,w))**2
    n = len(x)

    return np.sqrt(n/(n-1)*np.sum(w*diff)/np.sum(w))

# Group dataframe function
def group_df(df,group,value):
    return (df.loc[group==i,value].values for i in df[group].unique())

def wtd_zscore(x,w):
    return (x-wtd_mean(x,w))/wtd_sd(x,w)

def flatten(x):
    res = []
    for sublist in x:
        for item in sublist:
            res.append(item)

    return res

start = time.perf_counter()
for i in df['year'].unique():
    df.loc[df['year']==i,'zscore'] = wtd_zscore(df.loc[df['year']==i,'log_income'],df.loc[df['year']==i,'asecwt'])
end = time.perf_counter()
print(end-start)

start = time.perf_counter()
df['zscore'] = list(map(wtd_zscore,df.groupby('year')['income'].apply(lambda x: x),df.groupby('year')['asecwt'].apply(lambda x: x)))
end = time.perf_counter()
print(end-start)

start = time.perf_counter()
df['zscore'] = list(chain.from_iterable(map(wtd_zscore,(df.loc[df['year']==i,'log_income'] for i in df['year'].unique()),(df.loc[df['year']==i,'asecwt'] for i in df['year'].unique()))))
end = time.perf_counter()
print(end-start)

def income_class(x):
    if x > 1:
        return 'Upper class'
    elif x <= 1 and x > 0:
        return 'Middle class'
    elif x <= 0 and x > -1:
        return 'Working class'
    else:
        return 'Lower class'

df['class'] = list(map(income_class,df['zscore']))

class_id = df.groupby(['year','class'])['asecwt'].sum()/df.groupby('year')['asecwt'].sum()

(class_id.reset_index().pivot(index='year',columns='class',values='asecwt')).plot()
plt.show()
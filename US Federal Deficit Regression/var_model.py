from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from statsmodels.tsa.api import VAR
import statsmodels.formula.api as smf

df = pd.read_csv('deficit_data2.csv')
df['year'] = df.date.str.split('/',expand=True)[2].astype(float)
df['date'] = pd.to_datetime(df['date'],format='%d/%m/%Y')
df.set_index('date',inplace=True)
df.dropna(inplace=True)
df['vietnam'] = ((df['year']>=1965) & (df['year']<=1974)).astype(int)
df['b1980_2003'] = ((df['year']>=1980) & (df['year']<=2003)).astype(int)
df['after1990'] = (df['year']>=1990).astype(int)

def lag(x, n):
    if n == 0:
        return x
    if isinstance(x, pd.Series):
        return x.shift(n) 
    else:
        x = pd.Series(x)
        return x.shift(n) 

    x = x.copy()
    x[n:] = x[0:-n]
    x[:n] = np.nan
    return x

res = smf.ols('pb ~ output_gap + np.log(defense) + year + gs10 + output_gap:year + b1980_2003',df.loc[:'2007-10-01']).fit()
res.summary()

df['pred_pb'] = res.predict(df)

df.loc[:'2020-01-01',['pred_pb','pb']].plot()
plt.show()

(df['pred_pb']-df['pb']).loc[:'2020-01-01'].plot()
plt.show()
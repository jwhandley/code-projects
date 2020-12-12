import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

gini = pd.read_csv('swiid8_3_summary.csv')
cwed = pd.read_csv('cwed_gen.csv')
cpds = pd.read_stata('CPDS_1960-2018_Update_2020.dta')

df = pd.merge(pd.merge(gini,cwed),cpds)
df['after1980'] = (df['year']>=1980).astype(int)
df['gini_lag'] = df.groupby(['country'])['gini_disp'].shift()
data = df[['country','year','gini_lag','gini_disp','gov_left1','gov_cent1','totgen','adjcov_ipol','after1980']].set_index(['country','year']).dropna()

res = PanelOLS.from_formula('gini_disp ~ 1 + gini_lag + gov_left1*after1980 + totgen + adjcov_ipol + EntityEffects + TimeEffects',data,drop_absorbed=True)
res.fit()
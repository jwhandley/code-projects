import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

pwt = pd.read_stata('pwt91.dta')[['country','year','rgdpe','pop']]
cpds = pd.read_stata('CPDS_1960-2018_Update_2020.dta')[['country','year','gov_left1','gov_cent1','adjcov_ipol']]
df = pd.merge(cpds,pwt)

df['gdppc'] = np.log(df['rgdpe']/df['pop'])
df['growth'] = df.groupby('country')['gdppc'].diff(5).shift(-5)/5

data = df[['country','year','growth','gdppc','gov_left1','gov_cent1','adjcov_ipol']].dropna().set_index(['country','year'])

res = PanelOLS.from_formula('growth ~ gdppc + gov_left1*adjcov_ipol + EntityEffects + TimeEffects',data)
res.fit()
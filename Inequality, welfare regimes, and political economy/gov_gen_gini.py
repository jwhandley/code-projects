import pandas as pd
import numpy as np
from linearmodels import PanelOLS, BetweenOLS

df = pd.read_csv('gini_cwed_cpds.csv')

df['gini_chg'] = df.groupby('country')['gini_disp'].diff()
df['gen_chg'] = df.groupby('country')['totgen'].diff()
df['after1980'] = (df['year']>=1980).astype(int)
df['after1990'] = (df['year']>=1990).astype(int)
data = df.set_index(['country','year'])

fe = PanelOLS.from_formula('gini_chg ~ 1 + gov_left1*after1980 + EntityEffects + TimeEffects',data.dropna(subset=['gov_left1','gini_chg']),drop_absorbed=True)
fe.fit()

fd = PanelOLS.from_formula('gini_chg ~ gov_left1*after1980',data.dropna(subset=['gov_left1','gini_chg']))
fd.fit()

gen_fe = PanelOLS.from_formula('gen_chg ~ 1 + gov_left1*after1990 + EntityEffects + TimeEffects',data.dropna(subset=['gov_left1','gen_chg']),drop_absorbed=True)
gen_fe.fit()
from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS

df = pd.read_csv('gov_rile_gini1.csv')

df['gini_chg'] = df.groupby('country')['gini_disp'].diff()*100
df['after1980'] = (df['year']>1980).astype(int)

data = df.set_index(['country','year'])

base_model = PanelOLS.from_formula('gini_chg ~ 1 + gov_left1*after1980 + EntityEffects + TimeEffects',data.dropna(subset=['gini_chg']),drop_absorbed=True)
base_model.fit()

rile_model = PanelOLS.from_formula('gini_chg ~ 1 + rile*gov_left1 + EntityEffects + TimeEffects',data.dropna(subset=['gini_chg']),drop_absorbed=True)
rile_model.fit()
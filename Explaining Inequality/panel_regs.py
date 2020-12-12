from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

gini = pd.read_csv('swiid8_3_summary.csv')
gen = pd.read_csv('cwed_totgen.csv')
wage = pd.read_csv('wage_bargaining.csv')
reg = pd.read_csv('etc_reg.csv')

df = pd.merge(pd.merge(pd.merge(gini,gen),wage),reg)
df['gini_diff'] = df.groupby('country')['gini_disp'].diff().shift(-1)
data = df.set_index(['country','year'])


fe = PanelOLS.from_formula('gini_disp ~ 1 + level + coord + reg + totgen + EntityEffects + TimeEffects',data)
fe.fit()

fe2 = PanelOLS.from_formula('gini_diff ~ 1 + gini_disp + level + coord + reg + totgen + EntityEffects + TimeEffects',data.dropna(subset=['gini_diff']))
fe2.fit()

fd = FirstDifferenceOLS.from_formula('gini_disp ~ level + coord + reg + totgen',data)
fd.fit()
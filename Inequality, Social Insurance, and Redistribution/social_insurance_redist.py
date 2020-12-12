from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

sied = pd.read_csv('sied_gen.csv')
gini = pd.read_csv('swiid8_3_summary.csv')

df = pd.merge(sied,gini)
df['redist'] = np.log(df['gini_mkt']/df['gini_disp'])*100
df = df.set_index(['country','year'])

fd = FirstDifferenceOLS.from_formula('redist ~ gen',data=df)
print(fd.fit())

fe = PanelOLS.from_formula('redist ~ 1 + gen + EntityEffects + TimeEffects',data=df)
print(fe.fit())
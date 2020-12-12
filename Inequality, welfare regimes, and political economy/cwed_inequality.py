import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

cwed = pd.read_csv('cwed_gen.csv')
gini = pd.read_csv('swiid8_3_summary.csv')
etcr = pd.read_csv('etcreg.csv')

df = pd.merge(pd.merge(cwed,gini),etcr)
data = df.set_index(['country','year'])

fe = PanelOLS.from_formula('gini_disp ~ 1 + totgen + etcreg + EntityEffects + TimeEffects',data.dropna(subset=['gini_disp','totgen','etcreg']))
print(fe.fit())

mkt = PanelOLS.from_formula('gini_mkt ~  1 + etcreg + TimeEffects + EntityEffects',data.dropna(subset=['gini_mkt','etcreg']))
print(mkt.fit())
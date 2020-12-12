from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS, RandomEffects

df = pd.read_csv('inequality_social_insurance_gov.csv')

df['gini_chg'] = df.groupby('country')['gini_disp'].diff().shift(-1)
df['gen_chg'] = df.groupby('country')['gen'].diff(5).shift(-5)
df['after1980'] = (df['year']>=1980).astype(int)
df = df.set_index(['country','year'])

print('Testing the relationship between inequality and social insurance generosity.')
gini_gen = PanelOLS.from_formula('gini_disp ~ 1 + gen + EntityEffects + TimeEffects',df.dropna(subset=['gini_disp','gen']))
print(gini_gen.fit())

print('Testing whether more left-wing cabinets produce increases in social insurance generosity, and whether this has changed since 1980.')
res_gen = PanelOLS.from_formula('gen_chg ~ 1 + gen + gov_centleft*after1980 + EntityEffects + TimeEffects',df.dropna(subset=['gen_chg','gov_centleft']),drop_absorbed=True)
print(res_gen.fit())

print('Testing whether more left-wing cabinets produce reductions in inequality, and whether this has changed since 1980.')
res_gini = PanelOLS.from_formula('gini_chg ~ 1 + gini_disp + gov_centleft*after1980 + EntityEffects + TimeEffects',df.dropna(subset=['gini_chg','gov_centleft']),drop_absorbed=True)
print(res_gini.fit())
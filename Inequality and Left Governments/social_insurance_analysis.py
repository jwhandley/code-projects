from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS, RandomEffects

df = pd.read_csv('inequality_social_insurance_gov.csv').set_index(['country','year'])

df = df.dropna(subset=['gen'])
df['gen_diff'] = df['gen'].diff()*100

df = df.reset_index()
df['after1990'] = (df['year']>=1990).astype(int)
df = df.set_index(['country','year'])

res = PanelOLS.from_formula('gen_diff ~ 1 + gov_centleft*after1990',df.dropna(subset=['gen_diff']),drop_absorbed=True)
print(res.fit())

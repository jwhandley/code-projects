from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from linearmodels import PanelOLS, FirstDifferenceOLS

health = pd.read_csv('health_exp.csv')
gdppc = pd.read_csv('gdppc.csv')
aicpc = pd.read_csv('aicpc.csv')

df = pd.merge(pd.merge(health,gdppc),aicpc)
data = df.loc[df['country']!='United States',:].set_index(['country','year'])

fe_gdp = PanelOLS.from_formula('np.log(health_exp) ~ 1 + np.log(gdppc) + EntityEffects + TimeEffects',data)
fe_gdp.fit()

fe_aic = PanelOLS.from_formula('np.log(health_exp) ~ 1 + np.log(aicpc) + EntityEffects + TimeEffects',data)
fe_aic.fit()

diff_gdp = FirstDifferenceOLS.from_formula('np.log(health_exp) ~ np.log(gdppc)',data)
diff_gdp.fit()

diff_aic = FirstDifferenceOLS.from_formula('np.log(health_exp) ~ np.log(aicpc)',data)
diff_aic.fit()
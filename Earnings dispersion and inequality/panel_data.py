import pandas as pd
from linearmodels import PanelOLS, FirstDifferenceOLS
import statsmodels.formula.api as smf

df = pd.read_csv('earnings_gini.csv').set_index(['country','year'])

res = PanelOLS.from_formula('gini_mkt ~ 1 + earnings_90_10 + TimeEffects + EntityEffects',df)
res.fit()
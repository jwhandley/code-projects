from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

df = pd.read_csv('data_cleaned.csv')

res_dem = smf.mixedlm('dem ~ inccode + np.power(inccode,2)',df,groups=df['year']).fit()
res_dem.summary()
smf.ols('dem ~ inccode + np.power(inccode,2)',df).fit().summary()

res_rep = smf.mixedlm('rep ~ inccode',df,groups=df['year']).fit()
res_rep.summary()
from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from tqdm import tqdm
import statsmodels.api as sm

df = pd.read_stata('usa_00064.dta').query('occscore>0')

dist = (df.groupby(['year','occscore'])['perwt'].sum()/df.groupby('year')['perwt'].sum()).reset_index()

plt.plot(sm.nonparametric.lowess(dist.loc[dist.year=='2018','perwt'],dist.loc[dist.year=='2018','occscore'],frac=0.15,return_sorted=False),label='2018')
plt.plot(sm.nonparametric.lowess(dist.loc[dist.year=='2010','perwt'],dist.loc[dist.year=='2010','occscore'],frac=0.15,return_sorted=False),label='2010')
plt.legend()
plt.show()
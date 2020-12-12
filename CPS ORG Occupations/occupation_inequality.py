from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from tqdm import tqdm

df = pd.read_stata('cps_00172.dta').query('earnweek!=9999.99')

df['hourwage'].replace(999.99,np.nan,inplace=True)
df['uhrswork'] = df['uhrswork1'].replace(['0 hours','hours vary'],[np.nan,np.nan]).astype(float)
df['uhrswork'] = df['uhrswork'].fillna(df['ahrswork1'])
df['uhrsworkorg'] = df['uhrsworkorg'].fillna(df['uhrswork']).replace([0,'niu'],[np.nan,np.nan]).astype(float)
df['wage'] = df['hourwage'].fillna(df['earnweek']/df['uhrsworkorg'])
df = df.dropna(subset=['wage'])

results = pd.DataFrame(index=range(1982,2020),columns=['male','female'])
for year in tqdm(range(1982,2020)):
    for sex in ['male','female']:
        res = smf.ols('np.log(earnweek) ~ occ2010',df.query(f'(sex=="{sex}") & (year=={year})')).fit()
        results.loc[year,sex] = res.rsquared_adj


results.plot()
plt.show()
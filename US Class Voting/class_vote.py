from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

df = pd.read_csv('class_vote.csv')
df = df.replace(r'^\s*$',np.nan,regex=True)

for col in df.columns:
    df[col] = df[col].astype(float)


df.race = df.race.replace([1,2,3,0,9],['White non-hispanic','Black non-hispanic','Other',np.nan,np.nan])
df.race = df.race.astype('category').cat.reorder_categories(['White non-hispanic','Black non-hispanic','Other'])
df['nonwhite'] = 1-(df['race']=='White non-hispanic').astype(int)

df.educ = df.educ.replace(range(0,5),[np.nan,'High school or less','High school or less','Some college','Degree'])
df.educ = df.educ.astype('category').cat.reorder_categories(['Some college','High school or less','Degree'])

df.faminc = df.faminc.replace(range(0,6),[np.nan,'0-16 percentile','17-33 percentile','34-67 percentile','68-95 percentile','95-100 percentile'])
df.faminc = df.faminc.astype('category').cat.reorder_categories(['34-67 percentile','0-16 percentile','17-33 percentile','68-95 percentile','95-100 percentile'])

df.pres = df.pres.replace(range(0,3),[np.nan,1,0])
df.house = df.house.replace(range(0,3),[np.nan,1,0])

df.dropna(subset=['pres','educ','race','faminc'],inplace=True)

res_tot = smf.logit('pres ~ nonwhite + educ + faminc',df).fit()
res_tot.summary()
res_tot.params[[f'faminc[T.{per} percentile]' for per in ['0-16','17-33','68-95','95-100']]].values

output = pd.DataFrame(index=df.year.unique(),columns=['0-16','17-33','68-95','95-100'])
for year in df.year.unique():
    print(f'Running regression for {year}')
    try:
        res = smf.logit('pres ~ nonwhite + educ + faminc',df.loc[df.year==year]).fit()
        for per in ['0-16','17-33','68-95','95-100']:
            output.loc[year,per] = res.params[f'faminc[T.{per} percentile]']
    except:
        print(f'Regression for {year} failed')

output.head()
output.T.plot.bar()
plt.show()
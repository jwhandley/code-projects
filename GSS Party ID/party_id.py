from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

df = pd.read_csv('cleaned_data.csv')
df['income'] = df['income'].astype('category').cat.reorder_categories(['Low income','Middle income','High income'])
df['educ'] = df['educ'].astype('category').cat.reorder_categories(['High school','Some college','Degree'])
df['race'] = df['race'].astype('category').cat.reorder_categories(['White','Black','Other'])

res = smf.logit('democrat ~ evangelical + educ + np.log(faminc) + age + race',df).fit()
res.summary()

cols = pd.MultiIndex.from_product([['educ','income','evangelical','age','race'],['upper','mean','lower']])
data = pd.DataFrame(index=df.year.unique(),columns=cols)
for year in df.year.unique():
    res = smf.logit('democrat ~ evangelical + educ + np.log(faminc) + age + race',df.loc[df.year==year]).fit()
    data.loc[year,('educ','mean')] = res.params['educ[T.Degree]']
    data.loc[year,('income','mean')] = res.params['np.log(faminc)']
    data.loc[year,('evangelical','mean')] = res.params['evangelical']
    data.loc[year,('age','mean')] = res.params['age']
    data.loc[year,('race','mean')] = res.params['race[T.Black]']

    for idx, i in enumerate(['lower','upper']):
        data.loc[year,('educ',i)] = res.conf_int(alpha=0.05).loc['educ[T.Degree]',idx]
        data.loc[year,('income',i)] = res.conf_int(alpha=0.05).loc['np.log(faminc)',idx]
        data.loc[year,('evangelical',i)] = res.conf_int(alpha=0.05).loc['evangelical',idx]
        data.loc[year,('age',i)] = res.conf_int(alpha=0.05).loc['age',idx]
        data.loc[year,('race',i)] = res.conf_int(alpha=0.05).loc['race[T.Black]',idx]

data.head()

fig, ax = plt.subplots(2,2,figsize=(12,8))
ax[0,0].errorbar(data.index,data['income']['mean'],data['income']['upper']-data['income']['mean'],marker='o',capsize=2,ecolor='grey',color='k')
ax[0,0].axhline(0,color='grey')
ax[0,0].set_title('Income')

ax[1,0].errorbar(data.index,data['educ']['mean'],data['educ']['upper']-data['educ']['mean'],marker='o',capsize=2,ecolor='grey',color='k')
ax[1,0].axhline(0,color='grey')
ax[1,0].set_title('Education')

ax[0,1].errorbar(data.index,data['evangelical']['mean'],data['evangelical']['upper']-data['evangelical']['mean'],marker='o',capsize=2,ecolor='grey',color='k')
ax[0,1].axhline(0,color='grey')
ax[0,1].set_title('White Evangelical Protestant')

ax[1,1].errorbar(data.index,data['race']['mean'],data['race']['upper']-data['race']['mean'],marker='o',capsize=2,ecolor='grey',color='k')
ax[1,1].axhline(0,color='grey')
ax[1,1].set_title('Black')

fig.suptitle('Marginal effect on Democratic party ID')
fig.show()
fig.savefig('marginal_effects.png',dpi=500)

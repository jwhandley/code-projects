from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

df = pd.read_csv('cleaned_data.csv')
df['dem'] = (df['pres'] == 'Democrat').astype('int')
df['rep'] = (df['pres'] == 'Republican').astype('int')

df['faminc'] = df['faminc'].astype('category').cat.reorder_categories(['17-33p','0-16p','34-67p','68-95p','95-100p'])
df['educ'] = df['educ'].astype('category').cat.reorder_categories(['High school or less','Some college','Degree'])
df['race'] = df['race'].astype('category').cat.reorder_categories(['White non-hispanic','Black non-hispanic','Hispanic','Other'])
df['relig'] = df['relig'].astype('category').cat.reorder_categories(['Protestant','Catholic','Other'])
df['white_protestant'] = (df['relig']=='Protestant').astype('int')*(df['race']=='White non-hispanic').astype(int)
df['catholic'] = (df['relig']=='Catholic').astype('int')

data = df.query('dem+rep==1')

res_overall = smf.logit('dem ~ age_group + race + educ + faminc + south + relig',data).fit()
res_overall.summary()

cols = pd.MultiIndex.from_product([['Education','Income','Race','Religion'],['Lower','Mean','Upper']])
output = pd.DataFrame(index=data.year.unique(),columns=cols)
for year in data.year.unique():
    try:
        res = smf.logit('dem ~ age_group + race + educ + faminc + south + relig',data.loc[data.year==year]).fit()
        output.loc[year,('Education','Mean')] = res.params['educ[T.Degree]']
        output.loc[year,('Income','Mean')] = res.params['faminc[T.68-95p]']
        output.loc[year,('Race','Mean')] = res.params['race[T.Black non-hispanic]']
        output.loc[year,('Religion','Mean')] = res.params['relig[T.Catholic]']

        for idx, i in enumerate(['Lower','Upper']):
            output.loc[year,('Education',i)] = res.conf_int(alpha=0.05).loc['educ[T.Degree]',idx]
            output.loc[year,('Income',i)] = res.conf_int(alpha=0.05).loc['faminc[T.68-95p]',idx]
            output.loc[year,('Race',i)] = res.conf_int(alpha=0.05).loc['race[T.Black non-hispanic]',idx]
            output.loc[year,('Religion',i)] = res.conf_int(alpha=0.05).loc['relig[T.Catholic]',idx]

    except:
        pass
output.dropna(inplace=True)
output.head()

fig, axs = plt.subplots(2,2,figsize=(12,8))

axs[0,0].errorbar(output.index,output['Education']['Mean'],output['Education']['Upper'] - output['Education']['Mean'],marker='o',color='k',capsize=2)
axs[0,0].axhline(0,color='grey')
axs[0,0].set_title("Bachelor's degree vs. High school education")

axs[0,1].errorbar(output.index,output['Income']['Mean'],output['Income']['Upper'] - output['Income']['Mean'],marker='o',color='k',capsize=2)
axs[0,1].axhline(0,color='grey')
axs[0,1].set_title('High income vs. low income')

axs[1,0].errorbar(output.index,output['Religion']['Mean'],output['Religion']['Upper'] - output['Religion']['Mean'],marker='o',color='k',capsize=2)
axs[1,0].axhline(0,color='grey')
axs[1,0].set_title('Catholic vs. protestant')

axs[1,1].errorbar(output.index,output['Race']['Mean'],output['Race']['Upper'] - output['Race']['Mean'],marker='o',color='k',capsize=2)
axs[1,1].axhline(0,color='grey')
axs[1,1].set_title('Black non-hispanic vs. white non-hispanic')

fig.suptitle('Marginal effect on two-party presidential vote for Democrats')
fig.savefig('marginal_effects.png',dpi=500)
fig.show()

    

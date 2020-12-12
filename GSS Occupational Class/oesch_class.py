from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from tqdm import tqdm

df = pd.read_csv('data_cleaned.csv')

df['race'] = df['race'].astype('category').cat.reorder_categories(['White','Black','Other'])
df['religd'] = df['religd'].astype('category').cat.reorder_categories(['Mainline protestant','White evangelical protestant','Historical black protestant','Catholic','Other','None'])
df['oesch_class'] = df['oesch_class'].astype('category').cat.reorder_categories(['Managers','Clerks','Technical professionals','Production workers','Socio-cultural professionals','Service workers'])
df['decade'] = pd.cut(df['year'],bins=range(1970,2030,10),labels=[f'{i}-{i+10}' for i in range(1970,2020,10)])

res = smf.ols('pid7 ~ age + educ + np.log(faminc) + religd + race + oesch_class',df).fit()
res.summary()

output = pd.DataFrame(index=df.decade.unique(),columns=['Socio-cultural professionals','Technical professionals','Production workers','Service workers','Education','Income','Race','Religion'])
for decade in tqdm(df.decade.unique()):
    res = smf.ols('pid7 ~ age + educ + np.log(faminc) + religd + race + oesch_class',df.loc[df.decade==decade]).fit()

    for col in ['Socio-cultural professionals','Technical professionals','Production workers','Service workers']:
        output.loc[decade,col] = res.params[f'oesch_class[T.{col}]']

    output.loc[decade,'Education'] = res.params['educ']
    output.loc[decade,'Income'] = res.params['np.log(faminc)']
    output.loc[decade,'Race'] = res.params['race[T.Black]']
    output.loc[decade,'Religion'] = res.params['religd[T.White evangelical protestant]']

output.head()
output.plot(subplots=True,kind='bar',layout=(4,2),legend=False,figsize=(16,10),rot=0,title='Marginal effect on Republican party identification')
plt.show()



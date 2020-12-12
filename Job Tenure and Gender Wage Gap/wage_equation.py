from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

df = pd.read_stata('cps_00169.dta').query('(jtresp=="eligible, interviewed") and (earnweek!=9999.99) and (earnweek>0)')

df['educ'] = df['educ'].replace(['none or preschool','grades 1, 2, 3, or 4','grades 5 or 6','grades 7 or 8','grade 9','grade 10','grade 11','12th grade, no diploma','high school diploma or equivalent','some college but no degree',"associate's degree, occupational/vocational program","associate's degree, academic program","bachelor's degree","master's degree",'professional school degree','doctorate degree'],9*['High school or less'] + 3*['Some college'] + ["Bachelor's degree"] + 3*['Advanced degree']).astype('category')
df['educ'] = df['educ'].cat.reorder_categories(["Bachelor's degree",'Advanced degree','Some college','High school or less'],ordered=True)
df['age'] = df['age'].astype(int)
df = df.query('age>24')
df.loc[:,'age_band'] = pd.cut(df.loc[:,'age'],bins=range(25,80,5),labels=[f'{i}-{i+4}' for i in range(25,75,5)])

df['hourwage'].replace(999.99,np.nan,inplace=True)
df['uhrsworkorg'].replace(999,np.nan,inplace=True)
df['wage'] = df['hourwage'].fillna(df['earnweek']/df['uhrsworkorg'])

res = smf.ols('np.log(wage) ~ educ + age_band + sex',df).fit()
res.summary()

res1 = smf.ols('np.log(wage) ~ educ + age_band + sex + jtyears',df).fit()
res1.summary()
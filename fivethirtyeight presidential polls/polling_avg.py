from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.api as sm
lowess = sm.nonparametric.lowess

df = pd.read_csv('president_polls.csv')
df = df[df.answer.isin(['Biden','Trump'])]

def to_date(x):
    month, day, yr = x.str.split('/',expand=True).astype(int).values.T

    year = [i if i > 2000 else i + 2000 for i in yr]

    temp = pd.DataFrame()
    temp['year'] = year
    temp['month'] = month
    temp['day'] = day

    return pd.to_datetime(temp[['year','month','day']])

df['start_date'] = to_date(df['start_date'])
df['end_date'] = to_date(df['end_date'])
df['date'] = df['start_date'] + (df['end_date'] - df['start_date'])/2

df = df.loc[df.date>'2019-12-01']
df.to_csv('cleaned_polls.csv')

wis = df.loc[(df.state=='Wisconsin'),['date','answer','pct']].dropna(subset=['date']).sort_values('date')

plt.plot(wis.loc[wis.answer=='Biden','date'],lowess(wis.loc[wis.answer=='Biden','pct'],wis.loc[wis.answer=='Biden','date'],return_sorted=False,frac=0.1),label='Biden')
plt.scatter(wis.loc[wis.answer=='Biden','date'],wis.loc[wis.answer=='Biden','pct'],color='C0',s=3,alpha=0.2)
plt.plot(wis.loc[wis.answer=='Trump','date'],lowess(wis.loc[wis.answer=='Trump','pct'],wis.loc[wis.answer=='Trump','date'],return_sorted=False,frac=0.1),label='Trump',color='red')
plt.scatter(wis.loc[wis.answer=='Trump','date'],wis.loc[wis.answer=='Trump','pct'],color='red',s=3,alpha=0.2)
plt.legend()
plt.show()

cols = pd.MultiIndex.from_product([df.state.unique(),['Biden','Trump']])
output = pd.DataFrame(index = pd.date_range('2020-01-01','2020-11-03',freq='MS'),columns=cols)
for state in df.state.unique():
    st = df.loc[(df.state==state),['date','answer','pct']].dropna(subset=['date']).sort_values('date')

    output.loc[st.loc[st.answer=='Biden','date'],(state,'Biden')] = lowess(st.loc[st.answer=='Biden','pct'],st.loc[wis.answer=='Biden','date'],return_sorted=False,frac=0.1)
    output.loc[st.loc[st.answer=='Trump','date'],(state,'Trump')] = lowess(st.loc[st.answer=='Trump','pct'],st.loc[wis.answer=='Trump','date'],return_sorted=False,frac=0.1)



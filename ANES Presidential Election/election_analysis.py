from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf


columns = {
    'CASEID': 'id',
    'VCF0004': 'year',
    'VCF0009Z': 'wgt',
    'VCF0102': 'age_group',
    'VCF0105B': 'race',
    'VCF0110': 'educ',
    'VCF0114': 'faminc',
    'VCF0704A': 'vote',
    'VCF0803': 'libcon',
    'VCF0806': 'gov_ins',
    'VCF0809': 'jg'
}

df = pd.read_csv('sub-data.csv').rename(columns=columns)

df['age_group'].replace(range(0,8),[np.nan,'17 - 24','25 - 34','35 - 44','45 - 54','55 - 64','65 - 74','75 - 99'],inplace=True)
df['race'].replace(list(range(0,5))+[9],[np.nan,'White non-hispanic','Black non-hispanic','Hispanic','Other',np.nan],inplace=True)
df['educ'].replace(range(0,5),[np.nan,'High school or less','High school or less','Some college','Degree'],inplace=True)
df['faminc'].replace(range(0,6),[np.nan,'0 - 16 percentile','17 - 33 percentile','34 - 67 percentile','68 - 95 percentile','96 - 100 percentile'],inplace=True)
df['vote'].replace([1,2],[1,0],inplace=True)

res = smf.logit('vote ~ C(age_group) + C(race) + C(educ) + C(faminc) + libcon',df).fit()
res.summary()

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

df = pd.read_stata('Example data/cps_00165.dta')

df = df.query('ftotval>0 and ftotval!=999999').eval('income=ftotval/(famsize.cat.codes+1)')

def wtd_mean(data,x_name,w_name):
    x = data[x_name]
    w = data[w_name]

    try:
        return np.average(x,weights=w)
    except ZeroDivisionError:
        return np.mean(x)

def wtd_sd(data,x_name,w_name):

    x = data[x_name]
    w = data[w_name]

    diff = (x - wtd_mean(data,x_name,w_name))**2
    n = len(x)

    return np.sqrt(n/(n-1)*np.sum(w*diff)/np.sum(w))

def wtd_zscore(data,x_name,w_name):
    return (data[x_name] - wtd_mean(data,x_name,w_name))/wtd_sd(data,x_name,w_name)

def ungroup(data):
    return pd.concat([data.get_group(i) for i in data.groups])

def merge(data,value_name,parent):
    res = parent.set_index(data.index.name)

    res[value_name] = data

    return res.reset_index()

df.groupby('year').apply(wtd_zscore,'income','asecwt').pipe(merge,'mean_income',df)
from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

df = pd.read_csv('swiid8_3_summary.csv').set_index(['country','year'])

def se_plot2(data,country):
    dat = data.loc[data.country==country]

    fig, ax = plt.subplots()
    ax.plot(dat.year,dat.gini_disp,label='Disposable Income')
    ax.fill_between(dat.year,dat.gini_disp+dat.gini_disp_se,dat.gini_disp-dat.gini_disp_se,alpha=0.5)

    ax.plot(dat.year,dat.gini_mkt,label='Market Income')
    ax.fill_between(dat.year,dat.gini_mkt+dat.gini_mkt_se,dat.gini_mkt-dat.gini_mkt_se,alpha=0.5)

    ax.legend(title='Income Definition')
    ax.set_title(f'Inequality in {country}')

    return ax

countries = ['United States','United Kingdom','Canada','Sweden','Denmark','Finland']

fig, (ax0,ax1) = plt.subplots(1,2,sharex=True,sharey=True,constrained_layout=True)
for idx, country in enumerate(countries):
    if idx < 3:
        ax0.plot(df.loc[country,'gini_disp'],label=country)

        ax0.fill_between(df.loc[country].index,df.loc[country,'gini_disp'] + df.loc[country,'gini_disp_se'],df.loc[country,'gini_disp'] - df.loc[country,'gini_disp_se'],alpha=0.25)
    else: 
        ax1.plot(df.loc[country,'gini_disp'],label=country)

        ax1.fill_between(df.loc[country].index,df.loc[country,'gini_disp'] + df.loc[country,'gini_disp_se'],df.loc[country,'gini_disp'] - df.loc[country,'gini_disp_se'],alpha=0.25)


ax0.legend(title='Anglosphere')
ax1.legend(title='Nordics')
plt.suptitle('Gini for disposable income')
plt.show()
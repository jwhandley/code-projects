import pandas as pd
import numpy as np
from linearmodels import PanelOLS

cbi = pd.read_stata('cbi_dataset.dta')
cbi.loc[cbi.cname=='Korea, Republic of','cname'] = 'Korea'
cbi['country'] = cbi['cname']
ictwss = pd.read_stata('ICTWSS_v6_Stata_release.dta')
ictwss.loc[ictwss.country=='Korea, Republic of','country'] = 'Korea'
ictwss['coord'] = ictwss['Coord']

inst = pd.merge(cbi[['country','year','cuk_ceo','cuk_obj','cuk_pol','cuk_limlen']],ictwss[['country','year','coord']])
inst['cbi'] = inst[['cuk_ceo','cuk_obj','cuk_pol','cuk_limlen']].mean(axis=1)
inst = inst[['country','year','cbi','coord']]


cpi = pd.read_csv('oecd_cpi.csv')
unrate = pd.read_csv('oecd_unrate.csv')
cpi.loc[cpi.country=='United States','country'] = 'United States of America'
unrate.loc[unrate.country=='United States','country'] = 'United States of America'

oecd = pd.merge(cpi,unrate)
oecd['unrate_lag'] = oecd.groupby('country')['unrate'].shift()
oecd['cpi_lag'] = oecd.groupby('country')['cpi'].shift()

data = pd.merge(oecd,inst).set_index(['country','year'])

res_cpi = PanelOLS.from_formula('cpi ~ 1 + cbi*coord + TimeEffects',data.dropna())
res_cpi.fit()

res_unrate = PanelOLS.from_formula('unrate ~ 1 + cbi*coord + TimeEffects',data.dropna())
res_unrate.fit()
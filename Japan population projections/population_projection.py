from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

pop_init = pd.read_csv('pop_2019.csv')
pop_init['year'] = 2019
pop_init.set_index(['year','sex','age'],inplace=True)
pop_init.sort_index(level=['year','sex','age'], ascending=[1,1,1], inplace=True)
mort = pd.read_csv('mort_prediction.csv',index_col=0)
mort.set_index(['year','age','sex'],inplace=True)
fert = pd.read_csv('fertility_2015.csv')
fert.set_index('age',inplace=True)
immigration = pd.read_csv('met_migration_profile.csv')
immigration.set_index(['age','sex'],inplace=True)

pop = pd.DataFrame(index=mort.index[4:],columns=pop_init.columns)
pop.sort_index(level=['year','age','sex'],ascending=[1,1,0],inplace=True)

pop.loc[2019] = pop_init

death_jpn = pop.loc[2019,'jpn']*mort.loc[2019,'mort']
death_imm = pop.loc[2019,'imm']*mort.loc[2019,'mort']
birth = (pop.loc[(slice(15,49),1),'tot']*fert['fert']).sum()


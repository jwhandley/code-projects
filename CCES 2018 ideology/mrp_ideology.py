import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from bambi import Model
import arviz as az

indiv = pd.read_stata('individual_data.dta')

context = pd.read_csv('contextual_data.csv')
context['statefip'] = context['state']
context[['degree','nh_black','hispanic','dem16','evangelical']]*=1/100

pstrat = pd.read_stata('poststrat_data.dta')
post_data = pd.merge(pstrat,context)

gun_control = [f'gun_control{i}' for i in range(1,4)]
abortion = [f'abortion{i}' for i in range(1,7)]
immigration = [f'immigration{i}' for i in range(1,7)]
tax = [f'tax{i}' for i in range(1,7)]
health = [f'health{i}' for i in range(1,5)]
questions = [gun_control,abortion,immigration,tax,health]

reg_data = pd.merge(indiv,context).dropna(subset=gun_control + abortion + immigration + tax + health + ['statefip','region','sex','educ','faminc','race'])

for idx, question in enumerate(['gun_control','abortion','immigration','tax','health']):
    pca = PCA().fit(reg_data[questions[idx]])
    reg_data[question] = pca.transform(reg_data[questions[idx]])[:,0]

pca = PCA().fit(reg_data[['gun_control','abortion','immigration','tax','health']])
reg_data['ideology'] = pca.transform(reg_data[['gun_control','abortion','immigration','tax','health']])[:,0]
reg_data['econ'] = (reg_data['tax'] + reg_data['health'])/2
reg_data['social'] = (reg_data['abortion'] + reg_data['immigration'])/2



model = Model(reg_data)

reg_ideology = model.fit('ideology ~ degree + nh_black + hispanic + np.log(median_hh_income) + dem16',random=['1|statefip','1|region','1|sex','1|educ','1|faminc','1|race','0+race|statefip','0+race|faminc','0+sex|educ','0+race|educ','0+faminc|state'])

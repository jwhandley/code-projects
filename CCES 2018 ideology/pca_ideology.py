import pandas as pd
import numpy as np
from sklearn.decomposition import PCA

df = pd.read_stata('cces18_common_vv.dta')

gun_control = []
for idx, col in enumerate(['CC18_320a','CC18_320c','CC18_320d']):
    df[f'gun_control{idx+1}'] = df[col].cat.codes
    gun_control.append(f'gun_control{idx+1}')

abortion = []
for idx, col in enumerate(['CC18_321a','CC18_321b','CC18_321c','CC18_321d','CC18_321e','CC18_321f']):
    df[f'abortion{idx+1}'] = df[col].cat.codes
    abortion.append(f'abortion{idx+1}')

immigration = []
for idx, col in enumerate(['CC18_322a','CC18_322b','CC18_322c_new','CC18_322d_new','CC18_322c','CC18_322f']):
    df[f'immigration{idx+1}'] = df[col].cat.codes
    immigration.append(f'immigration{idx+1}')

tax = []
for idx, col in enumerate(['CC18_325a','CC18_325b','CC18_325c','CC18_325d','CC18_325e_new','CC18_325f_new']):
    df[f'tax{idx+1}'] = df[col].cat.codes
    tax.append(f'tax{idx+1}')

health = []
for idx, col in enumerate(['CC18_327a','CC18_327c','CC18_327d','CC18_327e']):
    df[f'health{idx+1}'] = df[col].cat.codes
    health.append(f'health{idx+1}')

questions = [gun_control,abortion,immigration,tax,health]

for idx, question in enumerate(['gun_control','abortion','immigration','tax','health']):
    pca = PCA().fit(df[questions[idx]])
    df[question] = pca.transform(df[questions[idx]])[:,0]

pca = PCA().fit(df[['gun_control','abortion','immigration','tax','health']])
df['ideology'] = pca.transform(df[['gun_control','abortion','immigration','tax','health']])[:,0]

def wtd_mean(x,w):
    return np.sum(x*w)/np.sum(w)

def wtd_std(x,w):
    return np.sqrt(np.sum(w*(x-wtd_mean(x,w)**2))/np.sum(w)*len(w)/(len(w)-1))
    


for col in ['gun_control','abortion','immigration','tax','health','ideology']:
    df[col] = (df[col] - wtd_mean(df[col],df['commonweight']))/wtd_std(df[col],df['commonweight'])

output = pd.DataFrame(index=df.inputstate.unique(),columns=['gun_control','abortion','immigration','tax','health','ideology'])
for state in df.inputstate.unique():
    for col in ['gun_control','abortion','immigration','tax','health','ideology']:
        output.loc[state,col] = wtd_mean(df.loc[df.inputstate==state,col],df.loc[df.inputstate==state,'commonweight'])

output.to_csv('state_ideology_python.csv')
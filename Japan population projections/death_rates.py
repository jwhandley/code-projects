import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from statsmodels.tsa.arima_model import ARIMA

df = pd.read_csv('death_ts_cleaned.csv')

forecast_interval = 50
year_start = 2015

out = {}

for sex in [0,1]:
    data = df.loc[df['sex']==sex].pivot(index='year',columns='age',values='mort')

    mean = np.log(data).mean()

    std = np.log(data) - mean

    pca_fit = PCA(n_components=1).fit(std)
    pca = pca_fit.transform(std)

    model = ARIMA(pca[20:],order=(5,1,0)).fit(disp=0)
    pred = model.forecast(forecast_interval)[0].reshape(forecast_interval,1)
    out[sex] = np.exp(pca_fit.inverse_transform(pred) + np.array(mean).reshape(1,101))

male = pd.DataFrame(out[0],index=range(year_start,year_start+forecast_interval),columns=range(0,101))
male['sex'] = 0
female = pd.DataFrame(out[1],index=range(year_start,year_start+forecast_interval),columns=range(0,101))
female['sex'] = 1

data = female.append(male)

prediction = pd.melt(data.reset_index(),id_vars=['index','sex'],var_name='age',value_name='mort').rename(columns={'index':'year'})
prediction.to_csv('mort_prediction.csv')
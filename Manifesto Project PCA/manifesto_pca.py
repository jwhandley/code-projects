from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA

df = pd.read_stata('MPDataset_MPDS2020a_stata14.dta',convert_categoricals=False)

pers = [f'per{100+i}' for i in range(1,11)] + [f'per{200+i}' for i in range(1,5)] + [f'per{300+i}' for i in range(1,6)] + [f'per{400+i}' for i in range(1,16)] + [f'per{500+i}' for i in range(1,8)] + [f'per{600+i}' for i in range(1,9)] + [f'per{700+i}' for i in range(1,7)] + [f'per{1010+i}' for i in range(1,7)] + [f'per{1020+i}' for i in range(1,7)] + [f'per{1030+i}' for i in range(1,4)] + [f'per{2020+i}' for i in range(1,4)] + [f'per{2030+i}' for i in range(1,4)] + ['per2041','per3011'] + [f'per{3050+i}' for i in range(1,6)] + [f'per{4010+i}' for i in range(1,4)] + [f'per{4120+i}' for i in range(1,5)] + [f'per{4130+i}' for i in range(1,3)] + ['per5021','per5031','per5041','per5061'] + [f'per{6010+i}' for i in range(1,5)] + ['per6061'] + [f'per{6070+i}' for i in range(1,3)] + ['per6081','per7051','per7052','per7061','per7062']
data = df.dropna(subset=pers)

pca = PCA().fit(data[pers])

#plt.plot(range(1,10),pca.explained_variance_ratio_[0:9],marker='o')
#plt.show()

data['dim1'] = pca.transform(data[pers])[:,0]
data['dim2'] = pca.transform(data[pers])[:,1]

data['year'] = np.round(data['date']/100)

uk19 = data.loc[(data['countryname']=='United Kingdom') & (data['year']==2019),['dim1','dim2','rile','partyname']]
uk19
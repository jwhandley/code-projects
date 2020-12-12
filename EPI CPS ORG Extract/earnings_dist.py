from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
from tqdm import tqdm

years = range(1980,2020,1)

df = pd.read_stata('epi_cpsorg_1979_2020/epi_cpsorg_1979.dta',convert_categoricals=False)

for year in tqdm(years):
    df = pd.concat([df,pd.read_stata(f'epi_cpsorg_1979_2020/epi_cpsorg_{year}.dta',convert_categoricals=False)])
    print(f'Added {year}: {len(df)}')

print(len(df))
df.head()

df.to_stata('epi_org_1979_2019.dta')
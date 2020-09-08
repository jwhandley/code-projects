from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

# Read data
df = pd.read_stata('data/cps_00166.dta')

# Clean up data
df['day'] = 1
df['month'] = df['month'].cat.codes+1
df['date'] = pd.to_datetime(df[['year','month','day']])
df['age'] = df['age'].cat.codes + int(df['age'].min())
df['employed'] = (df['empstat']=='at work').astype(int)

# Function for getting weighted averages from pandas dataframes and groups
def wtd_mean(data,x_name,w_name):
    x = data[x_name]
    w = data[w_name]

    try:
        return np.average(x,weights=w)
    except ZeroDivisionError:
        return np.mean(x)

def plot_by_groups(data,x,y,group):
    group_data = data.groupby(group)
    groups = group_data.groups

    plots = []
    for i in groups:
        plots.append(group_data.get_group(i).plot(x=x,y=y))

    return plots

# Output a dataframe with columns for each group (year, age, and sex) and the calculated values (employment rates)
res = df.groupby(['year','age','sex']).apply(wtd_mean,'employed','wtfinl').reset_index().rename(columns={0:'employment_rate'})

# Plot employment rates by sex and age

res.pivot(index=['year','age'],columns='sex',values='employment_rate')
plt.show()
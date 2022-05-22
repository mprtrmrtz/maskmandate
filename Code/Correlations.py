

import pandas as pd
import seaborn as sns
import os
from string import ascii_letters
import numpy as np
import matplotlib.pyplot as plt

os.chdir("C:/Users/momaleki/Desktop/Research/Project/Data/ToMerge")
print(os.getcwd())



#%%

df = pd.read_csv("df_updated.csv")


print(df.columns)

# test = df[["AL", "HL", "BD", "UN", "MO", "RL", "PD", "PO", "CC", "PW", "CT", "VA", "CR"]]
dependents = df[["C19-MO",  "C19-MC", "C19-VC", "PP-DW"]]
independents_1 = df[["EL-MC", "PP-DW", "EC-UN", "PS-PD", "EC-PO"]]
independents_2 = df[["PP-DW" , "C19-CC", "HC-PW", "PS-RL", "HC-HO"]]

print(dependents.head)

#%% Pairwise Plot
sns.pairplot(dependents,
             hue = "PP-DW", 
             palette=sns.color_palette("RdBu", 2))

#%%
sns.pairplot(independents_1,
             hue = "PP-DW", 
             palette=sns.color_palette("RdBu", 2))

sns.pairplot(independents_2,
             hue = "PP-DW", 
             palette=sns.color_palette("RdBu", 2))



#%% Correlation Plot 

sns.set_theme(style="white")

d = df
# Compute the correlation matrix
corr = d.corr()

# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=bool))

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap

cmap = sns.color_palette("vlag",  8) 


# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=1 , vmin = -1,  center=0,
            square=True, linewidths=.5, cbar_kws = dict(use_gridspec=True,location="right",
                                                       shrink = 0.3))

plt.legend(loc='upper center')

 










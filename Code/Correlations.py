import pandas as pd
import seaborn as sns
import os
# from string import ascii_letters
import numpy as np
import matplotlib.pyplot as plt

os.chdir("G:/My Drive/Research/Mask Mandate/")
print(os.getcwd())



#%%

df = pd.read_csv("Data/ToMerge/df_unscaled.csv")

del df['FIPS']

print(df.columns)

# df.columns = ['C19-MN', 'C19-MR', 'C19-MS', 'C19-MF', 'C19-MA',
#               'Mask Adoption Score', 'EL-LC',
#        'Education Levels', 'PP-RE', 'PP-DE', 'PP-DL', 
#        'Political Preference', 'EC-IN', 'Unemployment',
#        'Poverty Levels',
#        'Population Density', 'COVID-19 Cases', 'C19-DD', 
#        'Vaccination (%)', 'Movement Change', 'C19-PT',
#        'Essential Workers (%)',
#        'EL-IN', 'Hospital Capacity', 'HC-HB', 'C19-TT', 
#        'Rurality (%)', 'C19-TC111']

# test = df[["AL", "HL", "BD", "UN", "MO", "RL", "PD", "PO", "CC", "PW", 
# "CT", "VA", "CR"]]

dependents = df[['C19-MO',  'C19-MC',
                 "C19-VC", "PP-DW"]]
independents_1 = df[["EL-MC",  "EC-UN",
                     "PS-PD", "EC-PO", 
                     "PP-DW"]]
independents_2 = df[[ "C19-CC", "HC-PW",
                     "PS-RL", "HC-HO", 
                     "PP-DW"]]

print(dependents.head)

#%% Pairwise Plot
sns.pairplot(dependents,
             hue = "PP-DW", 
             palette = {1: "blue", 0: "red"}, 
             kind = 'scatter', 
             diag_kind = 'auto', 
             height = 4
            # palette = sns.color_palette("coolwarm", 2)
            )
            
plt.savefig('test_pdf_0821_1.pdf', format = 'pdf', 
            dpi=720)

#%%
sns.pairplot(independents_1,
              hue = "PP-DW", 
             palette = {1: "blue", 0: "red"}, 
             kind = 'scatter', 
             diag_kind = 'auto', 
             height = 3)

plt.savefig('test_pdf_0821_2.pdf', format = 'pdf', 
            dpi=720)


sns.pairplot(independents_2,
              hue = "PP-DW", 
             palette = {1: "blue", 0: "red"}, 
             kind = 'scatter', 
             diag_kind = 'auto', 
             height = 3)

plt.savefig('test_pdf_0821_3.pdf', format = 'pdf', 
            dpi=720)

#%% Correlation Plot 

sns.set_theme(style="white")
df = df[["C19-TT", "C19-CC", "C19-DD", "C19-TC", "C19-MC", "C19-MA", "C19-MF", "C19-MS", 
         "C19-MR", "C19-MN", "C19-MO", "C19-PT", "C19-VC", 
         "EL-LC", "EL-MC", "EL-IN", 
         "HC-HB", "HC-HO", "HC-PW", 
         "EC-PO", "EC-UN", "EC-IN", 
         "PP-DW", 
         "PS-PD", "PS-RL"]]
d = df
# Compute the correlation matrix
corr = d.corr()

# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=bool))

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap

cmap = sns.color_palette("vlag_r",  7) 

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=1 , vmin = -1,  center=0,
            square=True, linewidths=.5, 
            cbar_kws = dict(use_gridspec=True,location="right",
                                                       shrink = 0.3))

plt.legend(loc='upper center')

plt.savefig('test_pdf_0821_4.eps', format = 'eps', 
             dpi=720)










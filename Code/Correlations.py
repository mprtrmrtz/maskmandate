

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

os.chdir("C:/Users/momaleki/Desktop/Project/Data/Correlations")
print(os.getcwd())



#%%

df = pd.read_csv("df.csv")


print(df.columns)

test = df[["AL", "HL", "BD", "UN", "MO", "RL", "PD", "PO", "CC", "PW", "CT", "VA", "CR"]]
dependents = df[["AL",  "MO", "VA", "BD"]]
independents_1 = df[["HL", "BD", "UN", "PD", "PO"]]
independents_2 = df[["BD" , "CC", "PW", "CT",  "CR"]]


#%%
sns.pairplot(dependents,
             hue = "BD", 
             palette={1:"royalblue", 
                      0:"red"})




#%%

sns.pairplot(independents_1,
             hue = "BD", 
             palette={1:"royalblue", 
                      0:"red"})

sns.pairplot(independents_2,
             hue = "BD", 
             palette={1:"royalblue", 
                      0:"red"})


















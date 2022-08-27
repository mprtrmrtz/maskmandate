import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import requests
import urllib
from collections import Counter
import googlemaps
from datetime import datetime

os.chdir("C:/Users/momaleki/Desktop/Research/Project")
os.getcwd()

pd.set_option('display.width', 1000)
pd.set_option('display.max_columns', None)

Federal_Raw = pd.read_csv("Data/Federal Education Investment.csv")
Campus = pd.read_csv("Data/InstitutionCampus.csv")

Campus =  Campus.iloc[:, [3, 7]]
Campus.set_axis(["Recipient", "Address"], inplace = True, axis = 1)


Federal_Raw = pd.read_csv("Data/Federal Education Investment.csv")
Federal_Raw = Federal_Raw.iloc[:, [0, 7]]



merged =  pd.merge(Campus,Federal_Raw, on='Recipient')

merged = merged.groupby(['Recipient', 'Address']).aggregate(np.sum)
merged = merged.reset_index()

gmaps = googlemaps.Client(key = "###############")

for i in range(0, merged.shape[0]):
    address = merged.iloc[i, 1]
    result = gmaps.geocode(address)
    result = result[0]
    merged.loc[i, 'Lat'] = result['geometry']['location']['lat']
    merged.loc[i, 'Lon'] = result['geometry']['location']['lng']
    print(i, " ", result['geometry']['location']['lat'], " ", result['geometry']['location']['lng'], "\n")


merged.to_csv("merged_lonlat_added.csv")


df  = merged.copy()

start = 0
end = df.shape[0]


for i in range(start, end):
    try:
        params = urllib.parse.urlencode({'latitude': df['Lat'].iloc[i,],
                                         'longitude': df['Lon'].iloc[i,], 
                                         'format':'json'})
        url = 'https://geo.fcc.gov/api/census/block/find?'  + params 
        response =  requests.get(url)
        data = response.json()
        df.loc[i, 'FIPS'] = data['County']['FIPS']
        print(i, " ", data['County']['FIPS'])

    except KeyError:
        df.loc[i, 'FIPS'] = 0



df.to_csv("Federal_Investment_PYTHON.csv")










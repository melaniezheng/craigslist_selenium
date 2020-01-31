import glob
import pandas as pd
import csv
import re

# get data file names

path ='./data'
filenames = glob.glob(path + "/*.tsv")

dfs = []
for filename in filenames:
    location=filename.split('/')[2].split('_')[0].capitalize()
    if location == 'Sfbay':
        location = 'San Francisco'
    elif location == 'Sanantonio':
        location = 'San Antonio'
    elif location == 'Losangeles':
        location = 'Los Angeles'
    elif location == 'Newyork':
        location = 'New York'
    temp = pd.read_csv(filename,sep='\t')
    temp['city'] = location
    dfs.append(temp)

big_df = pd.concat(dfs, ignore_index=True)
big_df = big_df.drop(big_df[big_df['price']=='price'].index,axis=0)
big_df['price'] = big_df['price'].str.strip('$').str.strip().astype(int)
big_df['time'] = big_df['date'].str.slice(start=11)
big_df['date'] = big_df['date'].str.slice(stop=10)

big_df.to_csv("data.tsv", sep='\t')
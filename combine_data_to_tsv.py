import glob
import pandas as pd
import csv
import re

# get data file names
path ='./data'
filenames = glob.glob(path + "/*.tsv")

dfs = []
for filename in filenames:
    # clean up city names
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
    # add a column
    temp['city'] = location
    dfs.append(temp)

big_df = pd.concat(dfs, ignore_index=True)
# drop dupe headers
big_df = big_df.drop(big_df[big_df['price']=='price'].index,axis=0)
# clean up price, time, date
big_df['price'] = big_df['price'].str.strip('$').str.strip().astype(int)
big_df['time'] = big_df['date'].str.slice(start=11)
big_df['date'] = big_df['date'].str.slice(stop=10)
big_df['bedrooms'] = big_df['bedrooms'].str.strip()
big_df['neighborhood'] = big_df['neighborhood'].str.strip().str.lower().str.replace(r'\(|\)|_|-', '').str.replace('/','&')
big_df = big_df.drop(big_df[big_df['neighborhood'].isin(['jersey city','north newark, nj','union city, nj','roselle park, new jersey','pelham'])].index,axis=0)


big_df.to_csv("data.tsv", sep='\t')
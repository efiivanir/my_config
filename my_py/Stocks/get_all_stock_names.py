# -*- coding: utf-8 -*-
import sys
import os
import re
from subprocess import call
import urllib.request
import pandas as pd

def create_pd(*args):
    all = {}
    for path in args:
        market_name = path.split('&')[1].replace('exchange=','')
        csv_file = market_name + ".csv"
        urllib.request.urlretrieve(path, csv_file)
        df = pd.read_csv(path,header=0);
        all[market_name] = df

    for key in all.keys():
        print("\n" +"="*40)
        print(key)
        print("-"*40)
        print(all[key]['Symbol'],all[key]['Name'])
    
        
        
def main():
    nasdaq_csv_path = "https://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download"
    nyse_csv_path =   "https://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download"
    amex_csv_path =   "https://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amex&render=download"
    
    create_pd(nasdaq_csv_path,nyse_csv_path,amex_csv_path)
  
if __name__== "__main__":
  main()
    

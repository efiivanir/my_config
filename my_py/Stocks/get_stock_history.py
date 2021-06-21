import sys
import os
import re
from subprocess import call
import pandas as pd
import datetime
from pandas_datareader import data as pdr
import fix_yahoo_finance as yf

def get_ticket_history(ticket):
    start = datetime.datetime(2016,1,1)
    end = datetime.date.today()
    data = pdr.get_data_yahoo(ticket, start=start, end=end)
    print(data)

def main():
    get_ticket_history('MLNX')
    get_ticket_history('INTC')
  
if __name__== "__main__":
  main()
    



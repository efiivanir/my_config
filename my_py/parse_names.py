# -*- coding: utf-8 -*-
import sys
import os
import re
from bs4 import BeautifulSoup

html_f = 'all_names.txt'
female_f = 'female.csv'
male_f = 'male.csv'

f = open(male_f,"w")
with(open(html_f,"r")) as fp:
    
    for line in fp:
        pattern = 'בנים'
        results = re.search(pattern,line)
        if results:
            line_l = []
            line = re.sub(pattern,'',line)
            line = re.sub('\:',' ',line)
            line = re.sub('\n',' ',line)
            line = re.sub('\.',' ',line)
            line = re.sub('\,',' ',line)
            line_l = re.split('\s+', line)
            for world in line_l:
                if not re.search('\s+',world):
                    f.write(world + "\n")
f.close
fp.close()
    

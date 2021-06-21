import sys
import os
from random import seed
from random import random
from random import choices

import numpy as np

# os.chdir("/home/ivanir/.emacs.d/config/my_py")
os.chdir("/root/.emacs.d/config/my_py")
from HebrewNames import Males, Females, Sure, Cities, Streets
from EmployeRec import EmployeRec

males = Males()
males_names = males.get_names()
females = Females()
females_names = females.get_names()
sure = Sure()
sure_names = sure.get_names()
cities = Cities()
cities_names = cities.get_names()

streets = Streets()
streets_names = streets.get_names()


# Create 20000 employes
employesDic = {}
ids = np.random.randint(10000000, 1000000000, 20000)
house_numbers = np.random.randint(1, 100, 20000)
males_first_names = np.random.choice(males_names, 10000)
females_first_names = np.random.choice(females_names, 10000)
last_names = np.random.choice(sure_names, 20000)
cities_sample = np.random.choice(cities_names, 20000)
streets_sample = np.random.choice(streets_names, 20000)

index = 1
for i in ids:
    idN = str(i).zfill(10)
    employesDic[idN] = EmployeRec(idN)
    if index <= 10000:
        employesDic[idN].set_gender("F")
        employesDic[idN].set_first_name(females_first_names[index - 1])
    else:
        employesDic[idN].set_gender("M")
        employesDic[idN].set_first_name(males_first_names[index - 10000 - 1])
    employesDic[idN].set_last_name(last_names[index - 1])
    employesDic[idN].set_cities(cities_sample[index - 1])
    employesDic[idN].set_streets(streets_sample[index - 1])
    employesDic[idN].set_house_number(house_numbers[index - 1])
    index = index + 1
    print(employesDic[idN])

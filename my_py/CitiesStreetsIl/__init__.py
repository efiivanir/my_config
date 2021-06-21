import os
import pandas as pd 

CSV_FILE = 'rechovot_2_20200401.csv'

class CitiesStreetsIl:
    def __init__(self):
        csv_file = os.path.dirname(__file__) + "/" + CSV_FILE   
        data = pd.read_csv(csv_file)
        data.replace('(^\s+|\s+$)', '', regex=True, inplace=True)
        self.data = data
        
    def get_table(self):
        return self.data
    
    def get_regions(self):
        return [item for item in set(self.data.region_name)]

    def get_cities(self,region):
        is_region = self.data.region_name == region
        tmp = self.data[is_region]
        return [item for item in set(tmp.city_name)]

    def get_streets(self,region,city):
        is_region = self.data.region_name == region
        tmp = self.data[is_region]
        is_city = tmp.city_name == city
        tmp1 = tmp[is_city]
        return [item for item in set(tmp1.street_name)]
    

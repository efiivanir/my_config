import os
import csv


class Streets:
    def __init__(self):
        current_file = __file__
        base_dir = os.path.dirname(current_file)
        csv_file = base_dir + "/" + "streets.csv"
        self.csv_file = csv_file

    def get_names(self):
        with open(self.csv_file, newline="") as f:
            reader = csv.reader(f)
            tmp = list(reader)
            data = [num for elem in tmp for num in elem]
            return data

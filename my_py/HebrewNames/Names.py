from pathlib import Path
import inspect
import os
import random


class Names:
    """ This class use the folowing files:
    1. Last_Names.txt
    2. First_Names.txt
    Each file include list of 1000 last names and first names
    """

    def __init__(self):
        self.first_names_f = "First_Names.txt"
        self.last_names_f = "Last_Names.txt"
        self.max_names = 1000

    def first_names(self):
        root_dir = os.path.dirname(inspect.getfile(Names))
        file = root_dir + '/' + self.first_names_f
        f = open(file, 'r')
        words = list(f.read().split())
        f.close()
        return words

    def last_names(self):
        root_dir = os.path.dirname(inspect.getfile(Names))
        file = root_dir + '/' + self.last_names_f
        f = open(file, 'r')
        words = list(f.read().split())
        f.close()
        return words

    def random_first_names(self, n=10):
        if n >= self.max_names:
            raise ValueError(
                'Max Number of elements needs to be ' + str(self.max_names))
        if n < 1:
            raise ValueError('Number of elements needs to be at least 1')

        words = self.first_names()
        my_dict = {}
        while len(my_dict) < n:
            i = random.randint(0, self.max_names - 1)
            if not words[i] in my_dict:
                my_dict[words[i]] = 1
        return list(my_dict.keys())

    def random_last_names(self, n=10):
        if n >= self.max_names:
            raise ValueError(
                'Max Number of elements needs to be ' + str(self.max_names))
        if n < 1:
            raise ValueError('Number of elements needs to be at least 1')

        words = self.last_names()
        my_dict = {}
        while len(my_dict) < n:
            i = random.randint(0, self.max_names - 1)
            if not words[i] in my_dict:
                my_dict[words[i]] = 1
        return list(my_dict.keys())

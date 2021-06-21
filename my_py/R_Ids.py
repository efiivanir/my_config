from pathlib import Path
import inspect
import os
import random


class R_Ids:
    """ This class generate IDs number in a certian length
    """

    def __init__(self, n=9):
        self.length = n
        self.max_numbers = 10**n - 1

    def random_ids(self, n=10):
        if n >= self.max_numbers:
            raise ValueError(
                'Max Number of elements needs to be ' + str(self.max_numbers))
        if n < 1:
            raise ValueError('Number of elements needs to be at least 1')

        my_dict = {}
        while len(my_dict) < n:
            id = list()
            while len(id) < self.length:
                i = random.randint(0, 9)
                id.append(i)
            id_str = ''.join(str(e) for e in id)
            if not id_str in my_dict:
                my_dict[id_str] = 1
        return list(my_dict.keys())

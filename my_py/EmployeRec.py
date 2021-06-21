class EmployeRec:
    def __init__(self, idN):
        self.idN = idN

    def get_id(self):
        return self.idN

    def set_gender(self, gender):
        self.gender = gender

    def get_gender(self):
        return self.gender

    def set_first_name(self, first_name):
        self.first_name = first_name

    def get_first_name(self):
        return self.first_name

    def set_last_name(self, last_name):
        self.last_name = last_name

    def get_last_name(self):
        return self.last_name

    def set_cities(self, cities):
        self.cities = cities

    def get_cities(self):
        return self.cities

    def set_streets(self, streets):
        self.streets = streets

    def get_streets(self):
        return self.streets

    def set_house_number(self, house_number):
        self.house_number = house_number

    def get_house_number(self):
        return self.house_number

    def __str__(self):
        return "{0} | {1} | {2} | {3} | {4} | {5} | {6}".format(
            self.idN,
            self.gender,
            self.first_name,
            self.last_name,
            self.cities,
            self.streets,
            self.house_number,
        )

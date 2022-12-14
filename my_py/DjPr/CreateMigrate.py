import os
import sys


def create_migrate():
    os.system(f"./venv/bin/python manage.py makemigrations")
    os.system(f"./venv/bin/python manage.py migrate")
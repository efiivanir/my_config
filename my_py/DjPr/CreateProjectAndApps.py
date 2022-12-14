import os
import sys
from messages import message_info,message_error

def create_django_project(proj):
    message_info(f"Create django project {proj}")
    os.system(f"./venv/bin/django-admin startproject {proj} .")

def create_django_app(app):
    message_info(f"Create django app {app}")
    os.system(f"./venv/bin/python manage.py startapp {app}")
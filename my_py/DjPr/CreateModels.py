import os
import sys
from messages import message_info,message_error


def create_accounts_models():
    models_file = 'accounts/models.py'
    message_info(f"Create {models_file}")
    data = '''
from django.contrib.auth.models import AbstractUser
from django.db import models

class CustomUser(AbstractUser):
    date_of_birth = models.DateField(blank=True, null=True)
    id_number = models.CharField(blank=True, null=True,max_length=9)
    residence_city = models.CharField(blank=True, null=True,max_length=30)
    residence_street = models.CharField(blank=True, null=True,max_length=40)
    residence_house_num = models.CharField(blank=True, null=True,max_length=10)
    '''
    with open(models_file, "w") as fp:
        fp.write(data)


def create_models():
    create_accounts_models()

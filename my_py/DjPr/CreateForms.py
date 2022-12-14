import os
import sys
from messages import message_info,message_error


def create_accounts_forms():
    forms_file = 'accounts/forms.py'
    message_info(f"Create {forms_file}")
    data = '''
from django.contrib.auth.forms import UserCreationForm, UserChangeForm
from .models import CustomUser


class CustomUserCreationForm(UserCreationForm):
    class Meta(UserCreationForm):
        model = CustomUser
        fields = UserCreationForm.Meta.fields + ("id_number","date_of_birth",
            "residence_city","residence_street","residence_house_num",)
    
    
class CustomUserChangeForm(UserChangeForm):
    class Meta:
        model = CustomUser
        fields = UserChangeForm.Meta.fields
    '''
    with open(forms_file, "w") as fp:
        fp.write(data)


def create_forms():
    create_accounts_forms()

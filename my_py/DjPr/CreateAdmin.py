import os
import sys
from messages import message_info,message_error


def create_accounts_admin():
    admin_file = 'accounts/admin.py'
    message_info(f"Create {admin_file}")
    data = '''
from django.contrib import admin
from django.contrib.auth.admin import UserAdmin
from .forms import CustomUserCreationForm, CustomUserChangeForm
from .models import CustomUser


class CustomUserAdmin(UserAdmin):
    add_form = CustomUserCreationForm
    form = CustomUserChangeForm
    model = CustomUser
    list_display = [
        "username",
        "email",        
        "is_staff",
        "id_number",
        "date_of_birth",
    ]
    fieldsets = UserAdmin.fieldsets + ((None, {"fields": 
        ("id_number","date_of_birth","residence_city","residence_street","residence_house_num")}),)
    add_fieldsets = UserAdmin.add_fieldsets + ((None, {"fields": 
        ("id_number","date_of_birth",)}),"residence_city","residence_street","residence_house_num")
    
    
admin.site.register(CustomUser, CustomUserAdmin)
    '''
    with open(admin_file, "w") as fp:
        fp.write(data)


def create_admin():
    create_accounts_admin()

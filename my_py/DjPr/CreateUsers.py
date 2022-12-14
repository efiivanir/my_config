import os
import sys


os.environ["DJANGO_SUPERUSER_USERNAME"] = "admin"
os.environ["DJANGO_SUPERUSER_PASSWORD"] = "admin"
os.environ["DJANGO_SUPERUSER_EMAIL"] = "efi.ivanir.unsc@gmail.com"

def create_users():
    users = (
        "'pelegiv','pelegiv2@gmail.com','123456',first_name='Peleg',last_name='Ivanir'",
        "'ivanir','efi.ivanir@gmail.com','123456',first_name='Efi',last_name='Ivanir'",
        "'sungirl7','sungirl7@gmail.com','123456',first_name='Dana',last_name='Ivanir-Kalitzky'",
    )
    os.system(f"./venv/bin/python manage.py createsuperuser --noinput --skip-checks")
    shell_cmd = "./venv/bin/python manage.py shell"

    for u in users:
        os.system(f"{shell_cmd} -c \"from accounts.models import CustomUser; \
            user = CustomUser.objects.create_user({u});user.save()\"")



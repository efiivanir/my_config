import os
import sys
from messages import message_info,message_error



def change_settings_file(file,pattern, new_pattern):
    with open(file, "r") as f:
        data = f.read()
        data = data.replace(pattern, new_pattern)

    with open(file, "w") as f:
        f.write(data)

def add_to_settings_file(file,data):
    with open(file, "a") as f:
        f.write(data)

def settings_update(project,apps,sendgrid):
    settings_file = f"{project}/settings.py"
    message_info(f"Update {settings_file}")
    tmp = list(apps)
    tmp.append('accounts')
    for app in tmp:
        change_settings_file(
            file=settings_file,
            pattern='"django.contrib.staticfiles",',
            new_pattern=f"\"django.contrib.staticfiles\",\n    '{app}.apps.{app.capitalize()}Config',",
        )

    # Change time zone
    change_settings_file(
        file=settings_file,
        pattern='TIME_ZONE = "UTC"\n',
        new_pattern='TIME_ZONE = "Asia/Jerusalem"'
    )

    # add django extensions
    change_settings_file(
        file=settings_file,
        pattern='"django.contrib.staticfiles",',
        new_pattern=f"\"django.contrib.staticfiles\",\n    'django_extensions',",
    )

    change_settings_file(
        file=settings_file,
        pattern='"django.contrib.staticfiles",',
        new_pattern=f"\"django.contrib.staticfiles\",\n    'crispy_bootstrap5',",
    )

    change_settings_file(
        file=settings_file,
        pattern='"django.contrib.staticfiles",',
        new_pattern=f"\"django.contrib.staticfiles\",\n    'crispy_forms',",
    )

    change_settings_file(
        file=settings_file,
        pattern='"DIRS": [],',
        new_pattern='"DIRS": [BASE_DIR / "templates"],',
    )

    add_to_settings_file(
        file=settings_file,
        data='\nLOGIN_REDIRECT_URL = "home"\nLOGOUT_REDIRECT_URL = "home"\n'

    )
    add_to_settings_file(
        file=settings_file,
        data='AUTH_USER_MODEL = "accounts.CustomUser"\n',

    )

    add_to_settings_file(
        file=settings_file,
        data='CRISPY_ALLOWED_TEMPLATE_PACKS = "bootstrap5"\n',

    )

    add_to_settings_file(
        file=settings_file,
        data='CRISPY_TEMPLATE_PACK = "bootstrap5"\n',

    )

    if not sendgrid:
        add_to_settings_file(
            file=settings_file,
            data='EMAIL_BACKEND = "django.core.mail.backends.console.EmailBackend"\n',

        )



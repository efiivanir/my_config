import shutil
import os
import sys
from messages import message_info,message_error

def create_git(django_project):
    ignore_patterns = (
        '__pycache__/',
        'venv/',
        '.env',
        '.idea/',
    )
    ignore_file = '.gitignore'
    message_info(f"Create git")
    os.system("git init")
    with open(ignore_file, "w") as fp:
        for item in ignore_patterns:
            fp.write("%s\n" % item)
    os.system("git add --all")
    os.system('git commit -m "Init"')
    os.system('git branch -M main')
    os.system(f"git remote add origin https://github.com/efiivanir/{django_project}.git")

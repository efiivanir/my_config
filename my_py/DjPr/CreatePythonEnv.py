import shutil
import os
import sys
from messages import message_info,message_error
mode = 0o777

req_file = "requirements.txt"
packages = (
    "setuptools",
    "wheel",
    "ipython",
    "django",
    "django-extensions",
    "django-debug-toolbar",
    "django-crispy-forms",
    "crispy-bootstrap5",
    "pygraphviz",
    "werkzeug",
    "vobject",
    "pyparsing",
    "pydot",
    "black",
    "flake8",
)
def create_proj_dir(proj_dir,force):
    message_info(f"Create django directory {proj_dir}")
    if os.path.exists(proj_dir):
        if not force:
            message_error(f"directory {proj_dir} exists, please remove it, or use --force.")
            sys.exit()
        shutil.rmtree(proj_dir,ignore_errors = False)
    os.makedirs(proj_dir, mode)
    os.makedirs(f"{proj_dir}/templates/registration", mode)
    os.makedirs(f"{proj_dir}/static/css", mode)


def create_venv():
    # create venv env
    message_info(f"Create virtual environment.")
    os.system("python3 -m venv venv")

    with open(req_file, "w") as fp:
        for item in packages:
            fp.write("%s\n" % item)

    # install packages from the required
    os.system(f"./venv/bin/python -m pip install -r {req_file} -U")

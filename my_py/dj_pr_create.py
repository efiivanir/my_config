#!/usr/bin/python3
import os.path
import sys
import click

sys.path.insert(0,'/home/ivanir/my_config/my_py/DjPr')
from messages import message_info,message_error
from CreatePythonEnv import create_proj_dir,create_venv
from CreateProjectAndApps import create_django_project,create_django_app
from SettingsUpdate import settings_update
from CreateUrls import create_urls
from CreateHtmls import create_htmls
from CreateViews import create_views
from CreateMigrate import create_migrate

from CreateModels import create_models
from CreateForms import create_forms
from CreateAdmin import create_admin
from CreateGit import create_git


@click.command()
@click.option('--project','-p', help='Project name.')
@click.option('--apps','-a',help='Aplications names,an be more than one, Example: -a foo -a bar',
              multiple=True)
@click.option('--force','-f',is_flag=True,
              help='Force delete of exists project, error if project exists without --forece flag.')
@click.option('--sendgrid','-s',is_flag=True,
              help='Use sendgrid to send mails for passwd change.')

def main(project, apps, force,sendgrid):
    """Program that create basic Django project.
    """

    # Check project argument
    if not project.islower():
        message_error(f"Project {project} must include a-z letters only.")
        sys.exit()
    django_project = 'django_' + project

#     Check applications names
    app_number = len(apps)
    if app_number < 1:
        message_error("Num of apps must be at least 1")
        sys.exit()

    message_info(f"Project name : {project}")
    message_info(f"Django Project name : {django_project}")

    create_proj_dir(django_project,force)
    os.chdir(django_project)
    create_venv()

    create_django_project(project)
    create_django_app('accounts')
    for app in apps:
        create_django_app(app)

    settings_update(project,apps,sendgrid)
    create_urls(project,apps)
    create_views()
    create_htmls()
    create_models()
    create_forms()
    create_admin()
    create_migrate()
    from CreateUsers import create_users
    create_users()
    create_git(django_project)





if __name__ == '__main__':
    if len(sys.argv) == 1:
        main.main(['--help'])
        sys.exit()
    main()

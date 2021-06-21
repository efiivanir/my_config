#!/usr/bin/python3

import click
import os
import shutil

requirements = {
                "flask": 
                        (
                        'flask',
                        'flask-sqlalchemy'
                        'sqlalchemy',
                        'flask-bootstrap',
                        'flask-moment',
                        'flask-wtf',
                        ),

                    "std":
                        (
                        'python-dotenv',
                        ),

                    "dev":
                        (
                        'setuptools',
                        'wheel',
                        'ipython',
                        'pytest',
                        'pytest-cov',
                        'astroid',
                        'black',
                        'flake8',
                        'lxml',
                        'mypy',
                        'pydocstyle',
                        'pylint',
                        'pytz',
                        'reorder-python-imports',
                        'sphinx',
                        'sphinx-autodoc-typehints',
                        'sphinxcontrib-spelling',
                        'webtest',
                         )
            }

def create_requirements(tp):
    with open('requirements-dev.txt', 'w') as file:
        file.write('-r requirements.txt' + '\n')
        for pkg in requirements['dev']:
                file.write(pkg + '\n')

    with open('requirements.txt', 'w') as file:
        for req in requirements.keys():
            if req == 'dev':
                continue
            for pkg in requirements[req]:
                if req == 'flask' and tp != 'flask':
                    continue
                file.write(pkg + '\n')
    click.echo('Create requirements files')

def create_venv():
    os.system("/bin/bash -c 'python3 -m venv venv'")   
    os.system("/bin/bash -c 'source venv/bin/activate;pip install -r requirements-dev.txt; deactivate'") 

def create_std(name,tp):
    if os.path.exists(name):
        click.echo(f'Project {name} exsists,remove it')
        shutil.rmtree(name)
        
    os.mkdir(name)
    click.echo(f'Create Project directory {name}')
    os.chdir(name)
    create_requirements(tp)
    create_venv()


def create_flaskenv(name):
    flaskenv = '.flaskenv'
    with open(flaskenv, 'w') as file:
        file.write('FLASK_APP = ' + name + '.py\n')
        file.write('FLASK_DEBUG = 1\n')
        file.write('FLASK_ENV = development\n')


def create_flask(name,tp):
    create_std(name,tp)
    create_flaskenv(name)



@click.command()
@click.option('--name', help='Project name',prompt='Project name')
@click.option('--tp','-t', prompt='Project type',help='Project type: std,flask')
def create(name,tp):
    """
    Create python project strocture
    """
    if tp == 'std':
        create_std(name,tp)

    elif tp == 'flask':
        create_flask(name,tp)

if __name__ == '__main__':
    create()

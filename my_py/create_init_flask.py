#!/usr/bin/python3
import sys
import os.path
from os import path

if len(sys.argv) > 1:
   project_name=sys.argv[1]
else:
    print("Usage: " + sys.argv[0] + " " + "project_name")
    exit()

if(path.isdir(project_name)):
    print(project_name + " exists")
    exit()

    # Create directories
app_dir = project_name + "/app"
os.mkdir(project_name)
os.mkdir(app_dir)

# Create __init__.py
_init_file = app_dir + "/__init__.py"
f = open(_init_file, "w")
f.write("from flask import Flask\n")
f.write("app = Flask(__name__)\n")
f.write("from app import routes\n")
f.close()

# Create routes file
routes_file = app_dir + "/routes.py"
f = open(routes_file, "w")


f.write("from flask import render_template\n") 
f.write("from app import app\n\n")
f.write("@app.route('/')\n")
f.write("@app.route('/index')\n")
f.write("def index():\n")
f.write("  user = {'username': 'Ivanir'}\n")
f.write("  return render_template('index.html', title='Home', user=user)")
f.close()

# Create main module file
main_file = project_name + "/" + project_name + ".py"
f = open(main_file, "w")
f.write("from app import app\n")
f.close()

# Create .flaskenv
env_file = project_name + "/.flaskenv"
f = open(env_file, "w")
f.write("FLASK_APP=" + project_name + ".py\n")
f.write("FLASK_ENV=development\n")
f.close()

# Create templates
templates_dir = app_dir + "/templates"
os.mkdir(templates_dir)

index_file = templates_dir + "/index.html"
project_title = project_name.capitalize()
f = open(index_file, "w")
f.write("<html>\n")
f.write("    <head>\n")
f.write("        <title>{{ title }} - " + project_title + "</title>\n")
f.write("    </head>\n")
f.write("    <body>\n")
f.write("        <h1>Hello, {{ user.username }}!</h1>\n")
f.write("    </body>\n")
f.write("</html>\n")
f.close()

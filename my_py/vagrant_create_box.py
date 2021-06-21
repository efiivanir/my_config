#!/usr/bin/python3
# -*- coding: utf-8 -*-
import sys
import os
import re
from subprocess import call
import shutil

# ---------------- Edit here only ------------------------------------------------------
apt_installed_packages = ('vagrant','virtualbox')
box_to_download = 'ubuntu/xenial64'
default_root_dir = 'Sites/test'
provider = 'virtualbox'

vagrant_f = 'Vagrantfile'
provision_f = 'provision.sh'
bashrc_aliases_f = '.bash_aliases'
# --------------------------------------------------------------------------------------
def change_vagrant_file():
    f = open(vagrant_f,"w")
    f.write('Vagrant.configure("2") do |config|' + "\n")
    f.write('  config.vm.box = "' + box_to_download + '"' + "\n")
    f.write('  config.vm.network "forwarded_port", guest: 80, host: 8000' + "\n")
    f.write('  config.vm.provision "shell", path: "' + provision_f + '"' + "\n")
    f.write('  config.vm.provision "file", source: "' + bashrc_aliases_f + '", destination: "$HOME/' + bashrc_aliases_f + '"' + "\n")
    f.write('end' + "\n")
    f.close()
def create_provision_file():
    string = """#!/usr/bin/env bash
echo "Installing Apache and setting it up..."
apt-get update
apt-get install -y apache2 apt
rm -rf /var/www
ln -fs /vagrant /var/www"""
    f = open(provision_f,"w")
    f.write(string + "\n")
    f.close()

    string = """alias lt='ls -lat| more'
alias aguu='sudo apt-get update'
alias agi='sudo apt-get install -y '
alias ags='sudo apt-cache search '
alias agshow='apt show '
alias dp-qu='dpkg-query -l | grep '"""
    f = open(bashrc_aliases_f,"w")
    f.write(string + "\n")
    f.close()  
    
def create_vagrant_dir(dir = default_root_dir):
    if os.path.exists(dir):
        shutil.rmtree(dir)
    os.makedirs(dir, exist_ok=False)
    os.chdir(dir)
    return os.getcwd()

def apt_get_run():
    for i in apt_installed_packages:
        call(['apt-get','install','-y',i])

def vagrant_init_up():
    call(['vagrant','init',box_to_download])
    change_vagrant_file()
    create_provision_file()
    call(['vagrant','up','--provider',provider])
    call(['vagrant','status'])
    
    
def main():
    root_dir = create_vagrant_dir()
    os.chdir(root_dir)
    apt_get_run()
    vagrant_init_up()
    

if __name__== "__main__":
    main()
        

import os
import sys
from messages import message_info,message_error

def create_proj_url(proj,apps):
    url_file = f"{proj}/urls.py"
    message_info(f"Create {url_file}")
    apps_pattern = ''
    for app in apps:
        apps_pattern += f'path("{app}", include("{app}.urls")),\n    '
    data = f'''
from django.contrib import admin
from django.urls import path, include
from accounts.views import HomePageView

urlpatterns = [
    path("admin/", admin.site.urls),
    path("accounts/", include("django.contrib.auth.urls")),
    path("accounts/", include("accounts.urls")),
    {apps_pattern}
    path("", HomePageView.as_view(), name="home"),
]
    '''
    with open(url_file, "w") as fp:
        fp.write(data)


def create_app_url(app):
    url_file = f"{app}/urls.py"
    message_info(f"Create {url_file}")
    data = '''
from django.urls import path

urlpatterns = [
    
]
    '''
    with open(url_file, "w") as fp:
        fp.write(data)


def create_accounts_url():
    url_file = 'accounts/urls.py'
    message_info(f"Create {url_file}")
    data = '''
from django.urls import path
from .views import SignUpView
urlpatterns = [
    path("signup/", SignUpView.as_view(), name="signup"),
]    
    '''
    with open(url_file, "w") as fp:
        fp.write(data)

    
def create_urls(project,apps):
    create_proj_url(project,apps)
    for app in apps:
        create_app_url(app)
    create_accounts_url()
import os
import sys
from messages import message_info, message_error




def create_accounts_view():
    view_file = 'accounts/views.py'
    message_info(f"Create {view_file}")
    data = '''
from django.contrib.auth.forms import UserCreationForm
from django.urls import reverse_lazy
from django.views.generic import CreateView
from django.views.generic import TemplateView


class SignUpView(CreateView):
    form_class = UserCreationForm
    success_url = reverse_lazy("login")
    template_name = "registration/signup.html"
    
class HomePageView(TemplateView):
    template_name = "home.html"
    '''
    with open(view_file, "w") as fp:
        fp.write(data)


def create_views():
    create_accounts_view()

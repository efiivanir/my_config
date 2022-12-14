import os
import sys
from messages import message_info,message_error

def create_base_html():
    html_file = 'templates/base.html'
    message_info(f"Create {html_file}")
    data = '''
{% load static %}
<html>
  <head>
    <title>Django blog</title>
    <link href="https://fonts.googleapis.com/css?family=Source+Sans+Pro:400"
      rel="stylesheet">
    <!-- CSS only -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.2/dist/css/bootstrap.min.css" 
        rel="stylesheet" 
        integrity="sha384-Zenh87qX5JnK2Jl0vWa8Ck2rdkQ2Bzep5IDxbcnCeuOxjzrPF/et3URy9Bv1WTRi" 
        crossorigin="anonymous">

    <link href="{% static 'css/base.css' %}" rel="stylesheet"s>
  </head>
  <body>
    <div class="container">
        <header class="p-3 mb-3 border-bottom">
            <div class="container">
                <div class="d-flex flex-wrap align-items-center justify-content-center
                    justify-content-lg-start">
                    <a class="navbar-brand" href="{% url 'home' %}">Project</a>
                    <ul class="nav col-12 col-lg-auto me-lg-auto mb-2 justify-content-center
                    mb-md-0">
                    {% if user.is_authenticated %}
                        <li><a href="#" class="nav-link px-2 link-dark">+ New</a></li>
                    </ul>
                    <div class="dropdown text-end">
                        <a href="#" class="d-block link-dark text-decoration-none dropdown-toggle"
                            id="dropdownUser1" data-bs-toggle="dropdown" aria-expanded="false">
                            {{ user.username }}
                        </a>
                        <ul class="dropdown-menu text-small" aria-labelledby="dropdownUser1">
                            <li><a class="dropdown-item" href="{% url 'password_change'%}">
                                Change password</a></li>
                            <li><a class="dropdown-item" href="{% url 'logout' %}">Log Out</a></li>
                        </ul>
                    </div>
                    {% else %}
                    </ul>
                    <div class="text-end">
                        <a href="{% url 'login' %}" class="btn btn-outline-primary me-2">
                            Log In</a>
                        <a href="{% url 'signup' %}" class="btn btn-primary">Sign Up</a>
                    </div>
                    {% endif %}
                </div>
            </div>
        </header>
        
        <main>
            {% block content %}
            {% endblock content %}
        </main>
    </div>
    <!-- JavaScript Bundle with Popper -->
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.2/dist/js/bootstrap.bundle.min.js" 
    integrity="sha384-OERcA2EqjJCMA+/3y+gxIOqMEjwtxJY7qPCqsdltbNJuaOe923+mo//f6V8Qbsw3" 
    crossorigin="anonymous">
    </script>

  </body>
</html>
    '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_login_html():
    html_file = 'templates/registration/login.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% load crispy_forms_tags %}

{% block content %}
    <h2>Log In</h2>
    <form method="post">{% csrf_token %}
        {{ form|crispy }}
        <button class="btn btn-success ml-2" type="submit">Log In</button>
        </form>
    <p><a href="{% url 'password_reset' %}">Forgot your password?</a></p>
{% endblock content %}
    '''
    with open(html_file, "w") as fp:
        fp.write(data)


def create_signup_html():
    html_file = 'templates/registration/signup.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% load crispy_forms_tags %}

{% block title %}Sign Up{% endblock title %}

{% block content %}
    <h2>Sign Up</h2>
    <form method="post">{% csrf_token %}
        {{ form|crispy }}
        <button type="submit">Sign Up</button>
    </form>
{% endblock content %}
    '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_change_form_html():
    html_file = 'templates/registration/password_change_form.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% load crispy_forms_tags %}

{% block title %}Password Change{% endblock title %}

{% block content %}
    <h1>Password change</h1>
    <p>Please enter your old password, for security's sake, and then enter
    your new password twice so we can verify you typed it in correctly.</p>
    <form method="POST">{% csrf_token %}
        {{ form|crispy }}
        <input class="btn btn-success" type="submit"
        value="Change my password">
    </form>
{% endblock content %}
    '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_change_done_html():
    html_file = 'templates/registration/password_change_done.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}

{% block title %}Password Change Successful{% endblock title %}
{% block content %}
    <h1>Password change successful</h1>
    <p>Your password was changed.</p>
{% endblock content %}
    '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_reset_form_html():
    html_file = 'templates/registration/password_reset_form.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% load crispy_forms_tags %}


{% block title %}Forgot Your Password?{% endblock title %}
{% block content %}
    <h1>Forgot your password?</h1>
    <p>Enter your email address below, and we'll email instructions
    for setting a new one.</p>
    <form method="POST">{% csrf_token %}
        {{ form|crispy }}
        <input class="btn btn-success" type="submit"
        value="Send me instructions!">
    </form>
{% endblock content %}
        '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_reset_done_html():
    html_file = 'templates/registration/password_reset_done.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}


{% block title %}Email Sent{% endblock title %}
{% block content %}
    <h1>Check your inbox.</h1>
    <p>We've emailed you instructions for setting your password.
    You should receive the email shortly!</p>
{% endblock content %}
        '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_reset_confirm_html():
    html_file = 'templates/registration/password_reset_confirm.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% load crispy_forms_tags %}


{% block title %}Enter new password{% endblock title %}
{% block content %}
    <h1>Set a new password!</h1>
    <form method="POST">{% csrf_token %}
        {{ form|crispy }}
        <input class="btn btn-success" type="submit" value="Change my password">
    </form>
{% endblock content %}
        '''
    with open(html_file, "w") as fp:
        fp.write(data)

def create_password_reset_complete_html():
    html_file = 'templates/registration/password_reset_complete.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}

{% block title %}Password reset complete{% endblock title %}

{% block content %}
    <h1>Password reset complete</h1>
    <p>Your new password has been set.</p>
    <p>You can log in now on the
    <a href="{% url 'login' %}">Log In page</a>.</p>
{% endblock content %}
        '''
    with open(html_file, "w") as fp:
        fp.write(data)




def create_homepage_html():
    html_file = 'templates/home.html'
    message_info(f"Create {html_file}")
    data = '''
{% extends "base.html" %}
{% block title %}Home{% endblock title %}


{% block content %}
{% if user.is_authenticated %}
    Hi {{ user.username }}!
    <p><a href="{% url 'logout' %}">Log Out</a></p>
{% else %}
    <p>You are not logged in</p>
    <a href="{% url 'login' %}">Log In</a> |
    <a href="{% url 'signup' %}">Sign Up</a>
{% endif %}
{% endblock content %}
    '''
    with open(html_file, "w") as fp:
        fp.write(data)


def create_htmls():
    create_base_html()
    create_login_html()
    create_signup_html()
    create_homepage_html()

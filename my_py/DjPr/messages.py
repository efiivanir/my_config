import click
def message_info(message):
    click.secho(message, fg='green')

def message_error(message):
    click.secho(message, fg='red')

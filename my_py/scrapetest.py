from urllib.request import urlopen
from bs4 import BeautifulSoup
url = '/root/names.html'
html = open(url)
text = html.read()
bsObj = BeautifulSoup(text)
print(bsObj.p)


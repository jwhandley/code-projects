from bs4 import BeautifulSoup
from bs4 import SoupStrainer
import requests

cities = ['oxford']
URL = 'https://www.google.com/search?q={}+annual+weather'

s = requests.Session()

res = s.get(URL.format('oxford'))
out = BeautifulSoup(res.content,'html.parser')

out.find_all('g-expandable-content')

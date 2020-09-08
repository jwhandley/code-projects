import shutil
import requests
import datetime

today = datetime.datetime.now()

if today.weekday() == 5:
    date = today - datetime.timedelta(days=1)
elif today.weekday() == 6:
    date = today - datetime.timedelta(days=2)
else:
    date = today



api_key = 'lqs0feHwO9maxQMs8YjQyABmbQ1e8aVFOhAzeq4C'
date_str = date.strftime('%Y-%m-%d')

r = requests.get('https://api.nasa.gov/planetary/apod?api_key={}&date={}'.format(api_key,date_str))
response = requests.get(r.json()['url'])

with open("img.jpg", 'wb') as f:
    f.write(response.content)
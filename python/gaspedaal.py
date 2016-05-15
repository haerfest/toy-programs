from bs4 import BeautifulSoup
import requests

base_url = 'http://www.gaspedaal.nl/Mazda/MX-5/'
query = '?pmax=20000&kmax=80000&bmin=2005&trns=Handgeschakeld&srt=df'

r = requests.get(base_url + query)
if not r.status_code == 200:
    raise Exception('Unexpected HTTP status code {}'.format(r.status_code))

soup = BeautifulSoup(r.text, 'html.parser')
cars = soup.find_all('div', 'item-desktop')
if cars == 0:
    raise Exception('Did not find any cars')

print("Found {} cars".format(len(cars)))

for car in cars:
    try:
        price = car.find('span', 'prijs').b.text
        year = car.find('div', 'bj-col').contents[1]
        mileage = car.find('div', 'km-col').contents[1]
        title = car.find('div', 'titel-col').a.text
        link = car.find('div', 'titel-col').a['href']
        description = car.find('div', 'oms-col').text

        items = [year, mileage, price, title, description, link]
        print("\t".join(items))
    except:
        pass

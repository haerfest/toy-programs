from bs4 import BeautifulSoup
from urllib.parse import quote_plus as q
import requests


class Scraper:

    def scrape(self, brand, model, **kwargs):
        """
        Scrapes gaspedaal.nl for all ads for a particular car brand and model.
        The search can be narrowed down by **kwargs. Cars found are returned as
        a list of dictionaries.
        """
        url = 'http://www.gaspedaal.nl/{0}/{1}/'.format(q(brand), q(model))

        if len(kwargs) > 0:
            pairs = ['{0}={1}'.format(q(k), q(str(v)))
                     for k, v in kwargs.items()]
            url += '?' + '&'.join(pairs)

        r = requests.get(url)
        if not r.status_code == 200:
            raise Exception('HTTP status code {}'.format(r.status_code))

        soup = BeautifulSoup(r.text, 'html.parser')
        cars = soup.find_all('div', 'item-desktop')

        return filter(lambda car: car is not None, map(self.extract, cars))

    def extract(self, soup):
        """
        Extract a single car's information from HTML soup. Returns None if no
        information could be extracted.
        """
        try:
            price = soup.find('span', 'prijs').b.text
            year = soup.find('div', 'bj-col').contents[1]
            mileage = soup.find('div', 'km-col').contents[1]
            title = soup.find('div', 'titel-col').a.text
            link = soup.find('div', 'titel-col').a['href']
            description = soup.find('div', 'oms-col').text

            return {'year': year, 'mileage': mileage, 'price': price,
                    'title': title, 'description': description, 'link': link}
        except:
            return None


if __name__ == '__main__':
    scraper = Scraper()
    for car in scraper.scrape('Mazda', 'MX-5', pmax=20000, kmax=80000,
                              bmin=2005, trns='Handgeschakeld'):
        print(car)

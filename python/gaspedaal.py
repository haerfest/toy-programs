from bs4 import BeautifulSoup
from urllib.parse import quote_plus as q
import requests


class Scraper:

    def __init__(self, brand, model, **kwargs):
        """
        Scrapes a popular Dutch car website for all ads for a particular brand
        and model. The search can be narrowed down by **kwargs.
        """
        self.brand = brand
        self.model = model
        self.parameters = kwargs
        self.soup = None
        self.page = 0

    @property
    def url(self):
        """
        The URL representing the search request.
        """
        url = 'http://www.gaspedaal.nl/{0}/{1}/?srt=df&p={2}'.format(
            q(self.brand), q(self.model), self.page)

        if len(self.parameters) > 0:
            pairs = ['{0}={1}'.format(q(k), q(str(v)))
                     for k, v in self.parameters.items()]
            url += '&' + '&'.join(pairs)

        return url

    @property
    def has_next_page(self):
        """
        Returns whether there is another page with search results.
        """
        if self.soup is None:
            return True          # makes iteration easy

        return self.soup.find('li', 'volgende') is not None

    def next_page(self):
        """
        Scrapes a page and returns the vehicles found as a list of dicts.
        """
        self.page += 1
        r = requests.get(self.url)
        if not r.status_code == 200:
            raise Exception('HTTP status code {}'.format(r.status_code))

        self.soup = BeautifulSoup(r.text, 'html.parser')
        cars = self.soup.find_all('div', 'item-desktop')

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
    scraper = Scraper('Mazda', 'MX-5', pmax=20000, kmax=80000,
                      bmin=2005, trns='Handgeschakeld')

    while scraper.has_next_page:
        for car in scraper.next_page():
            print(car)

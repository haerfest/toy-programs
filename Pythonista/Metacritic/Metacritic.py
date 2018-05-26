#!/usr/bin/env python3

from bs4 import BeautifulSoup
import urllib
import requests
import ui
import console
import string
import webbrowser

BASE_URL = 'http://www.metacritic.com'

HEADERS = {
	'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36'
}

PAGE_LIMIT = 10

def normalize(s):
	return ''.join([ch for ch in s if str.isalnum(ch)]) if s else ''

class TableViewHelper(object):
	def __init__(self):
		self.scores = []

	@property
	def scores(self):
		return self._scores

	@scores.setter
	def scores(self, value):
		self._scores = sorted(value, key=self.sort_order)
		self._titles = sorted(list(set([x['title'] for x in self._scores])))

	def sort_order(self, x):
		try:
			sort_score = 100 - int(x['score'])
		except ValueError:
			sort_score = 100

		return (
			normalize(x['title']),
			sort_score,
			x['platform']
		)

	def tableview_number_of_sections(self, tableview):
		return len(self._titles)

	def tableview_title_for_header(self, tableview, section):
		return self._titles[section]

	def tableview_number_of_rows(self, tableview, section):
		title = self._titles[section]
		rows = [x for x in self._scores if x['title'] == title]
		return len(rows)

	def tableview_cell_for_row(self, tableview, section, row):
		title = self._titles[section]
		rows = [x for x in self._scores if x['title'] == title]
		x = rows[row]

		cell = ui.TableViewCell()
		cell.accessory_type = 'disclosure_indicator'
		cell.text_label.text = '{}, {}'.format(
			x['score'],
			x['platform']
		)

		return cell

	@ui.in_background
	def tableview_did_select(self, tableview, section, row):
		title = self._titles[section]
		rows = [x for x in self._scores if x['title'] == title]
		link = rows[row]['link']
		webbrowser.open(link, modal=True)

def parse_result(result):
	title_tag    = result.find(class_='product_title')
	score_tag    = result.find(class_='metascore_w')
	platform_tag = result.find(class_='platform')

	link = title_tag.find('a')
	return {
		'title': link.string.strip(),
		'score': score_tag.string,
		'platform': platform_tag.string,
		'link': BASE_URL + link.get('href'),
	}

def parse_html(html):
	soup = BeautifulSoup(html, 'html5lib')
	results = soup.find_all(class_='result')
	return [parse_result(r) for r in results]

def get_scores(url, page=1):
	page_url = url
	if page > 1:
		page_url += '?page={}/'.format(page)

	r = requests.get(
		page_url,
		headers=HEADERS
	)
	if r.status_code != 200:
		raise Exception('Got HTTP status code {} trying to fetch {}'.format(r.status_code, url_url))

	return parse_html(r.text)

def filter_scored(scores):
	return [x for x in scores if x['score'] != 'tbd']

@ui.in_background
def searchAction(sender):
	view['searchTextField'].enabled = False
	view['searchButton'].enabled    = False
	view['spinner'].start()

	term = view['searchTextField'].text
	quoted = urllib.parse.quote_plus(term)
	url = '{}/search/game/{}/results/'.format(
		BASE_URL,
		quoted
	)

	try:
		scores = []
		for page in range(1, 1 + PAGE_LIMIT):
			s = get_scores(url, page)
			if not s:
				break
			scores.extend(filter_scored(s))

			# Live update.
			view['scoresTableView'].delegate.scores = scores
			view['scoresTableView'].reload_data()

	except Exception as e:
		console.alert('Oops', str(e), 'OK', hide_cancel_button=True)
		return

	finally:
		view['spinner'].stop()
		view['searchButton'].enabled    = True
		view['searchTextField'].enabled = True

helper = TableViewHelper()
view = ui.load_view('Metacritic')
view['scoresTableView'].delegate    = helper
view['scoresTableView'].data_source = helper
view['searchButton'].action    = searchAction
view['searchTextField'].action = searchAction

spinner = ui.ActivityIndicator(
	name='spinner',
	flex='LRTB'
)
spinner.center = (view.width / 2, view.height / 2)
view.add_subview(spinner)

view.present()

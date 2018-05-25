#!/usr/bin/env python3

from bs4 import BeautifulSoup
import urllib
import requests
import ui
import console
import string
import webbrowser

BASE_URL = 'http://www.metacritic.com'

class TableViewHelper(object):
	def __init__(self, scores):
		self.scores = sorted(scores, key=self.sort_order)
	
	def sort_order(self, x):
		try:
			sort_score = 100 - int(x['score'])
		except ValueError:
			sort_score = 100
	
		return (
				self.normalize(x['title']),
				sort_score,
				x['platform']
		)

	def normalize(self, s):
		return ''.join([ch for ch in s if str.isalnum(ch)]) if s else ''

	def tableview_number_of_rows(self, tableview, section):
		return len(self.scores)
		
	def tableview_cell_for_row(self, tableview, section, row):
		cell = ui.TableViewCell()
		x = self.scores[row]
		
		cell.text_label.text = '{}: {} ({})'.format(
			x['score'],
			x['title'], 
			x['platform']
		)

		return cell
		
	@ui.in_background
	def tableview_did_select(self, tableview, section, row):
		link = self.scores[row]['link']
		webbrowser.open(link, modal=True)

def parse_search_result(result):
	title_tag    = result.find(class_='product_title')
	score_tag    = result.find(class_='metascore_w')
	platform_tag = result.find(class_='platform')
		
	if title_tag and score_tag and platform_tag:
		link = title_tag.find('a')	
		return {
			'title': link.string.strip(),
			'score': score_tag.string,
			'platform': platform_tag.string,
			'link': BASE_URL + link.get('href'),
		}
		
	return None
			
@ui.in_background
def searchAction(sender):
	view['scoresTableView'].data_source.items = []
	view['searchTextField'].enabled = False
	view['searchButton'].enabled = False
	view['spinner'].start()
	
	term = view['searchTextField'].text
	quoted = urllib.parse.quote_plus(term)
	url = '{}/search/game/{}/results'.format(
		BASE_URL,
		quoted
	)
	headers = {
		'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36'
	}

	r = requests.get(url, headers=headers)
	
	view['spinner'].stop()
	view['searchButton'].enabled = True
	view['searchTextField'].enabled = True
	
	if r.status_code != 200:
		console.alert('Oops', 'Got HTTP status code {} trying to fetch {}'.format(r.status_code, url), 'OK', hide_cancel_button=True)
		return
	
	soup = BeautifulSoup(r.text, 'html5lib')
	scores = [parse_search_result(r) for r in soup.find_all(class_='result')]
	scores = list(filter(None.__ne__, scores))

	helper = TableViewHelper(scores)
	view['scoresTableView'].delegate = helper
	view['scoresTableView'].data_source = helper
	view['scoresTableView'].reload_data()
	
view = ui.load_view('Metacritic')
view['searchButton'].action = searchAction
view['searchTextField'].action = searchAction

spinner = ui.ActivityIndicator(
	name='spinner',
	flex='LRTB'
)
spinner.center = (view.width / 2, view.height / 2)
view.add_subview(spinner)

view.present()

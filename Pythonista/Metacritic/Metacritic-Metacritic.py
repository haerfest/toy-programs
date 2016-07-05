#!/usr/bin/env python3

from bs4 import BeautifulSoup
import urllib
import requests
import ui
import console
import string

@ui.in_background
def searchAction(sender):
	view['scoresTableView'].data_source.items = []
	view['searchTextField'].enabled = False
	view['searchButton'].enabled = False
	view['spinner'].start()

	#
	# Prepare the search URL and headers.
	#
	term = view['searchTextField'].text
	quoted = urllib.parse.quote_plus(term)
	url = 'http://www.metacritic.com/search/game/{}/results'.format(quoted)
	headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36'}

	#
	# Fetch the results.
	#
	r = requests.get(url, headers=headers)
	
	view['spinner'].stop()
	view['searchButton'].enabled = True
	view['searchTextField'].enabled = True
	
	if r.status_code != 200:
		console.alert('Oops', 'Got HTTP status code {} trying to fetch {}'.format(r.status_code, url), 'OK', hide_cancel_button=True)
		return
	
	#
	# Extract each result's title, score, platform.
	#
	soup = BeautifulSoup(r.text, 'html5lib')
	scores = []
	for result in soup.find_all(class_='result'):
		score_tag = result.find(class_='metascore_w')
		if not score_tag is None:
			title = result.find(class_='product_title').string
			platform = result.find(class_='platform').string
			scores.append({
				'title': title,
				'score': int(score_tag.string),
				'platform': platform
			})

	#
	# Sort by normalized title, then descending by
	# score, then by platform.
	#
	scores.sort(key=lambda x: (''.join([ch for ch in x['title'] if str.isalnum(ch)]), 100 - x['score'], x['platform']))

	view['scoresTableView'].data_source.items = ['{}: {} ({})'.format(x['score'], x['title'], x['platform']) for x in scores]

view = ui.load_view('Metacritic')
view['searchButton'].action = searchAction

spinner = ui.ActivityIndicator(name='spinner', flex = 'LRTB')
spinner.center = (view.width / 2, view.height / 2)
view.add_subview(spinner)

view.present()


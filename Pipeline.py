while True:

	import csv
	import urllib.request
	import pandas as pd
	import time

	years = ('21', '20', '19', '18', '17', '16', '15', '14', '13', '12', '11', '10', '09', '08', '07', '06', '05', '04', '03', '02', '01', '00')

	preurl = 'https://www.football-data.co.uk/mmz4281/'

	def read_csv(url, filename):
		path = '/Users/christian/Kaggle/Sports Analytics/Football/Game Data/' + filename + '.csv'
		data = pd.read_csv(urllib.request.urlopen(url), error_bad_lines = False)
		data = data[['Date', 'HomeTeam', 'AwayTeam', 'FTHG', 'FTAG', 'FTR', 'HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC', 'AC', 'HF', 'AF', 'HY', 'AY', 'HR', 'AR']]
		data.to_csv(path, header = True, index = False)

	for previous, current in zip(years, years[1:]):
		filename = current + previous
		url = preurl + filename + '/E0.csv'
		print('Importing game data from the %s - %s season' % (previous, current))
		read_csv(url, filename)
		print('Done!')

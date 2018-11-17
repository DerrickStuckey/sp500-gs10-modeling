import pandas as pd
import csv
import numpy as np

# Schiller S&P price data
# 'Data' tab from http://www.econ.yale.edu/~shiller/data/ie_data.xls - saved as tab-delimited text
fname = "./schiller_sp_data.txt"
rows = []
with open(fname,'rb') as f:
	thereader = csv.reader(f,delimiter="\t")
	start=False
	headerstart = "Date"
	for row in thereader:
		# start saving rows after 'headerstart' row
		if(start):
			# quit when we hit a row with no Date value
			if(row[0]==""):
				break
			# just keep first 11 columns
			rows += [row[0:11]]
		# determine when to start parsing
		try:
			if(row[0].startswith(headerstart)):
				start=True
		except:
			continue

# convert to a Dataframe
data = pd.DataFrame(rows, columns=['Date.Raw','SP.Price','Dividend','Earnings','CPI','Date.Fraction','GS10','Real.Price','Real.Dividend','Real.Earnings','CAPE'])

# format dates nicely
data['Date'] = ["%s-%s-01" % tuple(x.split('.')) for x in data['Date.Raw']]

# prices for next and previous month
data['SP.Price.Last'] = data['SP.Price'].shift(1)
data['SP.Price.Next'] = data['SP.Price'].shift(-1)

# save the data to a .tsv
data.to_csv("./schiller_sp_data_formatted.csv",index=False)


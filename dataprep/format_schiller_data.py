import pandas as pd
import csv
import numpy as np

# Schiller S&P price data
# 'Data' tab from http://www.econ.yale.edu/~shiller/data/ie_data.xls - saved as tab-delimited text
fname = "../raw_data/schiller_sp_data.txt"
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
def fix_date(rawdate):
	year, month = rawdate.split(".")
	if(month=="1"):
		month="10"
	return("%s-%s-01" % (year,month))

data['Date'] = [fix_date(x) for x in data['Date.Raw']]

# prices for next and previous month
data['SP.Price.Last'] = data['SP.Price'].shift(1)
data['SP.Price.Next'] = data['SP.Price'].shift(-1)
data['SP.Price.Prev.6Mo'] = data['SP.Price'].shift(6)
data['SP.Price.Next.6Mo'] = data['SP.Price'].shift(-6)
data['SP.Price.Prev.12Mo'] = data['SP.Price'].shift(12)
data['SP.Price.Next.12Mo'] = data['SP.Price'].shift(-12)

# save the data to a .tsv
data.to_csv("../prepared_data/schiller_sp_data_formatted.tsv",index=False,sep="\t")

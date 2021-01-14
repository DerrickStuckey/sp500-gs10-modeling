import pandas as pd
import csv
import numpy as np

# AAII Sentimnt Data
# 'sentiment' tab from https://www.aaii.com/files/surveys/sentiment.xls - saved as tab-delimited text
fname = "../raw_data/sentiment.txt"
rows = []
with open(fname,'rb') as f:
	thereader = csv.reader(f,delimiter="\t")
	start=False
	headerstart = "6-26-87"
	for row in thereader:
		# start saving rows after 'headerstart' row
		if(start):
			# quit when we hit a row with no Date value
			if(row[0]==""):
				break
			# just keep first 5 columns
			rows += [row[0:5]]
		# determine when to start parsing
		try:
			if(row[0].startswith(headerstart)):
				start=True
				rows += [row[0:5]]
		except:
			continue

def parsePercentage(text):
	if text.endswith('%'):
		return(float(text.strip("%"))/100)
	try:
		return(float(text))
	finally:
		return(np.NaN)

# convert to a Dataframe
data = pd.DataFrame(rows, columns=['ReportedDate','Bullish','Neutral','Bearish','Total'])

data['Bullish'] = [parsePercentage(x) for x in data['Bullish']]
data['Neutral'] = [parsePercentage(x) for x in data['Neutral']]
data['Bearish'] = [parsePercentage(x) for x in data['Bearish']]
data['Total'] = [parsePercentage(x) for x in data['Total']]

data = data.dropna()
# import pdb; pdb.set_trace()

# save the data to a .tsv
outfile="../prepared_data/sentiment_data_formatted.tsv"
data.to_csv(outfile,index=False,sep="\t")
print("formatted data written out to: %s" % outfile)

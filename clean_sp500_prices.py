import pandas as pd
import csv
import numpy as np

# S&P 500 daily price data
spdata = pd.read_csv("sp500_daily_prices.csv")

spdata['Adj Close'] = pd.to_numeric(spdata['Adj Close'],errors=coerce)

spdata['Date'] = pd.to_datetime(spdata['Date'])

for i in spdata.index:
	try:
		curdate = spdata.loc[i,'Date']
		lookahead_180_date = curdate + pd.Timedelta('180 days')
		spdata.loc[i,'Adj Close 180 Ahead'] = spdata.loc[spdata['Date']>=lookahead_180_date,'Adj Close'].values[0]
		lookahead_90_date = curdate + pd.Timedelta('90 days')
		spdata.loc[i,'Adj Close 90 Ahead'] = spdata.loc[spdata['Date']>=lookahead_90_date,'Adj Close'].values[0]
		lookahead_30_date = curdate + pd.Timedelta('30 days')
		spdata.loc[i,'Adj Close 30 Ahead'] = spdata.loc[spdata['Date']>=lookahead_30_date,'Adj Close'].values[0]
	except:
		continue

spdata['Next Adj Close'] = spdata['Adj Close'].shift(-1)
spdata['Prev Adj Close'] = spdata['Adj Close'].shift(1)

# import pdb; pdb.set_trace()

spdata.to_csv("sp500_daily_cleaned.csv",index=False)

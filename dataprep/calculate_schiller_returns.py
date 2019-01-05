import pandas as pd
import csv
import numpy as np
import finance_utils

# read formatted schiller data (from format_schiller_data.py)
data = pd.read_csv("../prepared_data/schiller_sp_data_formatted.tsv",sep="\t")

# import pdb; pdb.set_trace()

# estimate S&P total return for current month
# assume dividends = 1/12 of dividend yield, accrued but not reinvested until end of month
# ignore tax and transaction costs
data['SP.Change.Forward'] = np.divide(data['SP.Price.Next'],data['SP.Price'])
data['SP.Return.Forward'] = np.divide(np.add(data['SP.Price.Next'], data['Dividend'] / 12),data['SP.Price'])

# estimate GS10 total return for current month
# assume exact 10 year duration, monthly coupons distributed at beginning of month, reinvested at end of month
# assume discount rate at end of month is equal to GS10 rate at 1st of the next month
# ignore tax and transaction costs
nrows = data.shape[0]
principal=1
duration=12*10-1 # duration in months
for i in range(0,nrows,1):
	try:
		coupon_rate=data.loc[i,'GS10']/100/12
		discount_rate = data.loc[i+1,'GS10']/100/12
		data.loc[i,'GS10.NPV.Forward'] = finance_utils.bond_npv(principal=principal,duration=duration,coupon_rate=coupon_rate,discount_rate=discount_rate)
		data.loc[i,'GS10.Return.Forward'] = data.loc[i,'GS10.NPV.Forward'] + coupon_rate
	except:
		pass

# import pdb; pdb.set_trace()

data.to_csv("../prepared_data/schiller_sp_data_withreturns.tsv",index=False,sep="\t")

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

# log returns
data['Log.SP.Return.Forward'] = np.log(data['SP.Return.Forward'])
data['Log.GS10.Return.Forward'] = np.log(data['GS10.Return.Forward'])

# cumulative log returns
sp_cumulative_log_return = 0
gs10_cumulative_log_return = 0
for i in range(0,data.shape[0],1):
	sp_cumulative_log_return = sp_cumulative_log_return + data.loc[i,'Log.SP.Return.Forward']
	gs10_cumulative_log_return = gs10_cumulative_log_return + data.loc[i,'Log.GS10.Return.Forward']
	data.loc[i,'Log.SP.Return.Forward.Cumulative'] = sp_cumulative_log_return
	data.loc[i,'Log.GS10.Return.Forward.Cumulative'] = gs10_cumulative_log_return

# calculate shifted values for 3-36 months forward
data['Log.SP.Return.Forward.Cumulative.36Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-36)
data['Log.SP.Return.Forward.Cumulative.18Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-18)
data['Log.SP.Return.Forward.Cumulative.12Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-12)
data['Log.SP.Return.Forward.Cumulative.6Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-6)
data['Log.SP.Return.Forward.Cumulative.3Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-3)

data['Log.GS10.Return.Forward.Cumulative.36Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-36)
data['Log.GS10.Return.Forward.Cumulative.18Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-18)
data['Log.GS10.Return.Forward.Cumulative.12Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-12)
data['Log.GS10.Return.Forward.Cumulative.6Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-6)
data['Log.GS10.Return.Forward.Cumulative.3Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-3)

# import pdb; pdb.set_trace()

# save the data
outfile="../prepared_data/schiller_sp_data_withreturns.tsv"
data.to_csv(outfile,index=False,sep="\t")
print("returns data written out to: %s" % outfile)

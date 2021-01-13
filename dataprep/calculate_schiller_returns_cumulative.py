import pandas as pd
import csv
import numpy as np
import finance_utils



data = pd.read_csv("../prepared_data/schiller_sp_data_withreturns.tsv",sep="\t")

data['Log.SP.Return.Forward'] = np.log(data['SP.Return.Forward'])
data['Log.GS10.Return.Forward'] = np.log(data['GS10.Return.Forward'])

sp_cumulative_log_return = 0
gs10_cumulative_log_return = 0
for i in range(0,data.shape[0],1):
	sp_cumulative_log_return = sp_cumulative_log_return + data.loc[i,'Log.SP.Return.Forward']
	gs10_cumulative_log_return = gs10_cumulative_log_return + data.loc[i,'Log.GS10.Return.Forward']
	data.loc[i,'Log.SP.Return.Forward.Cumulative'] = sp_cumulative_log_return
	data.loc[i,'Log.GS10.Return.Forward.Cumulative'] = gs10_cumulative_log_return

# calculate shifted values
data['Log.SP.Return.Forward.Cumulative.12Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-12)
data['Log.SP.Return.Forward.Cumulative.6Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-6)
data['Log.SP.Return.Forward.Cumulative.3Months.Ahead'] = data['Log.SP.Return.Forward.Cumulative'].shift(-3)

data['Log.GS10.Return.Forward.Cumulative.12Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-12)
data['Log.GS10.Return.Forward.Cumulative.6Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-6)
data['Log.GS10.Return.Forward.Cumulative.3Months.Ahead'] = data['Log.GS10.Return.Forward.Cumulative'].shift(-3)


# import pdb; pdb.set_trace()

outfile="../prepared_data/schiller_sp_data_withreturns_cumulative.tsv"
data.to_csv(outfile,sep="\t")
print("cumulative returns data written out to: %s" % outfile)
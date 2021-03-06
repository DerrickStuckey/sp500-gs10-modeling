import pandas as pd
import csv
import numpy as np

# from 
ycdata = pd.read_csv("../raw_data/T10Y2Y.csv")

# coerce series to numeric value, replacing '.' with NaN
ycdata['T10Y2Y'] = pd.to_numeric(ycdata['T10Y2Y'],errors=coerce)

# impute missing data points as average of previous and next
ycdata['T10Y2Y.prev'] = ycdata['T10Y2Y'].shift(1)
ycdata['T10Y2Y.next'] = ycdata['T10Y2Y'].shift(-1)

# start with the actual value
ycdata['T10Y2Y.imputed'] = ycdata['T10Y2Y']

missing_idx = np.isnan(ycdata['T10Y2Y'])

# overwrite the value as the average of the previous and next datapoints, if it is missing
for i in missing_idx[missing_idx].index:
	ycdata.loc[i,'T10Y2Y.imputed'] = np.nanmean([ycdata.loc[i,'T10Y2Y.prev'],ycdata.loc[i,'T10Y2Y.next']])

# find "inversion" points where 10-year 2-year spread turns negative
ycdata['Inversion'] = np.logical_and(ycdata['T10Y2Y.imputed']<0, ycdata['T10Y2Y.imputed'].shift(1)>=0)

ycdata.to_csv("../prepared_data/T10Y2Y_imputed.csv",index=False)



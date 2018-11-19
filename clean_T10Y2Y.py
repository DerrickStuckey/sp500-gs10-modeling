import pandas as pd
import csv
import numpy as np

# from https://fred.stlouisfed.org/series/T10Y2Y
ycdata = pd.read_csv("T10Y2Y.csv")

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

ycdata.to_csv("T10Y2Y_imputed.csv",index=False)



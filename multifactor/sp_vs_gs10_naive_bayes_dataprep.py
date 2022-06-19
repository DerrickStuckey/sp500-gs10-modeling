import pandas as pd
import numpy as np

## S&P 500 and 10-year treasury returns data monthly since 1871, from Schiller dataset
schiller_data_prepared_fname = "../prepared_data/schiller_sp_data_withreturns.tsv"
schiller_data = pd.read_csv(schiller_data_prepared_fname, sep="\t")
schiller_data.dtypes

# Calculate Momentum Factors

# use CAPE for earnings yield for smoothing


## Yield Curve Status
yield_curve_fname = "../prepared_data/yield_curve_10y_3mo.tsv"
yield_curve_data = pd.read_csv(yield_curve_fname, sep="\t")

## AAII Sentiment Data
aaii_filename = "../prepared_data/sentiment_data_formatted.tsv"
aaii_data = pd.read_csv(aaii_filename, sep="\t")

# Associate the most recent sentiment data report with each month
aaii_data['Ceiling.Month'] = aaii_data['ReportedDate'] + pd.offsets.MonthBegin(0)
# TODO keep only the latest entry for each Ceiling.Month


## T-bill rate for risk premium calculation
TB3MS = pd.read_csv("../raw_data/TB3MS.csv")

# format dates???
# schiller_data.dtypes
# yield_curve_data.dtypes
schiller_data = schiller_data.astype({"Date": np.datetime64})
yield_curve_data = yield_curve_data.astype({"DATE": np.datetime64})
aaii_data = aaii_data.astype({"ReportedDate": np.datetime64})
TB3MS = TB3MS.astype({"DATE": np.datetime64})

## join all the tables

# join schiller data and yield curve
joined_data = pd.merge(schiller_data, yield_curve_data, how="left", left_on="Date", right_on="DATE")

# verify the join
schiller_data['Date'].tail()
yield_curve_data['DATE'].head()
yield_curve_data['DATE'].tail()
joined_data[['Date','Yield.Curve.Status']].head()
joined_data[['Date','Yield.Curve.Status']].tail()
yield_curve_data['Yield.Curve.Status'].value_counts()
joined_data['Yield.Curve.Status'].value_counts()

import pdb; pdb.set_trace()

# add t-bill rate data
joined_data = pd.merge(joined_data, TB3MS, how="left", left_on="Date", right_on="DATE")

# add aaii data
aaii_data.head()
joined_data = pd.merge(joined_data, aaii_data, how="left", left_on="Date", right_on="Date")
joined_data[['Date','Bullish']].head()
joined_data[['Date','Bullish']].tail()
aaii_data['Bullish'].describe()
joined_data['Bullish'].describe()

# Construct the target variable
# For a Naive Bayes model we need a categorical target, so just use a binary variable
# indicating whether the S&P 500 or 10-year treasury performs better 
# on a one-month forward basis


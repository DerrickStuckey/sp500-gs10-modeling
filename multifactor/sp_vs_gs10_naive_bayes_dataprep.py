import pandas as pd
import numpy as np

## S&P 500 and 10-year treasury returns data monthly since 1871, from Schiller dataset
schiller_data_prepared_fname = "../prepared_data/schiller_sp_data_withreturns.tsv"
schiller_data = pd.read_csv(schiller_data_prepared_fname, sep="\t")
schiller_data.dtypes

# Calculate Momentum Factors
# use np.sign to preserve NaN when one value is unavailable
schiller_data['SP.Momentum.1Mo.Negative'] = np.sign(schiller_data['SP.Price.Last'] - schiller_data['SP.Price'])
schiller_data['SP.Momentum.6Mo.Negative'] = np.sign(schiller_data['SP.Price.Prev.6Mo'] - schiller_data['SP.Price'])
schiller_data['SP.Momentum.12Mo.Negative'] = np.sign(schiller_data['SP.Price.Prev.12Mo'] - schiller_data['SP.Price'])
# relabel sign=-1 to sign=0
schiller_data['SP.Momentum.1Mo.Negative'].mask(schiller_data['SP.Momentum.1Mo.Negative']<0,0,inplace=True)
schiller_data['SP.Momentum.6Mo.Negative'].mask(schiller_data['SP.Momentum.6Mo.Negative']<0,0,inplace=True)
schiller_data['SP.Momentum.12Mo.Negative'].mask(schiller_data['SP.Momentum.12Mo.Negative']<0,0,inplace=True)
# verify on latest 100 entries for each
(schiller_data['SP.Price.Last'] < schiller_data['SP.Price'])[-100:-1].value_counts()
schiller_data['SP.Momentum.1Mo.Negative'][-100:-1].value_counts()
(schiller_data['SP.Price.Prev.6Mo'] < schiller_data['SP.Price'])[-100:-1].value_counts()
schiller_data['SP.Momentum.6Mo.Negative'][-100:-1].value_counts()
(schiller_data['SP.Price.Prev.12Mo'] < schiller_data['SP.Price'])[-100:-1].value_counts()
schiller_data['SP.Momentum.12Mo.Negative'][-100:-1].value_counts()

## Yield Curve Status
yield_curve_fname = "../prepared_data/yield_curve_10y_3mo.tsv"
yield_curve_data = pd.read_csv(yield_curve_fname, sep="\t")

## AAII Sentiment Data
aaii_filename = "../prepared_data/sentiment_data_formatted.tsv"
aaii_data = pd.read_csv(aaii_filename, sep="\t")

# Associate the most recent sentiment data report with each month
aaii_data = aaii_data.astype({"ReportedDate": np.datetime64})
aaii_data['Ceiling.Month'] = aaii_data['ReportedDate'] + pd.offsets.MonthBegin(0)
# keep only the latest entry for each month
aaii_data_reverse = aaii_data.sort_index(ascending=False)
aaii_data_reverse_dupl = aaii_data_reverse['Ceiling.Month'].duplicated()
aaii_data_monthly = aaii_data_reverse[aaii_data_reverse_dupl==False].sort_index(ascending=True)

## T-bill rate for risk premium calculation
TB3MS = pd.read_csv("../raw_data/TB3MS.csv")

# format dates???
# schiller_data.dtypes
# yield_curve_data.dtypes
schiller_data = schiller_data.astype({"Date": np.datetime64})
yield_curve_data = yield_curve_data.astype({"DATE": np.datetime64})

TB3MS = TB3MS.astype({"DATE": np.datetime64})

## join all the tables

# join schiller data and yield curve
joined_data = pd.merge(schiller_data, yield_curve_data, how="left", left_on="Date", right_on="DATE")

# verify the join
schiller_data.shape
joined_data.shape
schiller_data['Date'].tail()
yield_curve_data['DATE'].head()
yield_curve_data['DATE'].tail()
joined_data[['Date','Yield.Curve.Status']].head()
joined_data[['Date','Yield.Curve.Status']].tail()
yield_curve_data['Yield.Curve.Status'].value_counts()
joined_data['Yield.Curve.Status'].value_counts()

# add t-bill rate data
joined_data = pd.merge(joined_data, TB3MS, how="left", left_on="Date", right_on="DATE")
joined_data.shape
joined_data[['Date','TB3MS']].head()
joined_data[['Date','TB3MS']].tail()
joined_data['TB3MS'].describe()
TB3MS['TB3MS'].describe()
joined_data['Tbill.Rate'] = np.divide(joined_data['TB3MS'],100)
joined_data['Tbill.Rate'].tail()

# TODO add in 10-year treasury data and calculate yield curve here

# add aaii data
aaii_data_monthly.head()
joined_data = pd.merge(joined_data, aaii_data_monthly, how="left", left_on="Date", right_on="Ceiling.Month")
joined_data.shape
joined_data[['Date','Bullish']].head()
joined_data[['Date','Bullish']].tail()
aaii_data_monthly['Bullish'].describe()
joined_data['Bullish'].describe()

# calculate risk premium
# use CAPE for earnings yield for smoothing
joined_data['Earnings.Yield'] = np.divide(1,joined_data['CAPE'])
joined_data[['Date','Earnings.Yield','CAPE']].tail()
joined_data['SP.Risk.Premium.Tbill'] = np.subtract(joined_data['Earnings.Yield'], joined_data['Tbill.Rate'])
joined_data[['Date','SP.Risk.Premium.Tbill','Earnings.Yield','Tbill.Rate']].tail()
# use np.sign to preserve NaN cases
joined_data['Low.Risk.Premium'] = np.sign(np.subtract(0.01,joined_data['SP.Risk.Premium.Tbill']))
# consider cases where risk premium = 0.01 to be high risk premium
joined_data['Low.Risk.Premium'].mask(joined_data['Low.Risk.Premium']<0,0,inplace=True)

import pdb; pdb.set_trace()

# Construct the target variable
# For a Naive Bayes model we need a categorical target, so just use a binary variable
# indicating whether the S&P 500 or 10-year treasury performs better 
# on a one-month forward basis

# use np.sign to preserve NaN cases
joined_data['SP.Outperforms.GS10'] = np.sign(np.subtract(joined_data['SP.Return.Forward'], joined_data['GS10.Return.Forward']))
# tie goes to bonds
joined_data['SP.Outperforms.GS10'].mask(joined_data['SP.Outperforms.GS10']<0,0,inplace=True)

# verify
joined_data[['Date','SP.Outperforms.GS10','SP.Return.Forward','GS10.Return.Forward']].tail()
joined_data['SP.Outperforms.GS10'].value_counts()


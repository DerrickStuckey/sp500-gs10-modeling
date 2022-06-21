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
TB3MS['Tbill.Rate'] = np.divide(TB3MS['TB3MS'],100)

## 10-year treasury data series for treasury spread (aka yield curve)
GS10 = pd.read_csv("../raw_data/GS10.csv")
GS10['Treasury.10Y.Rate'] = np.divide(GS10['GS10'],100)

# format dates
schiller_data.dtypes
GS10.dtypes
schiller_data = schiller_data.astype({"Date": np.datetime64})
GS10 = GS10.astype({"DATE": np.datetime64})
TB3MS = TB3MS.astype({"DATE": np.datetime64})

## join all the tables

# join 10-year treasury data
joined_data = pd.merge(schiller_data, GS10, how="left", left_on="Date", right_on="DATE")
# verify the join
schiller_data.shape
joined_data.shape
schiller_data['Date'].tail()
GS10['DATE'].head()
GS10['DATE'].tail()
joined_data[['Date','Treasury.10Y.Rate']].head()
joined_data[['Date','Treasury.10Y.Rate']].tail()
GS10['Treasury.10Y.Rate'].describe()
joined_data['Treasury.10Y.Rate'].describe()

# add t-bill rate data
joined_data = pd.merge(joined_data, TB3MS, how="left", left_on="Date", right_on="DATE")
joined_data.shape
joined_data[['Date','TB3MS']].head()
joined_data[['Date','TB3MS']].tail()
joined_data['TB3MS'].describe()
TB3MS['TB3MS'].describe()
joined_data['Tbill.Rate'].tail()

# add aaii data
aaii_data_monthly.head()
joined_data = pd.merge(joined_data, aaii_data_monthly, how="left", left_on="Date", right_on="Ceiling.Month")
joined_data.shape
joined_data[['Date','Bullish']].head()
joined_data[['Date','Bullish']].tail()
aaii_data_monthly['Bullish'].describe()
joined_data['Bullish'].describe()

# TODO calculate yield curve
# import pdb; pdb.set_trace()
joined_data['GS10.Tbill.Spread'] = np.subtract(joined_data['Treasury.10Y.Rate'], joined_data['Tbill.Rate'])
joined_data['GS10.Tbill.Spread'].describe()
joined_data['GS10.Tbill.Spread.Positive'] = np.sign(joined_data['GS10.Tbill.Spread'])
joined_data['GS10.Tbill.Spread.Positive'].value_counts()
# consider 0 spread to be negative
joined_data['GS10.Tbill.Spread.Positive'].mask(joined_data['GS10.Tbill.Spread.Positive']<0,0,inplace=True)
joined_data['GS10.Tbill.Spread.Positive'].value_counts()

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

joined_data.to_csv('./naive_bayes_model_training_data.tsv',sep="\t",index=False)


import pandas as pd

# S&P 500 and 10-year treasury returns data
# from monthly since 1871
schiller_data_prepared_fname = "../prepared_data/schiller_sp_data_withreturns.tsv"
schiller_data = pd.read_csv(schiller_data_prepared_fname, sep="\t")
schiller_data.dtypes

# Add Momentum Factors

# use CAPE for earnings yield for smoothing


# Add Yield Curve Status
yield_curve_fname = "../prepared_data/yield_curve_10y_3mo.tsv"
yield_curve_data = pd.read_csv(yield_curve_fname, sep="\t")

# Add AAII Sentiment Data
aaii_filename = "../prepared_data/sentiment_data_formatted.tsv"
aaii_data = pd.read_csv(aaii_filename, sep="\t")

# Associate the most recent sentiment data report with each month

# Add Risk Premium vs T-bill rate
TB3MS = pd.read_csv("../raw_data/TB3MS.csv")

import pdb; pdb.set_trace()

# format dates???
schiller_data.dtypes
yield_curve_data.dtypes

# join all the tables
joined_data = pd.merge(schiller_data, yield_curve_data, how="left", left_on="Date", right_on="DATE")

# verify the join
schiller_data['Date'].tail()
yield_curve_data['Date'].head()
yield_curve_data['Date'].tail()
joined_data[['Date','Yield.Curve.Status']].head()
joined_data[['Date','Yield.Curve.Status']].tail()
yield_curve_data['Yield.Curve.Status'].value_counts()
joined_data['Yield.Curve.Status'].value_counts()

# Construct the target variable
# For a Naive Bayes model we need a categorical target, so just use a binary variable
# indicating whether the S&P 500 or 10-year treasury performs better 
# on a one-month forward basis


# sp500-gs10-modeling
R and python scripts to model price changes and returns for the S&P 500 and 10-year treasury bonds, against various data sources (primarily from FRED)

## Dataprep
#### Schiller S&P 500 monthly returns data
`prepared_data/schiller_sp_data_withreturns.tsv`
* download Schiller S&P 500 monthly data from http://www.econ.yale.edu/~shiller/data/ie_data.xls  
  * save 'data' tab as tab-delimited text
  * move to `./raw_data/schiller_sp_data.txt`
* run `dataprep/format_schiller_returns.py`
* run `dataprep/calculate_schiller_returns.py`

#### Treasury Yield Curve (10y vs 3mo)
`prepared_data/yield_curve_10y_3mo.tsv`
* download 10-year yield data from https://fred.stlouisfed.org/series/GS10
  * save as `raw_data/GS10.csv`
* download 3-month T-bill rate data from https://fred.stlouisfed.org/series/TB3MS
  * save as `raw_data/TB3MS.csv`
* run `dataprep/yield_curve_calc.R`

#### AAII Sentiment Data
`prepared_data/sentiment_data_formatted.tsv`
* download sentiment data from https://www.aaii.com/files/surveys/sentiment.xls
  * save as tab-delimited text as `raw_data/sentiment.txt`
* run `dataprep/aaii_sentiment_prep.py`

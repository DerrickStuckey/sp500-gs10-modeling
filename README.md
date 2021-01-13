# sp500-gs10-modeling
R and python scripts to model price changes and returns for the S&P 500 and 10-year treasury bonds, against various data sources (primarily from FRED)

## Dataprep
Download Schiller S&P 500 monthly data from http://www.econ.yale.edu/~shiller/data/ie_data.xls
Save 'data' tab as ./raw_data/schiller_sp_data.txt
Run:
* dataprep/format_schiller_returns.py
* dataprep/calculate_schiller_returns.py
* dataprep/calculate_schiller_returns_cumulative.py


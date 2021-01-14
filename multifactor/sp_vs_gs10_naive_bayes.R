# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors
library(tidyverse)
library(lubridate)

# S&P 500 and 10-year treasury returns data
# monthly since 1871
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

## S&P forward return vs individual factors:

# 6-month price momentum
sp.data$SP.Momentum.6Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.6Mo
boxplot(sp.data$SP.Return.Forward-1 ~ sp.data$SP.Momentum.6Mo>0, outline=FALSE)

# 12-month price momentum
sp.data$SP.Momentum.12Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.12Mo
boxplot(sp.data$SP.Return.Forward-1 ~ sp.data$SP.Momentum.12Mo>0, outline=FALSE)

# Commercial and Industrial Loan growth


# Yield Curve


# Sentiment
sentiment.data <- read.csv("./prepared_data/sentiment_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
tail(sentiment.data)
# clean up dates
sentiment.data$ReportedDate <- lubridate::mdy(sentiment.data$ReportedDate)
sentiment.data$CeilingMonth <- sentiment.data$ReportedDate %>%
  ceiling_date(unit = "months")
# dedupe to one entry per month - keeping the most latest entry for that month
sentiment.data.monthly <- sentiment.data %>%
  arrange(CeilingMonth, desc(ReportedDate))
sentiment.data.monthly <- sentiment.data.monthly %>%
  distinct(CeilingMonth, .keep_all = TRUE)
# merge with S&P Data
sp.data.sentiment <- sp.data %>%
  inner_join(sentiment.data.monthly, by=c("Date"="CeilingMonth"))

# analyze the relationship between Bullish Sentiment and 1-month forward S&P 500 Return
plot(sp.data.sentiment$Bullish, sp.data.sentiment$Log.SP.Return.Forward)
sp.data.sentiment %>%
  select(Bullish, Log.SP.Return.Forward) %>%
  drop_na() %>%
  cor()

# Valuation (Risk Premium)


# Change in Interest Rates


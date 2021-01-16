# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors
library(tidyverse)
library(lubridate)

# S&P 500 and 10-year treasury returns data
# from monthly since 1871
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

sp.data.train <- sp.data %>% filter(Date < "2005-01-01")
sp.data.test <- sp.data %>% filter(Date >= "2005-01-01")
tail(sp.data.train)

### S&P forward return vs individual factors:

# 6-month price momentum
sp.data.train$SP.Momentum.6Mo <- sp.data.train$SP.Price - sp.data.train$SP.Price.Prev.6Mo
boxplot(sp.data.train$SP.Return.Forward-1 ~ sp.data.train$SP.Momentum.6Mo>0, outline=FALSE)

# 12-month price momentum
sp.data.train$SP.Momentum.12Mo <- sp.data.train$SP.Price - sp.data.train$SP.Price.Prev.12Mo
boxplot(sp.data.train$SP.Return.Forward-1 ~ sp.data.train$SP.Momentum.12Mo>0, outline=FALSE)


## Yield Curve (10-year 3-month spread, monthly)
# from dataprep/yield_curve_calc.R
yield.curve.data <- read.csv("./prepared_data/yield_curve_10y_3mo.tsv",sep="\t")
head(yield.curve.data)
yield.curve.data$DATE <- as.Date(yield.curve.data$DATE, format="%Y-%m-%d")
sp.data.train.yc <- sp.data.train %>% inner_join(yield.curve.data, by=c("Date"="DATE"))
# sp.data.train.yc %>%
#   select(Date,SP.Price,GS10.x,spread) %>%
#   View()

# view the relationship between positive / inverted spreads and S&P forward returns
boxplot(sp.data.train.yc$SP.Return.Forward-1 ~ sp.data.train.yc$Yield.Curve.Status)


## Sentiment
sentiment.data <- read.csv("./prepared_data/sentiment_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
head(sentiment.data)
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
sp.data.train.sentiment <- sp.data.train %>%
  inner_join(sentiment.data.monthly, by=c("Date"="CeilingMonth"))

# analyze the relationship between Bullish Sentiment and 1-month forward S&P 500 Return
plot(sp.data.train.sentiment$Bullish, sp.data.train.sentiment$Log.SP.Return.Forward)
sp.data.train.sentiment %>%
  select(Bullish, Log.SP.Return.Forward) %>%
  drop_na() %>%
  cor()
boxplot(sp.data.train.sentiment$Log.SP.Return.Forward ~ sp.data.train.sentiment$Bullish > mean(sp.data.train.sentiment$Bullish))

## Valuation (Risk Premium)
# sp.data.train$Earnings.Yield <- sp.data.train$Earnings / sp.data.train$SP.Price
sp.data.train$Earnings.Yield <- 1/sp.data.train$CAPE
sp.data.train$SP.Risk.Premium <- sp.data.train$Earnings.Yield - sp.data.train$GS10
summary(sp.data.train$SP.Risk.Premium)
hist(sp.data.train$SP.Risk.Premium)

# check out the relationship between risk premium and forward S&P returns
plot(y=sp.data.train$SP.Return.Forward-1, x=sp.data.train$SP.Risk.Premium)
sp.data.train %>% select(SP.Return.Forward, SP.Risk.Premium) %>%
  drop_na() %>%
  cor()

# try against real interest rates
sp.data.train$CPI.12mo.ago <- lag(sp.data.train$CPI,12)
# sp.data.train %>% select(CPI, CPI.12mo.ago) %>% View()
sp.data.train$CPI.YoY.Change <- sp.data.train$CPI / sp.data.train$CPI.12mo.ago - 1
tail(sp.data.train$CPI.YoY.Change)
sp.data.train$GS10.Real <- sp.data.train$GS10 - sp.data.train$CPI.YoY.Change
hist(sp.data.train$GS10.Real)
sp.data.train$SP.Risk.Premium.Real <- sp.data.train$Earnings.Yield - sp.data.train$GS10.Real

# relationship between real risk premium and forward S&P returns
plot(y=sp.data.train$SP.Return.Forward-1, x=sp.data.train$SP.Risk.Premium.Real)
sp.data.train %>% select(SP.Return.Forward, SP.Risk.Premium.Real) %>%
  drop_na() %>%
  cor()

# try using T-bill rate


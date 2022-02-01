# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors

# dataprep only: join all relevant datasets and perform all feature engineering

library(tidyverse)
library(lubridate)
library(e1071)
library(caret)
library(reshape2)

# S&P 500 and 10-year treasury returns data
# from monthly since 1871
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

train.test.cutoff <- as.Date("2005-01-01")

## Compute and/or join with predictors

# Add Momentum Factors
sp.data$SP.Momentum.6Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.6Mo
sp.data$SP.Momentum.6Mo.Negative <- sp.data$SP.Momentum.6Mo < 0
sp.data$SP.Momentum.1Mo <- sp.data$SP.Price - sp.data$SP.Price.Last
sp.data$SP.Momentum.1Mo.Negative <- sp.data$SP.Momentum.1Mo < 0
sp.data$SP.Momentum.12Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.12Mo
sp.data$SP.Momentum.12Mo.Negative <- sp.data$SP.Momentum.12Mo < 0

# Add Yield Curve Status
yield.curve.data <- read.csv("./prepared_data/yield_curve_10y_3mo.tsv",sep="\t")
head(yield.curve.data)
yield.curve.data$DATE <- as.Date(yield.curve.data$DATE, format="%Y-%m-%d")
# Only keep needed columns
yield.curve.data <- yield.curve.data %>% select(DATE,GS10.Tbill.Spread,Yield.Curve.Status)
sp.data <- sp.data %>% left_join(yield.curve.data, by=c("Date"="DATE"))
table(sp.data$Yield.Curve.Status)
sp.data$Yield.Curve.Inverted <- sp.data$Yield.Curve.Status=="Inverted"

# Add AAII Sentiment Data
sentiment.data <- read.csv("./prepared_data/sentiment_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
head(sentiment.data)
sentiment.data$ReportedDate <- lubridate::mdy(sentiment.data$ReportedDate)
# Associate the most recent sentiment data report with each month
sentiment.data$CeilingMonth <- sentiment.data$ReportedDate %>%
  ceiling_date(unit = "months")
sentiment.data.monthly <- sentiment.data %>%
  arrange(CeilingMonth, desc(ReportedDate))
sentiment.data.monthly <- sentiment.data.monthly %>%
  distinct(CeilingMonth, .keep_all = TRUE)
head(sentiment.data.monthly)
# join with the S&P data
sentiment.data.monthly <- sentiment.data.monthly %>% select(CeilingMonth,Bullish)
sp.data <- sp.data %>%
  left_join(sentiment.data.monthly, by=c("Date"="CeilingMonth"))
# construct a bineary feature
sp.data$Bullish.High <- sp.data$Bullish > 0.5

# Add Risk Premium vs T-bill rate
TB3MS <- read.csv("./raw_data/TB3MS.csv", stringsAsFactors = FALSE)
names(TB3MS)[2] <- "Tbill.Rate"
TB3MS$Tbill.Rate <- TB3MS$Tbill.Rate / 100
TB3MS$DATE <- as.Date(TB3MS$DATE)
head(TB3MS)
sp.data <- sp.data %>% 
  left_join(TB3MS,by=c("Date"="DATE"))

# use CAPE for earnings yield for smoothing
sp.data$Earnings.Yield <- 1/sp.data$CAPE
sp.data$SP.Risk.Premium.Tbill <- sp.data$Earnings.Yield - sp.data$Tbill.Rate

sp.data$Low.Risk.Premium <- sp.data$SP.Risk.Premium.Tbill < 0.01
table(sp.data$Low.Risk.Premium)

# Construct the target variable
# For a Naive Bayes model we need a categorical target, so just use a binary variable
# indicating whether the S&P 500 or 10-year treasury performs better 
# on a one-month forward basis
sp.data <- sp.data %>%
  mutate(
    SP.Outperforms.GS10 = as.factor(SP.Return.Forward > GS10.Return.Forward)
  )
summary(sp.data$SP.Outperforms.GS10)

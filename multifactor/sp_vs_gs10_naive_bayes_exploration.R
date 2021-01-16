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
sp.data.train %>% group_by(SP.Momentum.6Mo>0) %>%
  drop_na(`SP.Momentum.6Mo > 0`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

# 12-month price momentum
sp.data.train$SP.Momentum.12Mo <- sp.data.train$SP.Price - sp.data.train$SP.Price.Prev.12Mo
boxplot(sp.data.train$SP.Return.Forward-1 ~ sp.data.train$SP.Momentum.12Mo>0, outline=FALSE)
sp.data.train %>% group_by(SP.Momentum.12Mo>0) %>%
  drop_na(`SP.Momentum.12Mo > 0`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

# 6 and 12-month price momentum together
boxplot(sp.data.train$SP.Return.Forward-1 ~ (sp.data.train$SP.Momentum.6Mo>0) * (sp.data.train$SP.Momentum.12Mo>0), outline=FALSE)
sp.data.train %>% group_by(SP.Momentum.6Mo>0,SP.Momentum.12Mo>0) %>%
  drop_na(`SP.Momentum.6Mo > 0`,`SP.Momentum.12Mo > 0`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

# 1-month price momentum
sp.data.train$SP.Momentum.1Mo <- sp.data.train$SP.Price - sp.data.train$SP.Price.Last
boxplot(sp.data.train$SP.Return.Forward-1 ~ sp.data.train$SP.Momentum.1Mo>0, outline=FALSE)
aggregate(sp.data.train$SP.Return.Forward-1, by=list(sp.data.train$SP.Momentum.1Mo>0),
          FUN=mean)
sp.data.train %>% group_by(SP.Momentum.1Mo>0) %>%
  drop_na(`SP.Momentum.1Mo > 0`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

# 1 and 12-month price momentum together
boxplot(sp.data.train$SP.Return.Forward-1 ~ (sp.data.train$SP.Momentum.1Mo>0) * (sp.data.train$SP.Momentum.12Mo>0), outline=FALSE)
sp.data.train %>% group_by(SP.Momentum.1Mo>0,SP.Momentum.12Mo>0) %>%
  drop_na(`SP.Momentum.1Mo > 0`,`SP.Momentum.12Mo > 0`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

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
plot(sp.data.train.sentiment$Bearish, sp.data.train.sentiment$Log.SP.Return.Forward)
sp.data.train.sentiment %>%
  select(Bullish, Log.SP.Return.Forward) %>%
  drop_na() %>%
  cor()
boxplot(sp.data.train.sentiment$Log.SP.Return.Forward ~ sp.data.train.sentiment$Bullish > mean(sp.data.train.sentiment$Bullish))

# look for a good cutoff
sp.data.train.sentiment %>% group_by(Bullish>0.5) %>%
  drop_na(`Bullish > 0.5`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )


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
TB3MS <- read.csv("./raw_data/TB3MS.csv", stringsAsFactors = FALSE)
head(TB3MS)
tail(TB3MS)
names(TB3MS)[2] <- "Tbill.Rate"
TB3MS$Tbill.Rate <- TB3MS$Tbill.Rate / 100
TB3MS$DATE <- as.Date(TB3MS$DATE)
sp.data.train.tbill <- sp.data.train %>% 
  inner_join(TB3MS,by=c("Date"="DATE"))

sp.data.train.tbill$SP.Risk.Premium.Tbill <- sp.data.train.tbill$Earnings.Yield - sp.data.train.tbill$Tbill.Rate
sp.data.train.tbill <- sp.data.train.tbill %>% 
  mutate(SP.Risk.Premium.Tbill.Real = Earnings.Yield - (Tbill.Rate - CPI.YoY.Change) )
summary(sp.data.train.tbill$SP.Risk.Premium.Tbill.Real)

# check out the relationship between risk premium and forward S&P returns
plot(y=sp.data.train.tbill$SP.Return.Forward-1, x=sp.data.train.tbill$SP.Risk.Premium.Tbill)
sp.data.train.tbill %>% select(SP.Return.Forward, SP.Risk.Premium.Tbill) %>%
  drop_na() %>%
  cor()
# real version
plot(y=sp.data.train.tbill$SP.Return.Forward-1, x=sp.data.train.tbill$SP.Risk.Premium.Tbill.Real)
sp.data.train.tbill %>% select(SP.Return.Forward, SP.Risk.Premium.Tbill.Real) %>%
  drop_na() %>%
  cor()


sp.data.train.tbill %>% group_by(SP.Risk.Premium.Tbill>0.01) %>%
  drop_na(`SP.Risk.Premium.Tbill > 0.01`) %>%
  summarize(
    Mean.Forward.Return=mean(SP.Return.Forward-1),
    Median.Forward.Return=median(SP.Return.Forward-1),
    Count=length(SP.Return.Forward-1)
  )

# how close are the two CAPE ratios?
sp.data %>% select(CAPE,CAPE.TR.Scaled) %>% drop_na() %>% cor()
sp.data %>% select(CAPE,CAPE.TR.Scaled) %>% drop_na() %>% plot()
plot(sp.data$Date, sp.data$CAPE.TR.Scaled - sp.data$CAPE)


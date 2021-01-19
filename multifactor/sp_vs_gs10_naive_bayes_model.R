# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors
library(tidyverse)
library(lubridate)
library(e1071)
library(caret)

# S&P 500 and 10-year treasury returns data
# from monthly since 1871
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

train.test.cutoff <- as.Date("2005-01-01")

## Compute and/or join with predictors

# Add 6-month momentum factor
sp.data$SP.Momentum.6Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.6Mo
sp.data$SP.Momentum.6Mo.Negative <- sp.data$SP.Momentum.6Mo < 0
# may get better performance with a combination of momentum factors, 
# but don't want to overweight those too heavily vs other factors

# Try multiple momentum factors
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

# perform training / test split
sp.data.train <- sp.data %>% filter(Date < "2005-01-01")
sp.data.test <- sp.data %>% filter(Date >= "2005-01-01")
tail(sp.data.train)
head(sp.data.test)

# train the Naive Bayes model
nb.model <- naiveBayes(SP.Outperforms.GS10 ~ 
                         Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                       , data=sp.data.train)
nb.model

# performance against training data
sp.data.train$SP.Outperforms.GS10.Pred <- predict(nb.model, newdata=sp.data.train)
summary(sp.data.train$SP.Outperforms.GS10.Pred)
confusionMatrix(sp.data.train$SP.Outperforms.GS10.Pred,
      sp.data.train$SP.Outperforms.GS10)

# Balanced Accuracy against training data
# With all factors: 0.5728
# 3 Momentum factors only: 0.5677
# Yield curve only: 0.5728
# Bullish only: 0.50470
# Risk Premium only: 0.5 (always predicts yes)
# All except Bullish: 0.5719
# All except Yield Curve: 0.5739
# All except Risk Premium: 0.5707
# All except Momentum: 0.51128
# Original model (6-month momentum and other factors): 0.5487

# performance against test data
sp.data.test$SP.Outperforms.GS10.Pred <- predict(nb.model, newdata=sp.data.test)
summary(sp.data.test$SP.Outperforms.GS10.Pred)
confusionMatrix(sp.data.test$SP.Outperforms.GS10.Pred,
                sp.data.test$SP.Outperforms.GS10)

# Balanced Accuracy against test data
# All factors: 0.56040
# 3 momentum factors only: 0.56744
# Original model: 0.45900

# model return data
model.return.data <- sp.data.test %>% select(Date,
                                        Log.SP.Return.Forward,
                                        Log.GS10.Return.Forward,
                                        SP.Outperforms.GS10.Pred) %>%
  drop_na() %>%
  # filter(Date>"2010-01-01") %>%
  mutate(
    invest.in.sp = as.logical(SP.Outperforms.GS10.Pred),
    Log.Model.Return.Forward=Log.SP.Return.Forward*invest.in.sp +
      Log.GS10.Return.Forward*(!invest.in.sp),
    Model.Log.Cumulative.Return=cumsum(Log.Model.Return.Forward),
    SP.Log.Cumulative.Return=cumsum(Log.SP.Return.Forward),
    GS10.Log.Cumulative.Return=cumsum(Log.GS10.Return.Forward)
  )
tail(model.return.data)

# 
model.return.data %>%
  mutate() %>%
  summarize(
    start.date=min(Date),
    end.date=max(Date),
    num.months=length(Date),
    sp.return=exp(sum(Log.SP.Return.Forward)),
    gs10.return=exp(sum(Log.GS10.Return.Forward)),
    model.return.sp=exp(sum(Log.SP.Return.Forward*invest.in.sp)),
    model.return.gs10=exp(sum(Log.GS10.Return.Forward*(!invest.in.sp)))
    ) %>%
  mutate(
    model.total.return=model.return.sp * model.return.gs10
  )
  

# plot cumulative returns over time for model vs S&P and GS10
melted.return.data <- model.return.data %>% 
  select(Date,Model.Log.Cumulative.Return,
                             SP.Log.Cumulative.Return,
                             GS10.Log.Cumulative.Return) %>%
  rename(`S&P 500`=SP.Log.Cumulative.Return,
         `10-Year Treasury`=GS10.Log.Cumulative.Return,
         `Model`=Model.Log.Cumulative.Return) %>%
  melt(id.vars = c("Date"))

ggplot(data=melted.return.data, aes(x=Date,y=value,col=variable)) +
  geom_line(aes(group=variable)) + 
  ggtitle("Total Returns (Log-Scaled)") +
  ylab("Log Total Return")






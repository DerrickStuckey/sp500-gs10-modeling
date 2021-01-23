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

# perform training / test split
train.date.min <- as.Date("1950-01-01")
train.date.max <- as.Date("1980-01-01")
test.date.min <- as.Date("2005-01-01")
sp.data.train <- sp.data %>% filter(Date >= train.date.min & Date < train.date.max)
# sp.data.val <- sp.data %>% filter(Date >= "1980-01-01" & Date < "2005-01-01")
sp.data.test <- sp.data %>% filter(Date >= test.date.min)
dim(sp.data.train)
dim(sp.data.test)

# train the Naive Bayes models
nb.model.1 <- naiveBayes(SP.Outperforms.GS10 ~ 
                         Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                       , data=sp.data.train)

nb.model.2 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         # + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.3 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         # + SP.Momentum.1Mo.Negative
                         # + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.4 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         # + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.5 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         # + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         # + SP.Momentum.1Mo.Negative
                         # + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.6 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         # + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.7 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           # Yield.Curve.Inverted
                           Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.8 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         # + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.9 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         # + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.10 <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         # + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         # + SP.Momentum.12Mo.Negative
                         , data=sp.data.train)

nb.model.11 <- naiveBayes(SP.Outperforms.GS10 ~ 
                            # Yield.Curve.Inverted
                          # + Bullish.High
                          SP.Momentum.6Mo.Negative
                          # + Low.Risk.Premium
                          + SP.Momentum.1Mo.Negative
                          + SP.Momentum.12Mo.Negative
                          , data=sp.data.train)

## evaluate performance against training and/or validation data

# set of models to evaluate against training data
nb.models <- list("Full Model"=nb.model.1,
               "Exclude 6Mo"=nb.model.2,
               "6Mo and Other Factors"=nb.model.3,
               "Exclude 12Mo"=nb.model.4,
               "Exclude Momentum"=nb.model.5,
               "Exclude Sentiment"=nb.model.6,
               "Exclude Yield Curve"=nb.model.7,
               "Exclude Risk Premium"=nb.model.8,
               "Exclude 1Mo"=nb.model.9,
               "Exclude 6Mo and 12Mo"=nb.model.10,
               "1Mo, 6Mo, 12Mo Only"=nb.model.11)
names(nb.models)

# arrays to hold results
model.name.vals <- c()
balanced.accuracy.vals <- c()
model.total.performance.vals <- c()
sp.total.performance.vals <- c()
gs.total.performance.vals <- c()
min.date.vals <- c()
max.date.vals <- c()
num.periods.vals <- c()
periods.predicted.positive.vals <- c()

# dataframe for training and validation data (if)
sp.data.train.val <- sp.data %>% filter(Date >= train.date.min & Date < test.date.min)
period.starts <- c(train.date.min,train.date.max)
period.ends <- c(train.date.max,test.date.min)

# calculate performance of each version on the training data
for (model.name in names(nb.models)) {
  for (i in 1:length(period.starts)) {
    nb.model = nb.models[model.name][[1]]
    period.start <- period.starts[i]
    period.end <- period.ends[i]
    
    print(model.name)
    print(period.start)
    
    # performance against training and/or validation data
    sp.data.train.val$SP.Outperforms.GS10.Pred <- predict(nb.model, newdata=sp.data.train.val)
    # select subset of training data where prediction and reference are both available
    sp.data.train.val.subset <- sp.data.train.val %>% 
      filter(Date >= as.Date(period.start)) %>%
      filter(Date < as.Date(period.end)) %>%
      select(Date,
             Log.SP.Return.Forward,
             Log.GS10.Return.Forward,
             SP.Outperforms.GS10.Pred,
             SP.Outperforms.GS10) %>%
      drop_na()
    # confusion matrix stats
    cm <- confusionMatrix(sp.data.train.val.subset$SP.Outperforms.GS10.Pred,
                          sp.data.train.val.subset$SP.Outperforms.GS10)
    balanced.accuracy <- cm$byClass['Balanced Accuracy']
    num.periods <- sum(cm$table)
    periods.predicted.positive <- sum(sp.data.train.val.subset$SP.Outperforms.GS10.Pred=="TRUE")
    min.date <- min(sp.data.train.val.subset$Date) %>% as.character()
    max.date <- max(sp.data.train.val.subset$Date) %>% as.character()
    
    # monthly return stats
    model.return.data.train.subset <- sp.data.train.val.subset %>% mutate(
      invest.in.sp = as.logical(SP.Outperforms.GS10.Pred),
      Log.Model.Return.Forward=Log.SP.Return.Forward*invest.in.sp +
        Log.GS10.Return.Forward*(!invest.in.sp),
      Model.Log.Cumulative.Return=cumsum(Log.Model.Return.Forward),
      SP.Log.Cumulative.Return=cumsum(Log.SP.Return.Forward),
      GS10.Log.Cumulative.Return=cumsum(Log.GS10.Return.Forward)
    )
    
    # total return stats for selected period
    total.return.stats.train.subset <- model.return.data.train.subset %>%
      mutate() %>%
      summarize(
        start.date=min(Date),
        end.date=max(Date),
        num.months=length(Date),
        sp.return=exp(sum(Log.SP.Return.Forward))-1,
        gs10.return=exp(sum(Log.GS10.Return.Forward))-1,
        model.total.return=exp(sum(Log.Model.Return.Forward))-1
      )
    
    sp.total.performance <- total.return.stats.train.subset$sp.return
    gs.total.performance <- total.return.stats.train.subset$gs10.return
    model.total.performance <- total.return.stats.train.subset$model.total.return
    
    # append each value to its array
    model.name.vals <- c(model.name.vals, model.name)
    balanced.accuracy.vals <- c(balanced.accuracy.vals, balanced.accuracy)
    model.total.performance.vals <- c(model.total.performance.vals, model.total.performance)
    sp.total.performance.vals <- c(sp.total.performance.vals, sp.total.performance)
    gs.total.performance.vals <- c(gs.total.performance.vals, gs.total.performance)
    min.date.vals <- c(min.date.vals, min.date)
    max.date.vals <- c(max.date.vals, max.date)
    num.periods.vals <- c(num.periods.vals, num.periods)
    periods.predicted.positive.vals <- c(periods.predicted.positive.vals, periods.predicted.positive)
  }
}

# write out results to a dataframe
training.results.df <- data.frame(
  "model.name"=model.name.vals,
  "trained.on"=paste(as.character(train.date.min),as.character(train.date.max),sep=" to "),
  "balanced.accuracy"=balanced.accuracy.vals,
  "model.total.performance"=model.total.performance.vals,
  "sp.total.performance"=sp.total.performance.vals,
  "gs.total.performance"=gs.total.performance.vals,
  "min.date"=min.date.vals,
  "max.date"=max.date.vals,
  "num.periods"=num.periods.vals,
  "periods.predicted.positive"=periods.predicted.positive.vals
)

# View(training.results.df)
write.table(training.results.df, file="./multifactor/naive_bayes_results/training_data_results.tsv",
            sep="\t",row.names = FALSE)


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


## evaluate performance against test data

# retrain models on training and validation data
nb.model.full <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data.train.val)

nb.model.mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                              SP.Momentum.6Mo.Negative
                            + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train.val)

nb.model.6mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            + SP.Momentum.6Mo.Negative
                            + Low.Risk.Premium
                            # + SP.Momentum.1Mo.Negative
                            # + SP.Momentum.12Mo.Negative
                            , data=sp.data.train.val)

# set of models to evaluate against test data
nb.models <- list("Full Model"=nb.model.full,
                  "1Mo, 6Mo, 12Mo Only"=nb.model.mo,
                  "6Mo and Other Factors"=nb.model.6mo)
names(nb.models)

# arrays to hold results
model.name.vals <- c()
balanced.accuracy.vals <- c()
model.total.performance.vals <- c()
sp.total.performance.vals <- c()
gs.total.performance.vals <- c()
min.date.vals <- c()
max.date.vals <- c()
num.periods.vals <- c()
periods.predicted.positive.vals <- c()

for (model.name in names(nb.models)) {
  nb.model = nb.models[model.name][[1]]
  
  # calculate the predictions for the model
  sp.data.test$SP.Outperforms.GS10.Pred <- predict(nb.model, newdata=sp.data.test)
  
  # subset of test data where prediction and reference are both available
  sp.data.test.subset <- sp.data.test %>% 
    select(Date,
           Log.SP.Return.Forward,
           Log.GS10.Return.Forward,
           SP.Outperforms.GS10.Pred,
           SP.Outperforms.GS10) %>%
    drop_na()
  
  # confusion matrix stats
  cm <- confusionMatrix(sp.data.test.subset$SP.Outperforms.GS10.Pred,
                        sp.data.test.subset$SP.Outperforms.GS10)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  num.periods <- sum(cm$table)
  periods.predicted.positive <- sum(sp.data.test.subset$SP.Outperforms.GS10.Pred=="TRUE")
  min.date <- min(sp.data.test.subset$Date) %>% as.character()
  max.date <- max(sp.data.test.subset$Date) %>% as.character()
  
  # monthly return stats
  model.return.data.test.subset <- sp.data.test.subset %>% mutate(
    invest.in.sp = as.logical(SP.Outperforms.GS10.Pred),
    Log.Model.Return.Forward=Log.SP.Return.Forward*invest.in.sp +
      Log.GS10.Return.Forward*(!invest.in.sp),
    Model.Log.Cumulative.Return=cumsum(Log.Model.Return.Forward),
    SP.Log.Cumulative.Return=cumsum(Log.SP.Return.Forward),
    GS10.Log.Cumulative.Return=cumsum(Log.GS10.Return.Forward)
  )
  
  # total return stats for selected period
  total.return.stats.test.subset <- model.return.data.test.subset %>%
    mutate() %>%
    summarize(
      start.date=min(Date),
      end.date=max(Date),
      num.months=length(Date),
      sp.return=exp(sum(Log.SP.Return.Forward))-1,
      gs10.return=exp(sum(Log.GS10.Return.Forward))-1,
      model.total.return=exp(sum(Log.Model.Return.Forward))-1
    )
  
  sp.total.performance <- total.return.stats.test.subset$sp.return
  gs.total.performance <- total.return.stats.test.subset$gs10.return
  model.total.performance <- total.return.stats.test.subset$model.total.return
  
  # append each value to its array
  model.name.vals <- c(model.name.vals, model.name)
  balanced.accuracy.vals <- c(balanced.accuracy.vals, balanced.accuracy)
  model.total.performance.vals <- c(model.total.performance.vals, model.total.performance)
  sp.total.performance.vals <- c(sp.total.performance.vals, sp.total.performance)
  gs.total.performance.vals <- c(gs.total.performance.vals, gs.total.performance)
  min.date.vals <- c(min.date.vals, min.date)
  max.date.vals <- c(max.date.vals, max.date)
  num.periods.vals <- c(num.periods.vals, num.periods)
  periods.predicted.positive.vals <- c(periods.predicted.positive.vals, periods.predicted.positive)
  
  # plot cumulative returns over time for model vs S&P and GS10
  melted.return.data <- model.return.data.test.subset %>%
    select(Date,Model.Log.Cumulative.Return,
           SP.Log.Cumulative.Return,
           GS10.Log.Cumulative.Return) %>%
    rename(`S&P 500`=SP.Log.Cumulative.Return,
           `10-Year Treasury`=GS10.Log.Cumulative.Return,
           `Model`=Model.Log.Cumulative.Return) %>%
    melt(id.vars = c("Date"))
  
  p <- ggplot(data=melted.return.data, aes(x=Date,y=value,col=variable)) +
    geom_line(aes(group=variable)) +
    ggtitle(model.name) +
    ylab("Log Total Return")
  ggsave(plot=p,filename=paste("./multifactor/naive_bayes_results/",model.name," Test.png",sep=""))
  
  # plot model return difference with S&P
  p2 <- ggplot(data=model.return.data.test.subset,
         aes(x=Date,y=Model.Log.Cumulative.Return-SP.Log.Cumulative.Return)) +
    geom_line() +
    ggtitle(model.name) +
    ylab("Model Return - S&P 500 Return\n(Log-Scaled")
  ggsave(plot=p2,filename=paste("./multifactor/naive_bayes_results/",model.name," vs SP Test.png",sep=""))
}

# write out results to a dataframe
test.results.df <- data.frame(
  "trained.on"=paste(as.character(train.date.min),as.character(train.date.max),sep=" to "),
  "model.name"=model.name.vals,
  "balanced.accuracy"=balanced.accuracy.vals,
  "model.total.performance"=model.total.performance.vals,
  "sp.total.performance"=sp.total.performance.vals,
  "gs.total.performance"=gs.total.performance.vals,
  "eval.min.date"=min.date.vals,
  "eval.max.date"=max.date.vals,
  "num.periods"=num.periods.vals,
  "periods.predicted.positive"=periods.predicted.positive.vals
)

# View(test.results.df)
write.table(test.results.df, file="./multifactor/naive_bayes_results/test_data_results.tsv",
            sep="\t",row.names = FALSE)



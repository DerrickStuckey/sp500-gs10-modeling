# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors

# trains on data up to 2005, tests on subsequent periods

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

# inspect data before training/test split
# View(sp.data)

# perform training / test split
train.date.min <- as.Date("1950-01-01")
train.date.max <- as.Date("1980-01-01")
test.date.min <- as.Date("2005-01-01")
sp.data.train <- sp.data %>% filter(Date >= train.date.min & Date < train.date.max)
# sp.data.val <- sp.data %>% filter(Date >= "1980-01-01" & Date < "2005-01-01")
sp.data.test <- sp.data %>% filter(Date >= test.date.min)
dim(sp.data.train)
dim(sp.data.test)

## evaluate performance against test data

# retrain models on training and validation data
nb.model.full <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            + SP.Momentum.6Mo.Negative
                            + Low.Risk.Premium
                            + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train)

nb.model.mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                            SP.Momentum.6Mo.Negative
                          + SP.Momentum.1Mo.Negative
                          + SP.Momentum.12Mo.Negative
                          , data=sp.data.train)

nb.model.6mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                             Yield.Curve.Inverted
                           + Bullish.High
                           + SP.Momentum.6Mo.Negative
                           + Low.Risk.Premium
                           # + SP.Momentum.1Mo.Negative
                           # + SP.Momentum.12Mo.Negative
                           , data=sp.data.train)

nb.model.1mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                             Yield.Curve.Inverted
                           + Bullish.High
                           # + SP.Momentum.6Mo.Negative
                           + Low.Risk.Premium
                           + SP.Momentum.1Mo.Negative
                           # + SP.Momentum.12Mo.Negative
                           , data=sp.data.train)

nb.model.12mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            # + SP.Momentum.6Mo.Negative
                            + Low.Risk.Premium
                            # + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train)

nb.model.nomo <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            + Low.Risk.Premium
                            , data=sp.data.train)

nb.model.nosent <- naiveBayes(SP.Outperforms.GS10 ~ 
                                Yield.Curve.Inverted
                              + SP.Momentum.6Mo.Negative
                              + Low.Risk.Premium
                              + SP.Momentum.1Mo.Negative
                              + SP.Momentum.12Mo.Negative
                              , data=sp.data.train)

nb.model.norisk <- naiveBayes(SP.Outperforms.GS10 ~ 
                               Yield.Curve.Inverted
                             + Bullish.High
                             + SP.Momentum.6Mo.Negative
                             + SP.Momentum.1Mo.Negative
                             + SP.Momentum.12Mo.Negative
                             , data=sp.data.train)

nb.model.noyc <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Bullish.High
                            + SP.Momentum.6Mo.Negative
                            + Low.Risk.Premium
                            + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train)

# set of models to evaluate against test data
nb.models <- list("Full Model"=nb.model.full,
                  "1Mo, 6Mo, 12Mo Only"=nb.model.mo,
                  "6Mo and Other Factors"=nb.model.6mo,
                  "12Mo and Other Factors"=nb.model.12mo,
                  "1Mo and Other Factors"=nb.model.1mo,
                  "Exclude Momentum"=nb.model.nomo,
                  "Exclude Sentiment"=nb.model.nosent,
                  "Exclude Risk Premium"=nb.model.norisk,
                  "Exclude Yield Curve"=nb.model.noyc)
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

# directory for output
plot.dir <- "./multifactor/naive_bayes_results/plots/"
if (!file.exists("./multifactor/naive_bayes_results")) {
  dir.create(file.path("./multifactor/naive_bayes_results"))
}
if (!file.exists("./multifactor/naive_bayes_results/plots")) {
  dir.create(file.path("./multifactor/naive_bayes_results/plots/"))
}


# calculate and plot returns for each model against test data
for (model.name in names(nb.models)) {
  nb.model = nb.models[model.name][[1]]
  
  # calculate the predictions for the model
  sp.data.test$SP.Outperforms.GS10.Pred <- predict(nb.model, newdata=sp.data.test)
  
  # subset of test data where prediction and reference are both available
  sp.data.test.subset <- sp.data.test %>% 
    select(Date,
           SP.Price,
           GS10,
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
  
  # save monthly return stats
  write.table(model.return.data.test.subset, 
              file=paste("./multifactor/naive_bayes_results/",model.name," vs Test Monthly.tsv",sep=""),
              sep="\t",row.names = FALSE)
  
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
  ggsave(plot=p,filename=paste(plot.dir,model.name," Test.png",sep=""))
  
  # plot model return difference with S&P
  p2 <- ggplot(data=model.return.data.test.subset,
               aes(x=Date,y=Model.Log.Cumulative.Return-SP.Log.Cumulative.Return)) +
    geom_line() +
    ggtitle(model.name) +
    ylab("Model Return - S&P 500 Return\n(Log-Scaled)")
  ggsave(plot=p2,filename=paste("./multifactor/naive_bayes_results/plots/",model.name," vs SP Test.png",sep=""))
}

# write out results to a dataframe
test.results.df <- data.frame(
  "model.name"=model.name.vals,
  "trained.on"=paste(as.character(train.date.min),as.character(test.date.min),sep=" to "),
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



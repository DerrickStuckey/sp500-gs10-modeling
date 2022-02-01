# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors
library(tidyverse)
library(lubridate)
library(e1071)
library(caret)
library(reshape2)

# perform data prep
source("multifactor/sp_vs_gs10_naive_bayes_dataprep.R")

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
             SP.Price,
             GS10,
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
    
    # save monthly return stats
    if (i==1) {
      batch.label <- "Train"
    } else {
      batch.label <- "Validation"
    }
    write.table(model.return.data.train.subset, 
                file=paste("./multifactor/naive_bayes_results/",model.name," vs ",batch.label," Monthly.tsv",sep=""),
                sep="\t",row.names = FALSE)
    
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

nb.model.1mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                             Yield.Curve.Inverted
                           + Bullish.High
                           # + SP.Momentum.6Mo.Negative
                           + Low.Risk.Premium
                           + SP.Momentum.1Mo.Negative
                           # + SP.Momentum.12Mo.Negative
                           , data=sp.data.train.val)

nb.model.12mo <- naiveBayes(SP.Outperforms.GS10 ~ 
                             Yield.Curve.Inverted
                           + Bullish.High
                           # + SP.Momentum.6Mo.Negative
                           + Low.Risk.Premium
                           # + SP.Momentum.1Mo.Negative
                           + SP.Momentum.12Mo.Negative
                           , data=sp.data.train.val)

nb.model.nomo <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            + Low.Risk.Premium
                          , data=sp.data.train.val)

nb.model.nosent <- naiveBayes(SP.Outperforms.GS10 ~ 
                                Yield.Curve.Inverted
                              + SP.Momentum.6Mo.Negative
                              + Low.Risk.Premium
                              + SP.Momentum.1Mo.Negative
                              + SP.Momentum.12Mo.Negative
                              , data=sp.data.train.val)

nb.model.noval <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Yield.Curve.Inverted
                            + Bullish.High
                            + SP.Momentum.6Mo.Negative
                            + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train.val)

nb.model.noyc <- naiveBayes(SP.Outperforms.GS10 ~ 
                              Bullish.High
                            + SP.Momentum.6Mo.Negative
                            + Low.Risk.Premium
                            + SP.Momentum.1Mo.Negative
                            + SP.Momentum.12Mo.Negative
                            , data=sp.data.train.val)

# set of models to evaluate against test data
nb.models <- list("Full Model"=nb.model.full,
                  "1Mo, 6Mo, 12Mo Only"=nb.model.mo,
                  "6Mo and Other Factors"=nb.model.6mo,
                  "12Mo and Other Factors"=nb.model.12mo,
                  "1Mo and Other Factors"=nb.model.1mo,
                  "Exclude Momentum"=nb.model.nomo,
                  "Exclude Sentiment"=nb.model.nosent,
                  "Exclude Risk Premium"=nb.model.noval,
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
  ggsave(plot=p,filename=paste("./multifactor/naive_bayes_results/plots/",model.name," Test.png",sep=""))
  
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



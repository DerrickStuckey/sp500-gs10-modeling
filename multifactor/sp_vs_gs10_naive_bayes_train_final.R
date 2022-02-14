# A Naive Bayes model for market timing
# Investing in the S&P 500 or 10-year treasury based on various factors
# This script trains the full model using all available data for current predictors
library(tidyverse)
library(lubridate)
library(e1071)
library(caret)
library(reshape2)

# perform data prep
source("multifactor/sp_vs_gs10_naive_bayes_dataprep.R")

training.data <- sp.data %>% select(Date
                                    , SP.Outperforms.GS10
                                    , Yield.Curve.Inverted
                                    , Bullish.High
                                    , SP.Momentum.6Mo.Negative
                                    , Low.Risk.Premium
                                    , SP.Momentum.1Mo.Negative
                                    , SP.Momentum.12Mo.Negative) %>%
  filter(Date >= "1950-01-01")
head(training.data)
tail(training.data)

write.table(training.data, file="./multifactor/sp_vs_gs10_training_data.tsv",sep="\t",
            row.names = FALSE)

# train the Naive Bayes model
nb.model.final <- naiveBayes(SP.Outperforms.GS10 ~ 
                           Yield.Curve.Inverted
                         + Bullish.High
                         + SP.Momentum.6Mo.Negative
                         + Low.Risk.Premium
                         + SP.Momentum.1Mo.Negative
                         + SP.Momentum.12Mo.Negative
                         , data=sp.data)

nb.model.final$apriori
nb.model.final$tables[1]

## predictions for all possible scenarios

predictors <- names(nb.model.final$tables)

# construct data frame of all possible combinations of binary predictors
df <- data.frame("blah"=c(TRUE,FALSE))
names(df)[1] <- predictors[1]

for (i in 2:length(predictors)) {
  nr <- nrow(df)
  tmp <- rep(TRUE,nr)
  df.t <- cbind(df,tmp)
  tmp <- rep(FALSE,nr)
  df.f <- cbind(df,tmp)
  df <- rbind(df.t, df.f)
  names(df)[i] <- predictors[i]
}
df

# obtain binary predictions and predicted probability of S&P outperforming GS10
df$prediction <- predict(nb.model.final, newdata=df)
df$predicted.prob <- predict(nb.model.final, newdata=df, type="raw")[,2]

predictions.filename <- paste("./multifactor/naive_bayes_results/final_model_predictions_",
                              max(training.data$Date),
                              ".tsv",
                              sep="")

write.table(df, predictions.filename, sep = "\t", row.names=FALSE)




# 
# predictors <- c()
# pred.f.if.sp.f <- c()
# pred.t.if.sp.f <- c()
# pred.t.if.sp.f <- c()
# pred.t.if.sp.t <- c()
# 
# for (i in 1:length(nb.model.final$tables)) {
#   t <- nb.model.final$tables[i]
#   # print(t)
#   # print(names(t))
#   predictors <- c(predictors,
#                   names(t)[1])
#   pred.f.if.sp.f <- c(pred.f.if.sp.f, t[[1]][1,1])
#   pred.t.if.sp.f <- c(pred.t.if.sp.f, t[[1]][1,2])
#   pred.t.if.sp.f <- c(pred.t.if.sp.f, t[[1]][2,1])
#   pred.t.if.sp.t <- c(pred.t.if.sp.t, t[[1]][2,2])
#   # print(t[[1,1]])
#   # print(t[[2,2]])
# }
# 
# predictors
# pred.f.if.sp.f

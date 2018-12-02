
return.data <- read.csv("./schiller_data_with_returns.tsv", sep="\t", stringsAsFactors = FALSE)

# training/test split for "modern" era
premodern_data <- return.data[return.data$Year<1980,]
modern_data <- return.data[return.data$Year>=1980,]
modern_data_train <- modern_data[modern_data$Year<2005,]
modern_data_test <- modern_data[modern_data$Year>=2005,]

# build linear models to predict the log change in the S&P and GS10 NPV
log.sp.return.lm <- lm(Log.SP.Return.1.0 ~ SP.Change.7.1 + SP.Change.13.7 + log(Dividend.Yield.1.Month.Ago), data = modern_data_train)
summary(log.sp.return.lm)

log.gs10.return.lm <- lm(Log.GS10.Return.1.0 ~ GS10.NPV.Change.7.1 + GS10.NPV.Change.13.7 + log(GS10.1.Month.Ago), data = modern_data_train)
summary(log.gs10.return.lm)

modern_data_test$pred.log.sp.return <- predict(log.sp.return.lm, newdata = modern_data_test)
modern_data_test$pred.log.gs10.return <- predict(log.gs10.return.lm, newdata = modern_data_test)

write.table(modern_data_test,file="./modern_data_holdout_linear_preds2.tsv",sep="\t",row.names = FALSE)

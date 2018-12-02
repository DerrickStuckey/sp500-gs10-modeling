source("./utils/finance_utils.R")

data <- read.csv("./prepared_data/Schiller Market Data - Formatted Data.tsv", sep="\t", stringsAsFactors = FALSE)

# applicable region only
return.data <- data[!is.na(data$SP.13.Months.Ago),]
return.data <- return.data[!is.na(return.data$GS10),]
return.data <- return.data[!is.na(return.data$Dividend.Yield.),]

# proportional changes in S&P from month x to month y relative to current month 0
return.data$SP.Change.7.1 <- return.data$SP.1.Month.Ago / return.data$SP.7.Months.Ago
return.data$SP.Change.13.7 <- return.data$SP.7.Months.Ago / return.data$SP.13.Months.Ago
return.data$SP.Change.1.0 <- return.data$SP / return.data$SP.1.Month.Ago

# proportional changes in S&P from month x to month y relative to current month 0
return.data$GS10.NPV.Change.7.1 <- bond.npv(1,10,return.data$GS10.7.Months.Ago,return.data$GS10.1.Month.Ago)
return.data$GS10.NPV.Change.13.7 <- bond.npv(1,10,return.data$GS10.13.Months.Ago,return.data$GS10.7.Months.Ago)
return.data$GS10.NPV.Change.1.0 <- bond.npv(1,10,return.data$GS10.1.Month.Ago,return.data$GS10)

return.data$Log.SP.Change.1.0 <- log(return.data$SP.Change.1.0)
return.data$Log.GS10.NPV.Change.1.0 <- log(return.data$GS10.NPV.Change.1.0)
# hist(return.data$Log.SP.Change.1.0)

# approximate returns by including coupon + dividend data
# (assumes dividend yield, bond coupons divided evenly across months)
return.data$SP.Return.1.0 <- return.data$SP.Change.1.0 + (return.data$Dividend.Yield.1.Month.Ago / 12)
return.data$GS10.Return.1.0 <- return.data$GS10.NPV.Change.1.0 + (return.data$GS10.1.Month.Ago / 12)

return.data$Log.SP.Return.1.0 <- log(return.data$SP.Return.1.0)
return.data$Log.GS10.Return.1.0 <- log(return.data$GS10.Return.1.0)

write.table(return.data,file="./prepared_data/schiller_data_with_returns.tsv",sep="\t",row.names = FALSE)


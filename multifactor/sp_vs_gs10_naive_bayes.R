
# S&P 500 and 10-year treasury returns data
# monthly since 1871
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

## S&P forward return vs individual factors:

# 6-month price momentum
sp.data$SP.Momentum.6Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.6Mo
boxplot(sp.data$SP.Return.Forward-1 ~ sp.data$SP.Momentum.6Mo>0, outline=FALSE)

# 12-month price momentum
sp.data$SP.Momentum.12Mo <- sp.data$SP.Price - sp.data$SP.Price.Prev.12Mo
boxplot(sp.data$SP.Return.Forward-1 ~ sp.data$SP.Momentum.12Mo>0, outline=FALSE)

# Commercial and Industrial Loan growth


# Yield Curve


# Sentiment


# Valuation


# Change in Interest Rates


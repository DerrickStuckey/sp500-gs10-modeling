# from http://www.econ.yale.edu/~shiller/data.htm
sp.data <- read.csv("./schiller_sp_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)

# from https://fred.stlouisfed.org/series/FEDFUNDS
fedfunds <- read.csv("./FEDFUNDS_1954_2018.csv",sep=",",stringsAsFactors = FALSE)
head(fedfunds)

# format dates
sp.data$Date <- as.POSIXct(sp.data$Date)
fedfunds$DATE <- as.POSIXct(fedfunds$DATE)

# merge the datasets
combined.data <- merge(sp.data, fedfunds, by.x="Date",by.y="DATE")
head(combined.data)
tail(combined.data)
nrow(combined.data)
nrow(fedfunds)
nrow(sp.data)

# Fed Funds rate over time
plot(combined.data$Date,combined.data$FEDFUNDS,type="l")

# Yield curve
combined.data$Yield.Curve.10Y <- combined.data$GS10 - combined.data$FEDFUNDS
plot(combined.data$Date,combined.data$Yield.Curve.10Y,type="l",
     main="Yield Curve (10-Year minus Fed Funds)",xlab="Date",ylab="10Y-FF",
     col="blue")
abline(h=0)

# add future 1-month S&P 500 price change
combined.data$SP.Price.Change.Forward <- (combined.data$SP.Price.Next / combined.data$SP.Price - 1) * 100
plot(combined.data$SP.Price.Change.Forward,type="l")

# Forward 1-month S&P price change vs yield curve inversion
boxplot(combined.data$SP.Price.Change.Forward ~ combined.data$Yield.Curve.10Y<0,
        main="Forward 1-Month S&P Price Change vs\n Yield Curve Inversion",
        xlab="Yield Curve Inversion",ylab="% Change in S&P 500")
boxplot(combined.data$SP.Price.Change.Forward ~ combined.data$Yield.Curve.10Y<0,outline=FALSE,
        main="Forward 1-Month S&P Price Change vs\n Yield Curve Inversion",
        xlab="Yield Curve Inversion",ylab="% Change in S&P 500")
summary(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y<0])
summary(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y>0])
hist(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y<0])





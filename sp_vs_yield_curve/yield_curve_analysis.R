# from http://www.econ.yale.edu/~shiller/data.htm
sp.data <- read.csv("./prepared_data/schiller_sp_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)

# from https://fred.stlouisfed.org/series/FEDFUNDS
fedfunds <- read.csv("./raw_data/FEDFUNDS_1954_2018.csv",sep=",",stringsAsFactors = FALSE)
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
png("./sp_vs_yield_curve/yield_curve_plots/yield_curve_vs_time.png")
combined.data$Yield.Curve.10Y <- combined.data$GS10 - combined.data$FEDFUNDS
plot(combined.data$Date,combined.data$Yield.Curve.10Y,type="l",
     main="Yield Curve (10-Year minus Fed Funds)",xlab="Date",ylab="10Y-FF",
     col="blue",yaxt="n")
breaks <- c(-6,-4,-2,0,2,4)
breaklabels <- paste(breaks,"%",sep="")
axis(2,at=breaks,lab=breaklabels)
abline(h=0)
dev.off()

# add future 1-month S&P 500 price change
combined.data$SP.Price.Change.Forward <- (combined.data$SP.Price.Next / combined.data$SP.Price - 1) * 100
plot(combined.data$SP.Price.Change.Forward,type="l")

# Forward 1-month S&P price change vs yield curve inversion
png("./sp_vs_yield_curve/yield_curve_plots/sp_1mo_vs_yc.png")
boxplot(combined.data$SP.Price.Change.Forward ~ combined.data$Yield.Curve.10Y<0,
        main="Forward 1-Month S&P Price Change vs\n Inverted Yield Curve",
        xlab="Inverted Yield Curve",ylab="% Change in S&P 500",yaxt="n")
breaks <- c(-20,-10,0,10)
breaklabels <- paste(breaks,"%",sep="")
axis(2,at=breaks,lab=breaklabels)
dev.off()

png("./sp_vs_yield_curve/yield_curve_plots/sp_1mo_vs_yc_zoom.png")
boxplot(combined.data$SP.Price.Change.Forward ~ combined.data$Yield.Curve.10Y<0,outline=FALSE,
        main="Forward 1-Month S&P Price Change vs\n Inverted Yield Curve",
        xlab="Inverted Yield Curve",ylab="% Change in S&P 500",yaxt="n")
breaks <- c(-10,-5,0,5)
breaklabels <- paste(breaks,"%",sep="")
axis(2,at=breaks,lab=breaklabels)
dev.off()

summary(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y<0])
summary(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y>0])
hist(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y<0],breaks=20)
hist(combined.data$SP.Price.Change.Forward[combined.data$Yield.Curve.10Y>0],breaks=20)

# chi-sq test of positive price change vs positive yield curve
table(combined.data$SP.Price.Change.Forward>0,combined.data$Yield.Curve.10Y>0)
chisq.test(table(combined.data$SP.Price.Change.Forward>0,combined.data$Yield.Curve.10Y>0))
# p-value = 0.1232

# Backward 1-month S&P price change vs yield curve inversion
combined.data$SP.Price.Change.Backward <- (combined.data$SP.Price / combined.data$SP.Price.Last - 1) * 100

boxplot(combined.data$SP.Price.Change.Backward ~ combined.data$Yield.Curve.10Y<0,
        main="Backward 1-Month S&P Price Change vs\n Inverted Yield Curve",
        xlab="Inverted Yield Curve",ylab="% Change in S&P 500")
summary(combined.data$SP.Price.Change.Backward[combined.data$Yield.Curve.10Y<0])
summary(combined.data$SP.Price.Change.Backward[combined.data$Yield.Curve.10Y>0])
hist(combined.data$SP.Price.Change.Backward[combined.data$Yield.Curve.10Y<0])
hist(combined.data$SP.Price.Change.Backward[combined.data$Yield.Curve.10Y>0])

# Forward 6-month S&P price change vs yield curve inversion
combined.data$SP.Price.Change.Forward.6Mo <- (combined.data$SP.Price.Next.6Mo / combined.data$SP.Price - 1) * 100

boxplot(combined.data$SP.Price.Change.Forward.6Mo ~ combined.data$Yield.Curve.10Y<0,
        main="Forward 6-Month S&P Price Change vs\n Inverted Yield Curve",
        xlab="Inverted Yield Curve",ylab="% Change in S&P 500",yaxt="n")
axis(2,at=c(-40,-20,0,20,40),lab=c("-40%","-20%","0%","20%","40%"))
summary(combined.data$SP.Price.Change.Forward.6Mo[combined.data$Yield.Curve.10Y<0])
summary(combined.data$SP.Price.Change.Forward.6Mo[combined.data$Yield.Curve.10Y>0])
hist(combined.data$SP.Price.Change.Forward.6Mo[combined.data$Yield.Curve.10Y<0])
hist(combined.data$SP.Price.Change.Forward.6Mo[combined.data$Yield.Curve.10Y>0])

# chisq test for forward 6-month price change
table(combined.data$SP.Price.Change.Forward.6Mo>0,combined.data$Yield.Curve.10Y>0)
chisq.test(table(combined.data$SP.Price.Change.Forward.6Mo>0,combined.data$Yield.Curve.10Y>0))
# p-value = 9.011e-10

boxplot(combined.data$GS10 ~ combined.data$Yield.Curve.10Y<0)



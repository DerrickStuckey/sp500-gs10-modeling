library(ggplot2)

# S&P 500 daily price data
sp.data <- read.csv("./sp500_daily_cleaned.csv", sep=",", stringsAsFactors = FALSE)
head(sp.data)

# from https://fred.stlouisfed.org/series/FEDFUNDS
yieldcurve <- read.csv("./T10Y2Y_imputed.csv",sep=",",stringsAsFactors = FALSE)
head(yieldcurve)

# format dates
sp.data$Date <- as.POSIXct(sp.data$Date)
yieldcurve$DATE <- as.POSIXct(yieldcurve$DATE)

# merge the datasets
combined.data <- merge(sp.data, yieldcurve, by.x="Date",by.y="DATE")
head(combined.data)
tail(combined.data)
nrow(combined.data)
nrow(yieldcurve)
nrow(sp.data)

# Boolean var for Yield Curve Inversion
combined.data$Yield.Curve.Status <- "Positive"
combined.data$Yield.Curve.Status[combined.data$T10Y2Y.imputed<0] <- "Inverted"

# Yield curve
# png("./yield_curve_plots_daily/yield_curve_vs_time.png")
plot(combined.data$Date,combined.data$T10Y2Y.imputed,type="l",
     main="Yield Curve (10-Year minus Fed Funds)",xlab="Date",ylab="10Y-2Y",
     col="blue",yaxt="n")
breaks <- c(-2,-1,0,1,2,3)
breaklabels <- paste(breaks,"%",sep="")
axis(2,at=breaks,lab=breaklabels)
abline(h=0)
# dev.off()

# add future 1-month S&P 500 price change
combined.data$SP.Price.Change.Forward <- (combined.data$Next.Adj.Close / combined.data$Adj.Close - 1)
plot(combined.data$SP.Price.Change.Forward,type="l")

# Forward 1-day S&P price change vs yield curve inversion
ggplot(combined.data, aes(x=Yield.Curve.Status, y=SP.Price.Change.Forward)) + 
  ggtitle("Daily S&P Price Change vs\n Inverted Yield Curve") +
  xlab("Yield Curve") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-.025,.025),
                       labels = scales::percent_format(accuracy = 1)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

# Density Plot
ggplot(combined.data, aes(x=SP.Price.Change.Forward,fill=Yield.Curve.Status)) + 
  ggtitle("Daily S&P Price Change vs\n Inverted Yield Curve") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  geom_density(alpha=0.3) + 
  scale_x_continuous(limits=c(-0.05,0.05), labels = scales::percent_format(accuracy = 1)) + 
  labs(fill="Yield Curve")

summary(combined.data$SP.Price.Change.Forward)
summary(combined.data$SP.Price.Change.Forward[combined.data$T10Y2Y.imputed<0])
summary(combined.data$SP.Price.Change.Forward[combined.data$T10Y2Y.imputed>0])

# chi-sq test of positive price change vs positive yield curve
table(combined.data$SP.Price.Change.Forward>0,combined.data$T10Y2Y.imputed>0)
chisq.test(table(combined.data$SP.Price.Change.Forward>0,combined.data$T10Y2Y.imputed>0))
# p-value = 0.2209

# Forward 30-Day S&P price change vs yield curve inversion
combined.data$SP.Price.Change.Forward.30Day <- (combined.data$Adj.Close.30.Ahead / combined.data$Adj.Close - 1)

# Boxplot in ggplot2
ggplot(combined.data, aes(x=Yield.Curve.Status, y=SP.Price.Change.Forward.30Day)) + 
  ggtitle("Forward 30-Day S&P Price Change vs\n Yield Curve Status") +
        xlab("Yield Curve") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-0.15,0.15), labels = scales::percent_format(accuracy=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

# Density Plot
ggplot(combined.data, aes(x=SP.Price.Change.Forward.30Day,fill=Yield.Curve.Status)) + 
  ggtitle("Forward 30-Day S&P Price Change vs\n Yield Curve") +
  xlab("30-Day % Change in S&P 500") + ylab("Freqency") + 
  geom_density(alpha=0.3) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(-0.15,0.15), labels = scales::percent_format(accuracy = 1)) + 
  labs(fill="Yield Curve")

summary(combined.data$SP.Price.Change.Forward.30Day[combined.data$T10Y2Y.imputed<0])
summary(combined.data$SP.Price.Change.Forward.30Day[combined.data$T10Y2Y.imputed>0])

# chisq test for forward 30-Day price change
table(combined.data$SP.Price.Change.Forward.30Day>0,combined.data$T10Y2Y.imputed>0)
chisq.test(table(combined.data$SP.Price.Change.Forward.30Day>0,combined.data$T10Y2Y.imputed>0))
# p-value = 0.005698



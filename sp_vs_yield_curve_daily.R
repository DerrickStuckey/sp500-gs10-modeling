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

# split the daily data points into 'runs' of positive or inverted yield curve periods
run=1
run.start <- combined.data$Date[1]
current.status=combined.data$Yield.Curve.Status[1]
last.inversion <- NA
for(i in 2:nrow(combined.data)) {
  if(combined.data$Yield.Curve.Status[i]!=current.status) {
    run <- run+1
    run.start <- combined.data$Date[i]
    current.status <- combined.data$Yield.Curve.Status[i]
    if(combined.data$Yield.Curve.Status[i]=="Inverted") {
      print(combined.data$Date[i])
      last.inversion <- combined.data$Date[i]
    }
  }
  combined.data$run[i] <- run
  combined.data$days.since.start[i] <- combined.data$Date[i] - run.start
  combined.data$days.since.last.inversion[i] <- combined.data$Date[i] - last.inversion
  combined.data$last.inversion[i] <- last.inversion
}

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

## Daily S&P 500 price change
combined.data$SP.Price.Change.Forward <- (combined.data$Next.Adj.Close / combined.data$Adj.Close - 1)
plot(combined.data$SP.Price.Change.Forward,type="l")

# Forward 1-day S&P price change vs yield curve inversion
png("./yield_curve_plots_daily/sp_day_price_vs_yc_boxplot.png")
ggplot(combined.data, aes(x=Yield.Curve.Status, y=SP.Price.Change.Forward)) + 
  ggtitle("Daily S&P Price Change vs\n Inverted Yield Curve") +
  xlab("Yield Curve") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-.025,.025),
                       labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)
dev.off()

# Density Plot
png("./yield_curve_plots_daily/sp_day_price_vs_yc_density.png")
ggplot(combined.data, aes(x=SP.Price.Change.Forward,fill=Yield.Curve.Status)) + 
  ggtitle("Daily S&P Price Change vs\n Inverted Yield Curve") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  geom_density(alpha=0.3) + 
  scale_x_continuous(limits=c(-0.05,0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill="Yield Curve")
dev.off()

summary(combined.data$SP.Price.Change.Forward)
summary(combined.data$SP.Price.Change.Forward[combined.data$T10Y2Y.imputed<0])
summary(combined.data$SP.Price.Change.Forward[combined.data$T10Y2Y.imputed>0])

# chi-sq test of positive price change vs positive yield curve
table(combined.data$SP.Price.Change.Forward>0,combined.data$T10Y2Y.imputed>0)
# FALSE TRUE
# FALSE   747 4278
# TRUE    797 4890
chisq.test(table(combined.data$SP.Price.Change.Forward>0,combined.data$T10Y2Y.imputed>0))
# p-value = 0.2209


## Forward 30-Day S&P price change vs yield curve inversion
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
# NOT valid as these data points are highly autocorrelated
# need to segment data points to periods with no overlap in performance period


## Forward 180-Day S&P price change vs yield curve inversion
combined.data$SP.Price.Change.Forward.180Day <- (combined.data$Adj.Close.180.Ahead / combined.data$Adj.Close - 1)

# Boxplot in ggplot2
ggplot(combined.data, aes(x=Yield.Curve.Status, y=SP.Price.Change.Forward.180Day)) + 
  ggtitle("Forward 180-Day S&P Price Change vs\n Yield Curve Status") +
  xlab("Yield Curve") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-0.15,0.15), labels = scales::percent_format(accuracy=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

# Density Plot
ggplot(combined.data, aes(x=SP.Price.Change.Forward.180Day,fill=Yield.Curve.Status)) + 
  ggtitle("Forward 180-Day S&P Price Change vs\n Yield Curve") +
  xlab("180-Day % Change in S&P 500") + ylab("Freqency") + 
  geom_density(alpha=0.3) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(-0.30,0.40), labels = scales::percent_format(accuracy = 1)) + 
  labs(fill="Yield Curve")

summary(combined.data$SP.Price.Change.Forward.180Day[combined.data$T10Y2Y.imputed<0])
summary(combined.data$SP.Price.Change.Forward.180Day[combined.data$T10Y2Y.imputed>0])

# chisq test for forward 180-Day price change
# NOT valid as these data points are highly autocorrelated
# need to segment data points to periods with no overlap in performance period

# Boxplot with Yield Curve Buckets
combined.data$Yield.Curve.Level <- as.factor(paste(round(combined.data$T10Y2Y.imputed,0),"%",sep=""))
levels(combined.data$Yield.Curve.Level) <- c("-2%","-1%","0%","1%","2%","3%")
ggplot(combined.data, aes(x=Yield.Curve.Level, y=SP.Price.Change.Forward)) + 
  ggtitle("Daily S&P Price Change vs\n Inverted Yield Curve") +
  xlab("10-Year 2-Year Spread") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-.025,.025),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

# 30-day
ggplot(combined.data, aes(x=Yield.Curve.Level, y=SP.Price.Change.Forward.30Day)) + 
  ggtitle("30-Day S&P Price Change vs\n Inverted Yield Curve") +
  xlab("10-Year 2-Year Spread") + ylab("% Change in S&P 500") + 
  geom_boxplot(outlier.shape = NA, notch=FALSE) + 
  scale_y_continuous(limits=c(-.025,.025),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)




## data manipulation for S&P price change since last inversion
inversion.starts <- subset(combined.data[!is.na(combined.data$days.since.last.inversion) & 
                                     combined.data$days.since.last.inversion==0,],select=c("Date","Adj.Close"))
head(inversion.starts)
names(inversion.starts)[2] <- "Last.Inversion.Adj.Close"
after.inversion <- combined.data[!is.na(combined.data$days.since.last.inversion) & 
                                   combined.data$days.since.last.inversion<365*2,]
head(after.inversion)
after.inversion <- merge(after.inversion,inversion.starts,by.x="last.inversion",by.y="Date")
after.inversion <- after.inversion[order(after.inversion$Date),]
head(after.inversion)
after.inversion$Price.Change.Since.Last.Inversion <- after.inversion$Adj.Close / after.inversion$Last.Inversion.Adj.Close
after.inversion$last.inversion <- as.factor(after.inversion$last.inversion)
inversion.lengths <- table(after.inversion$last.inversion)
selected.inversions <- inversion.lengths[inversion.lengths>9]
summary(as.integer(inversion.lengths))

inversion.periods <- after.inversion[after.inversion$Yield.Curve.Status=="Inverted",]
inversion.periods <- inversion.periods[order(inversion.periods$Date, decreasing=TRUE),]
inversion.periods <- inversion.periods[!duplicated(inversion.periods$last.inversion),]
inversion.periods <- subset(inversion.periods, select=c("Date","last.inversion"))
names(inversion.periods) <- c("StartDate","EndDate")
inversion.periods$StartDate <- as.Date(inversion.periods$StartDate)
inversion.periods$EndDate <- as.Date(inversion.periods$EndDate)
combined.data$Date <- as.Date(combined.data$Date)

## plot S&P 500 vs days since last inversion
after.inversion.selected <- after.inversion[after.inversion$last.inversion %in% names(selected.inversions),]
png("./yield_curve_plots_daily/sp_price_since_last_inversion")
ggplot(data=after.inversion.selected, aes(x=days.since.last.inversion, y=Price.Change.Since.Last.Inversion, color=last.inversion)) +
  geom_line(aes(group=run)) +
  # geom_jitter() + 
  # geom_point() + 
  ggtitle("S&P Price vs Time") +
  xlab("Days Since Last Inversion") + ylab("S&P 500") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Inversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1))
dev.off()

## plot log-scale S&P 500 vs time, highlighting inverted yield curve periods
png("./yield_curve_plots_daily/sp_vs_time_yccolor_bars.png")
ggplot(combined.data) + 
  geom_line(aes(x=date, y=Adj.Close, color=Yield.Curve.Status, group=run)) + 
  ggtitle("S&P Price vs Time") + 
  xlab("Date") + ylab("S&P 500 (Log-scaled)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_log10() + labs(color="Yield Curve") + 
  geom_rect(data=inversion.periods, aes(xmin=StartDate, xmax=EndDate, ymin=0, ymax=+Inf), fill='pink', alpha=0.4)
dev.off()


library(ggplot2)

# 10-year treasury bond yield
# from https://fred.stlouisfed.org/series/GS10
GS10 <- read.csv("./raw_data/GS10.csv", stringsAsFactors = FALSE)
head(GS10)
tail(GS10)

# T-bill rate from IMF data
# from https://fred.stlouisfed.org/series/INTGSTUSM193N
# INTGSTUSM193N <- read.csv("./raw_data/INTGSTUSM193N.csv", stringsAsFactors = FALSE)
# head(INTGSTUSM193N)
# names(INTGSTUSM193N)[2] <- "Tbill.Rate"

# 3-month T-bill rate
# from https://fred.stlouisfed.org/series/TB3MS
TB3MS <- read.csv("./raw_data/TB3MS.csv", stringsAsFactors = FALSE)
head(TB3MS)
tail(TB3MS)
names(TB3MS)[2] <- "Tbill.Rate"

# combined.data <- merge(GS10, INTGSTUSM193N, by="DATE")
combined.data <- merge(GS10, TB3MS, by="DATE")
# View(combined.data)
combined.data$DATE <- as.Date(combined.data$DATE, format="%Y-%m-%d")

# 10-Year T-Bill spread
combined.data$spread <- combined.data$GS10 - combined.data$Tbill.Rate
combined.data$Yield.Curve.Status <- "Positive"
combined.data$Yield.Curve.Status[combined.data$spread<0] <- "Inverted"


plot(combined.data$DATE, combined.data$spread, type='l')
abline(h=0)

# View(combined.data[combined.data$spread<0,])

# add S&P data, prepared by calculate_schiller_returns.py
sp.data <- read.csv("./prepared_data/schiller_sp_data_withreturns.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data)
tail(sp.data)
sp.data$Date <- as.Date(sp.data$Date, format="%Y-%m-%d")

# estimate returns for stocks and bonds for each 6, 12, and 18-month period
combined.data <- merge(sp.data, combined.data, by.x="Date", by.y="DATE")
# View(combined.data)
summary(combined.data$GS10.x == combined.data$GS10.y)
combined.data <- subset(combined.data,select=-c(GS10.y))
names(combined.data)[names(combined.data)=="GS10.x"] <- "GS10"

# add recessions data (from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
head(recessions)
recessions.trim <- recessions[recessions$Peak>min(combined.data$Date),]

# Yield curve over time with recessions shaded
ggplot(data=combined.data) + 
  geom_line(aes(x=Date,y=spread/100),color="blue") + 
  ggtitle("10-Year T-Bill Treasury Spread") + 
  xlab("Date") + ylab("10Y-2Y") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave(filename = "./sp_vs_yield_curve/yield_curve_plots_daily/yield_curve_with_recessions_monthly.png")

## 1-month forward looking
# S&P forward return vs yield curve status
boxplot(combined.data$SP.Return.Forward ~ combined.data$Yield.Curve.Status,
        main="1-month SP return vs Positive Yield Curve")
summary(combined.data$SP.Return.Forward[combined.data$spread>0])
summary(combined.data$SP.Return.Forward[combined.data$spread<0])

# GS10 forward return vs yield curve status
boxplot(combined.data$GS10.Return.Forward ~ combined.data$Yield.Curve.Status,
        main="1-month GS10 return vs Positive Yield Curve")
summary(combined.data$GS10.Return.Forward[combined.data$spread>0])
summary(combined.data$GS10.Return.Forward[combined.data$spread<0])

# 1-month S&P vs GS10 spread
combined.data$SP.GS10.Spread.Forward <- combined.data$SP.Return.Forward - combined.data$GS10.Return.Forward

boxplot(combined.data$SP.GS10.Spread.Forward ~ combined.data$Yield.Curve.Status,
        main="1-month SP vs GS10 return spread vs Positive Yield Curve")
summary(combined.data$SP.GS10.Spread.Forward[combined.data$spread>0])
summary(combined.data$SP.GS10.Spread.Forward[combined.data$spread<0])

## 6-month forward looking
# calculate 6-month forward returns for S&P 500
combined.data$SP.Return.Forward.6Mo <- 
  exp(combined.data$Log.SP.Return.Forward.Cumulative.6Months.Ahead - 
        combined.data$Log.SP.Return.Forward.Cumulative)

# calculate 6-month forward returns for 10-year treasuries
combined.data$GS10.Return.Forward.6Mo <- 
  exp(combined.data$Log.GS10.Return.Forward.Cumulative.6Months.Ahead - 
        combined.data$Log.GS10.Return.Forward.Cumulative)

tail(combined.data$SP.Return.Forward.6Mo, n=20)
tail(combined.data)

boxplot(combined.data$SP.Return.Forward.6Mo ~ combined.data$Yield.Curve.Status,
        main="6-month S&P returns vs Positive Yield Curve")
summary(combined.data$SP.Return.Forward.6Mo[combined.data$spread>0])
summary(combined.data$SP.Return.Forward.6Mo[combined.data$spread<0])

boxplot(combined.data$GS10.Return.Forward.6Mo ~ combined.data$Yield.Curve.Status,
        main="6-month GS10 returns vs Positive Yield Curve")
summary(combined.data$GS10.Return.Forward.6Mo[combined.data$spread>0])
summary(combined.data$GS10.Return.Forward.6Mo[combined.data$spread<0])

# 6-month S&P vs GS10 spread
combined.data$SP.GS10.Spread.6Mo <- combined.data$SP.Return.Forward.6Mo - combined.data$GS10.Return.Forward.6Mo

boxplot(combined.data$SP.GS10.Spread.6Mo ~ combined.data$Yield.Curve.Status,
        main="6-month SP vs GS10 return spread vs Positive Yield Curve")
summary(combined.data$SP.GS10.Spread.6Mo[combined.data$spread>0])
summary(combined.data$SP.GS10.Spread.6Mo[combined.data$spread<0])

## 18-month forward returns
# calculate 18-month forward returns for S&P 500
combined.data$SP.Return.Forward.18Mo <- 
  exp(combined.data$Log.SP.Return.Forward.Cumulative.18Months.Ahead - 
        combined.data$Log.SP.Return.Forward.Cumulative)

# calculate 18-month forward returns for 10-year treasuries
combined.data$GS10.Return.Forward.18Mo <- 
  exp(combined.data$Log.GS10.Return.Forward.Cumulative.18Months.Ahead - 
        combined.data$Log.GS10.Return.Forward.Cumulative)

tail(combined.data$SP.Return.Forward.18Mo, n=20)
tail(combined.data)

boxplot(combined.data$SP.Return.Forward.18Mo ~ combined.data$Yield.Curve.Status,
        main="18-month S&P returns vs Positive Yield Curve")
summary(combined.data$SP.Return.Forward.18Mo[combined.data$spread>0])
summary(combined.data$SP.Return.Forward.18Mo[combined.data$spread<0])

boxplot(combined.data$GS10.Return.Forward.18Mo ~ combined.data$Yield.Curve.Status,
        main="18-month GS10 returns vs Positive Yield Curve")
summary(combined.data$GS10.Return.Forward.18Mo[combined.data$spread>0])
summary(combined.data$GS10.Return.Forward.18Mo[combined.data$spread<0])

# 18-month S&P vs GS10 spread
combined.data$SP.GS10.Spread.18Mo <- combined.data$SP.Return.Forward.18Mo - combined.data$GS10.Return.Forward.18Mo

boxplot(combined.data$SP.GS10.Spread.18Mo ~ combined.data$Yield.Curve.Status,
        main="18-month SP vs GS10 return spread vs Positive Yield Curve")
summary(combined.data$SP.GS10.Spread.18Mo[combined.data$spread>0])
summary(combined.data$SP.GS10.Spread.18Mo[combined.data$spread<0])

# split the daily data points into 'runs' of positive or inverted yield curve periods
run=1
run.start <- 1
last.inversion <- NA
last.inversion.date <- as.Date(NA)
current.status=combined.data$Yield.Curve.Status[1]
combined.data$last.inversion.date[1] <- NA
last.inversion.Log.SP.Return.Forward.Cumulative <- NA
last.inversion.Log.GS10.Return.Forward.Cumulative <- NA
for(i in 1:nrow(combined.data)) {
  if(combined.data$Yield.Curve.Status[i]!=current.status) {
    run <- run+1
    run.start <- i
    current.status <- combined.data$Yield.Curve.Status[i]
    if(combined.data$Yield.Curve.Status[i]=="Inverted") {
      # print(combined.data$Date[i])
      last.inversion.date <- combined.data$Date[i]
      last.inversion <- i
      last.inversion.Log.SP.Return.Forward.Cumulative <- combined.data$Log.SP.Return.Forward.Cumulative[i]
      last.inversion.Log.GS10.Return.Forward.Cumulative <- combined.data$Log.GS10.Return.Forward.Cumulative[i]
    }
  }
  combined.data$run[i] <- run
  combined.data$months.since.start[i] <- i - run.start
  combined.data$months.since.last.inversion[i] <- i - last.inversion
  combined.data$last.inversion.date[i] <- last.inversion.date
  combined.data$last.inversion.Log.SP.Return.Forward.Cumulative[i] <- last.inversion.Log.SP.Return.Forward.Cumulative
  combined.data$last.inversion.Log.GS10.Return.Forward.Cumulative[i] <- last.inversion.Log.GS10.Return.Forward.Cumulative
}
combined.data$last.inversion.date <- as.Date(combined.data$last.inversion.date,tz="GMT",origin="1970-01-01")
combined.data$last.inversion.date <- as.factor(combined.data$last.inversion.date) # make this a factor for graph legend
tail(combined.data$last.inversion.date)

# calculate returns since last inversion
combined.data$SP.Return.Since.Last.Inversion <- exp(combined.data$Log.SP.Return.Forward.Cumulative - 
                                                      combined.data$last.inversion.Log.SP.Return.Forward.Cumulative) - 1
combined.data$GS10.Return.Since.Last.Inversion <- exp(combined.data$Log.GS10.Return.Forward.Cumulative - 
                                                      combined.data$last.inversion.Log.GS10.Return.Forward.Cumulative) - 1
combined.data$SP.GS10.Return.Spread.Since.Last.Inversion <- 
  combined.data$SP.Return.Since.Last.Inversion - combined.data$GS10.Return.Since.Last.Inversion

tail(combined.data$GS10.Return.Since.Last.Inversion)
tail(combined.data$SP.Return.Since.Last.Inversion)

# View(combined.data)

## plot returns vs months since last inversion
after.inversion <- combined.data[!is.na(combined.data$months.since.last.inversion) & 
                                   combined.data$months.since.last.inversion<12*3,]
after.inversion.selected <- after.inversion[as.Date(after.inversion$last.inversion.date)<as.Date("2016-08-01"),]
head(after.inversion.selected)

# S&P return since last inversion
ggplot(data=after.inversion.selected, aes(x=months.since.last.inversion, y=SP.Return.Since.Last.Inversion, color=last.inversion.date)) +
  geom_line(aes(group=last.inversion.date)) +
  ggtitle("S&P Total Return Since Last Inversion") +
  xlab("Months Since Last Inversion") + ylab("S&P 500 Total Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Inversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")
  # scale_color_discrete(breaks=c(names(inversion.lengths[inversion.lengths>200])))

# GS10 return since last inversion
ggplot(data=after.inversion.selected, aes(x=months.since.last.inversion, y=GS10.Return.Since.Last.Inversion, color=last.inversion.date)) +
  geom_line(aes(group=last.inversion.date)) +
  ggtitle("10-Year Treasury Total Return Since Last Inversion") +
  xlab("Months Since Last Inversion") + ylab("10-Year Treasury Total Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Inversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")
# scale_color_discrete(breaks=c(names(inversion.lengths[inversion.lengths>200])))

# S&P - GS10 Return Spread
ggplot(data=after.inversion.selected, aes(x=months.since.last.inversion, y=SP.GS10.Return.Spread.Since.Last.Inversion, color=last.inversion.date)) +
  geom_line(aes(group=last.inversion.date)) +
  ggtitle("S&P Return - GS10 Return Since Last Inversion") +
  xlab("Months Since Last Inversion") + ylab("S&P Return - GS10 Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Inversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")

# Average S&P - GS10 Return Spread
avg.sp.vs.gs <- aggregate(after.inversion.selected$SP.GS10.Return.Spread.Since.Last.Inversion,
                          by=list(after.inversion.selected$months.since.last.inversion),
                          FUN=mean)
names(avg.sp.vs.gs) <- c("months.since.last.inversion","SP.GS10.Return.Spread.Since.Last.Inversion")
ggplot(data=avg.sp.vs.gs, aes(x=months.since.last.inversion, y=SP.GS10.Return.Spread.Since.Last.Inversion)) +
  geom_line(aes(group=last.inversion.date)) +
  ggtitle("S&P Return - GS10 Return Since Last Inversion") +
  xlab("Months Since Last Inversion") + ylab("S&P Return - GS10 Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Inversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")





# momentum vs yield curve
mo.data <- combined.data
mo.data$recent.inversion <- mo.data$months.since.last.inversion < 36
table(mo.data$recent.inversion)

mo.data$mo.6mo <- mo.data$SP.Price > mo.data$SP.Price.Prev.6Mo
table(mo.data$mo.6mo)

boxplot(mo.data$SP.Return.Forward ~ mo.data$recent.inversion, outline=FALSE)
boxplot(mo.data$SP.Return.Forward ~ mo.data$mo.6mo, outline=FALSE)

boxplot(mo.data$SP.Return.Forward ~ mo.data$recent.inversion + mo.data$mo.6mo)
boxplot(mo.data$SP.Return.Forward.6Mo ~ mo.data$recent.inversion + mo.data$mo.6mo)

# look at avg and sd of returns versus these 2 variables
avg.returns <- aggregate(data.frame("return.avg"=mo.data$SP.Return.Forward), 
                         by=list("recent.inversion"=mo.data$recent.inversion,
                                 "momentum.6mo"=mo.data$mo.6mo),
                         FUN=function(x) mean(x,na.rm = TRUE))
avg.returns
sd.returns <- aggregate(data.frame("return.sd"=mo.data$SP.Return.Forward), 
                        by=list("recent.inversion"=mo.data$recent.inversion,
                                "momentum.6mo"=mo.data$mo.6mo),
                        FUN=function(x) sd(x,na.rm=TRUE))
sd.returns
count.returns <- aggregate(data.frame("months"=mo.data$SP.Return.Forward), 
                        by=list("recent.inversion"=mo.data$recent.inversion,
                                "momentum.6mo"=mo.data$mo.6mo),
                        FUN=function(x) sum(!is.na(x)))
count.returns

mo.summary <- merge(avg.returns,sd.returns,by=c("recent.inversion","momentum.6mo"))
mo.summary <- merge(mo.summary,count.returns,by=c("recent.inversion","momentum.6mo"))
mo.summary

write.csv(mo.summary,file="./sp_vs_yield_curve/momentum_results.csv",row.names=FALSE)


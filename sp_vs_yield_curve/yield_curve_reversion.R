### 'Reversion' data
# perform same analysis but for 'reversion' to positive spread
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


reversion.data <- combined.data
run=1
run.start <- 1
last.reversion <- NA
last.reversion.date <- as.Date(NA)
current.status=reversion.data$Yield.Curve.Status[1]
reversion.data$last.reversion.date[1] <- NA
last.reversion.Log.SP.Return.Forward.Cumulative <- NA
last.reversion.Log.GS10.Return.Forward.Cumulative <- NA
for(i in 1:nrow(reversion.data)) {
  if(reversion.data$Yield.Curve.Status[i]!=current.status) {
    run <- run+1
    run.start <- i
    current.status <- reversion.data$Yield.Curve.Status[i]
    if(reversion.data$Yield.Curve.Status[i]=="Positive") {
      # print(reversion.data$Date[i])
      last.reversion.date <- reversion.data$Date[i]
      last.reversion <- i
      last.reversion.Log.SP.Return.Forward.Cumulative <- reversion.data$Log.SP.Return.Forward.Cumulative[i]
      last.reversion.Log.GS10.Return.Forward.Cumulative <- reversion.data$Log.GS10.Return.Forward.Cumulative[i]
    }
  }
  reversion.data$run[i] <- run
  reversion.data$months.since.start[i] <- i - run.start
  reversion.data$months.since.last.reversion[i] <- i - last.reversion
  reversion.data$last.reversion.date[i] <- last.reversion.date
  reversion.data$last.reversion.Log.SP.Return.Forward.Cumulative[i] <- last.reversion.Log.SP.Return.Forward.Cumulative
  reversion.data$last.reversion.Log.GS10.Return.Forward.Cumulative[i] <- last.reversion.Log.GS10.Return.Forward.Cumulative
}
reversion.data$last.reversion.date <- as.Date(reversion.data$last.reversion.date,tz="GMT",origin="1970-01-01")
reversion.data$last.reversion.date <- as.factor(reversion.data$last.reversion.date)
tail(reversion.data$last.reversion.date)

# calculate returns since last reversion
reversion.data$SP.Return.Since.Last.Reversion <- exp(reversion.data$Log.SP.Return.Forward.Cumulative - 
                                                       reversion.data$last.reversion.Log.SP.Return.Forward.Cumulative) - 1
reversion.data$GS10.Return.Since.Last.Reversion <- exp(reversion.data$Log.GS10.Return.Forward.Cumulative - 
                                                         reversion.data$last.reversion.Log.GS10.Return.Forward.Cumulative) - 1
reversion.data$SP.GS10.Return.Spread.Since.Last.Reversion <- 
  reversion.data$SP.Return.Since.Last.Reversion - reversion.data$GS10.Return.Since.Last.Reversion

# select data up to 3 years out from last reversion
after.reversion.selected <- reversion.data[!is.na(reversion.data$months.since.last.reversion) & 
                                             reversion.data$months.since.last.reversion<12*3,]
head(after.reversion.selected)

# S&P return since last reversion
ggplot(data=after.reversion.selected, aes(x=months.since.last.reversion, y=SP.Return.Since.Last.Reversion, color=last.reversion.date)) +
  geom_line(aes(group=last.reversion.date)) +
  ggtitle("S&P Total Return Since Last Reversion") +
  xlab("Months Since Last reversion") + ylab("S&P 500 Total Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last reversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")

# GS10 return since last reversion
ggplot(data=after.reversion.selected, aes(x=months.since.last.reversion, y=GS10.Return.Since.Last.Reversion, color=last.reversion.date)) +
  geom_line(aes(group=last.reversion.date)) +
  ggtitle("10-Year Treasury Total Return Since Last Reversion") +
  xlab("Months Since Last Reversion") + ylab("10-Year Treasury Total Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Reversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")
# scale_color_discrete(breaks=c(names(reversion.lengths[reversion.lengths>200])))

# S&P - GS10 Return Spread
ggplot(data=after.reversion.selected, aes(x=months.since.last.reversion, y=SP.GS10.Return.Spread.Since.Last.Reversion, color=last.reversion.date)) +
  geom_line(aes(group=last.reversion.date)) +
  ggtitle("S&P Return - GS10 Return Since Last Reversion") +
  xlab("Months Since Last Reversion") + ylab("S&P Return - GS10 Return") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Last Reversion") + scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")


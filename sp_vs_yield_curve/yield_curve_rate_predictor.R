## Is the yield curve actually good at predicting changes in short-term interest rates?

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

# calculate lookahead interest rates
combined.data$GS10.6.Months.Ahead <- NA
combined.data$GS10.12.Months.Ahead <- NA
combined.data$GS10.24.Months.Ahead <- NA
combined.data$GS10.36.Months.Ahead <- NA
combined.data$Tbill.Rate.6.Months.Ahead <- NA
combined.data$Tbill.Rate.12.Months.Ahead <- NA
combined.data$Tbill.Rate.24.Months.Ahead <- NA
combined.data$Tbill.Rate.36.Months.Ahead <- NA
for (i in 1:nrow(combined.data)) {
  if (i-6 > 0) {
    combined.data$GS10.6.Months.Ahead[i-6] <- combined.data$GS10[i]
    combined.data$Tbill.Rate.6.Months.Ahead[i-6] <- combined.data$Tbill.Rate[i]
  }
  if (i-12 > 0) {
    combined.data$GS10.12.Months.Ahead[i-12] <- combined.data$GS10[i]
    combined.data$Tbill.Rate.12.Months.Ahead[i-12] <- combined.data$Tbill.Rate[i]
  }
  if (i-24 > 0) {
    combined.data$GS10.24.Months.Ahead[i-24] <- combined.data$GS10[i]
    combined.data$Tbill.Rate.24.Months.Ahead[i-24] <- combined.data$Tbill.Rate[i]
  }
  if (i-36 > 0) {
    combined.data$GS10.36.Months.Ahead[i-36] <- combined.data$GS10[i]
    combined.data$Tbill.Rate.36.Months.Ahead[i-36] <- combined.data$Tbill.Rate[i]
  }
}

combined.data.6ahead <- combined.data[!is.na(combined.data$Tbill.Rate.6.Months.Ahead),]
combined.data.12ahead <- combined.data[!is.na(combined.data$Tbill.Rate.12.Months.Ahead),]
plot(combined.data.12ahead$spread, combined.data.12ahead$GS10.12.Months.Ahead - combined.data.12ahead$GS10)
cor(combined.data.12ahead$spread, combined.data.12ahead$GS10.12.Months.Ahead - combined.data.12ahead$GS10)

boxplot(combined.data.12ahead$GS10.12.Months.Ahead - combined.data.12ahead$GS10 ~ combined.data.12ahead$spread>0)


plot(combined.data.12ahead$spread, combined.data.12ahead$Tbill.Rate.12.Months.Ahead - combined.data.12ahead$Tbill.Rate)
cor(combined.data.12ahead$spread, combined.data.12ahead$Tbill.Rate.12.Months.Ahead - combined.data.12ahead$Tbill.Rate)

boxplot(combined.data.12ahead$Tbill.Rate.12.Months.Ahead - combined.data.12ahead$Tbill.Rate ~ combined.data.12ahead$spread>0)

# extract start and end dates for inversions
inversion.starts <- c(as.Date("2099-01-01")) # initialize with a fake date
inversion.ends <- c(as.Date("2099-01-01")) # initialize with a fake date
prior.status <- combined.data$Yield.Curve.Status[1]
if(prior.status=="Inverted") { inversion.starts <- c(combined.data$DATE[1]) }
for (i in 2:(nrow(combined.data)-1)) {
  current.date <- combined.data$DATE[i]
  current.status <- combined.data$Yield.Curve.Status[i]
  if (prior.status=="Positive" & current.status=="Inverted") {
    inversion.starts <- c(inversion.starts, current.date)
    prior.status <- "Inverted"
  }
  if (prior.status=="Inverted" & current.status=="Positive") {
    inversion.ends <- c(inversion.ends, current.date)
    prior.status <- "Positive"
  }
}
if(current.status=="Inverted") { inversion.ends <- c(inversion.ends, combined.data$DATE[nrow(combined.data)]) }
inversion.starts <- inversion.starts[2:length(inversion.starts)] # drop the initial fake date
inversion.ends <- inversion.ends[2:length(inversion.ends)] # drop the initial fake date
inversions <- data.frame("start"=inversion.starts,"end"=inversion.ends)
head(inversions)

# plot short-term interest rates over time w/ inversions shaded
# is there a visually clear pattern following an inversion?
ggplot(data=combined.data) + 
  geom_line(aes(x=DATE,y=Tbill.Rate/100),color="blue") + 
  ggtitle("3-Month T-Bill Rate\nYield Curve Inversions Shaded") + 
  xlab("Date") + ylab("T-Bill Rate") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_rect(data=inversions, aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# yes, T-bill rates tend to fall after a yield curve inversion

# Change in T-bill rates over next 6 months versus yield curve status
combined.data.6ahead$Tbill.Rate.6.Months.Change <- combined.data.6ahead$Tbill.Rate.6.Months.Ahead - combined.data.6ahead$Tbill.Rate
boxplot(combined.data.6ahead$Tbill.Rate.6.Months.Change ~ 
          combined.data.6ahead$Yield.Curve.Status,
        ylab="6-month change in T-bill rates")
boxplot(combined.data.6ahead$Tbill.Rate.6.Months.Change ~ 
          combined.data.6ahead$Yield.Curve.Status, 
        outline=FALSE, ylab="6-month change in T-bill rates")
summary(combined.data.6ahead$Tbill.Rate.6.Months.Change[combined.data.6ahead$Yield.Curve.Status=="Inverted"])
summary(combined.data.6ahead$Tbill.Rate.6.Months.Change[combined.data.6ahead$Yield.Curve.Status=="Positive"])
chisq.test(table(combined.data.6ahead$Tbill.Rate.6.Months.Change>0,combined.data.6ahead$Yield.Curve.Status=="Inverted"))
# p-value = 0.0008832
# Inverted status definitely predicts lower T-bill rates in 6 months time
# on average, about 0.5% lower


# Change in T-bill rates over next 12 months versus yield curve status
combined.data.12ahead$Tbill.Rate.12.Months.Change <- combined.data.12ahead$Tbill.Rate.12.Months.Ahead - combined.data.12ahead$Tbill.Rate
boxplot(combined.data.12ahead$Tbill.Rate.12.Months.Change ~ 
          combined.data.12ahead$Yield.Curve.Status,
        ylab="12-month change in T-bill rates")
boxplot(combined.data.12ahead$Tbill.Rate.12.Months.Change ~ 
          combined.data.12ahead$Yield.Curve.Status, 
        outline=FALSE, ylab="12-month change in T-bill rates")
summary(combined.data.12ahead$Tbill.Rate.12.Months.Change[combined.data.12ahead$Yield.Curve.Status=="Inverted"])
summary(combined.data.12ahead$Tbill.Rate.12.Months.Change[combined.data.12ahead$Yield.Curve.Status=="Positive"])
chisq.test(table(combined.data.12ahead$Tbill.Rate.12.Months.Change>0,combined.data.12ahead$Yield.Curve.Status=="Inverted"))
# p-value = 6.539e-06
# Inverted status definitely predicts lower T-bill rates in 12 months time
# on average, about 1% lower



library(ggplot2)

### S&P 500 daily price data
sp.data <- read.csv("./prepared_data/sp500_daily_cleaned.csv", sep=",", stringsAsFactors = FALSE)
head(sp.data)

# format dates
sp.data$Date <- as.Date(sp.data$Date)

# derived variables
sp.data$SP.Price.Change.Forward <- sp.data$Next.Adj.Close / sp.data$Adj.Close - 1
sp.data$SP.Price.Change.Backward <- sp.data$Adj.Close / sp.data$Prev.Adj.Close - 1
sp.data$SP.Price.Change.Backward.2 <- sp.data$Prev.Adj.Close / sp.data$Adj.Close.2.Prev - 1
sp.data$SP.Price.Change.Backward.3 <- sp.data$Adj.Close.2.Prev / sp.data$Adj.Close.3.Prev - 1
sp.data$Prev.SP.Direction[sp.data$SP.Price.Change.Backward>0] <- "Up"
sp.data$Prev.SP.Direction[sp.data$SP.Price.Change.Backward<0] <- "Down"
sp.data$Prev.SP.Direction.2[sp.data$SP.Price.Change.Backward.2>0] <- "Up"
sp.data$Prev.SP.Direction.2[sp.data$SP.Price.Change.Backward.2<0] <- "Down"
sp.data$Prev.SP.Direction.Last.2 <- paste(sp.data$Prev.SP.Direction.2,sp.data$Prev.SP.Direction,sep=",")
table(sp.data$Prev.SP.Direction.Last.2)

# plot probability density for next S&P price change vs previous S&P price change direction
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward),]) + 
  geom_density(aes(x=SP.Price.Change.Forward,fill=Prev.SP.Direction), alpha=0.3) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_x_continuous(limits=c(-0.05,0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill="Previous S&P Move")
ggsave(filename = "./momentum_analysis/momentum_charts_1/sp_daily_mo_density.png")

# boxplot for next S&P price change vs previous S&P price change direction
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction\n1950-2018") +
  xlab("Previous Days's Price Change Direction") + ylab("Current Day's Price Change") + 
  scale_y_continuous(limits=c(-0.025,0.025), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "./momentum_analysis/momentum_charts_1/sp_daily_mo_boxplot_1950_2018.png")

sp.data.sel <- sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward),]
chisq.test(sp.data.sel$Prev.SP.Direction, 
           sp.data.sel$SP.Price.Change.Forward>0)
# p-value = 1.082e-14
t.test(sp.data.sel$SP.Price.Change.Forward ~ sp.data.sel$Prev.SP.Direction)
# p-value = 8.846e-14
aggregate(sp.data.sel$SP.Price.Change.Forward,
          by=list("Prev.Direction"=sp.data.sel$Prev.SP.Direction),
          FUN=mean)
# Prev.Direction             x
# 1           Down -0.0002496785
# 2             Up  0.0008597255

# last 20 years only
# boxplot for next S&P price change vs previous S&P price change direction
sp.data.recent <- sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward) & 
                            sp.data$Date>as.Date("1998-01-01"),]
ggplot(data=sp.data.recent) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction\n 1998-2018") +
  xlab("Previous Days's Price Change Direction") + ylab("Current Day's Price Change") + 
  scale_y_continuous(limits=c(-0.03,0.03), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "./momentum_analysis/momentum_charts_1/sp_daily_mo_1998_2018.png")

chisq.test(sp.data.recent$Prev.SP.Direction, 
           sp.data.recent$SP.Price.Change.Forward>0)
# p-value = 5.246e-05
aggregate(sp.data.recent$SP.Price.Change.Forward,
          by=list("Prev.Direction"=sp.data.recent$Prev.SP.Direction),
          FUN=mean)
# Prev.Direction             x
# 1           Down  0.0007834305
# 2             Up -0.0001733372

# last year only
# boxplot for next S&P price change vs previous S&P price change direction
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward) & 
                      sp.data$Date>as.Date("2018-01-01"),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_y_continuous(limits=c(-0.03,0.03), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))


# boxplot vs last two price moves
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$Prev.SP.Direction.2),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.Last.2,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_y_continuous(limits=c(-0.03,0.03), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))

# aggregate data by Year
sp.data$Year <- as.numeric(substr(as.character(sp.data$Date),1,4))
yearly.mo <- aggregate(data.frame("Positive.Change.Pct"=sp.data$SP.Price.Change.Forward>0,
                     "Avg.Change"=sp.data$SP.Price.Change.Forward),
          by=list("Year"=sp.data$Year,"Prev.SP.Direction"=sp.data$Prev.SP.Direction),
          FUN=mean)
head(yearly.mo)
library(reshape2)
yearly.mo.dir <- dcast(yearly.mo, Year ~ Prev.SP.Direction, value.var="Positive.Change.Pct", FUN=mean)
yearly.mo.dir$Diff <- yearly.mo.dir$Up - yearly.mo.dir$Down
# View(yearly.mo.dir)
write.csv(yearly.mo.dir, file="./momentum_analysis/momentum_charts_1/daily_momentum_by_year_dir.csv", row.names=FALSE)

yearly.mo.avg <- dcast(yearly.mo, Year ~ Prev.SP.Direction, value.var="Avg.Change", FUN=mean)
yearly.mo.avg$Diff <- yearly.mo.avg$Up - yearly.mo.avg$Down
# View(yearly.mo.avg)
write.csv(yearly.mo.avg, file="./momentum_analysis/momentum_charts_1/daily_momentum_by_year_avg.csv", row.names=FALSE)

# prob density vs last two moves, when last move is up
ggplot(data=sp.data[sp.data$Prev.SP.Direction.Last.2 %in% c("Down,Up","Up,Up"),]) + 
  geom_density(aes(x=SP.Price.Change.Forward,fill=Prev.SP.Direction.Last.2), alpha=0.3) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_x_continuous(limits=c(-0.05,0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill="Previous Two S&P Moves")

sp.data.sel <- sp.data[sp.data$Prev.SP.Direction.Last.2 %in% c("Down,Up","Up,Up") &
                         !is.na(sp.data$SP.Price.Change.Forward),]
chisq.test(sp.data.sel$Prev.SP.Direction.Last.2, 
           sp.data.sel$SP.Price.Change.Forward>0)
t.test(sp.data.sel$SP.Price.Change.Forward ~ sp.data.sel$Prev.SP.Direction.2)


### Monthly Data
sp.data.monthly <- read.csv("./prepared_data/schiller_sp_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
sp.data.monthly$Date <- as.Date(sp.data.monthly$Date)
head(sp.data.monthly)

# derived variables
sp.data.monthly$SP.Price.Change.Forward <- sp.data.monthly$SP.Price.Next / sp.data.monthly$SP.Price - 1
sp.data.monthly$SP.Price.Change.Backward <- sp.data.monthly$SP.Price / sp.data.monthly$SP.Price.Last - 1
sp.data.monthly$Prev.SP.Direction[sp.data.monthly$SP.Price.Change.Backward>0] <- "Up"
sp.data.monthly$Prev.SP.Direction[sp.data.monthly$SP.Price.Change.Backward<0] <- "Down"
sp.data.monthly$SP.Price.Change.Forward.6mo <- sp.data.monthly$SP.Price.Next.6Mo / sp.data.monthly$SP.Price - 1
sp.data.monthly$SP.Price.Change.Backward.6mo <- sp.data.monthly$SP.Price / sp.data.monthly$SP.Price.Prev.6Mo - 1
sp.data.monthly$Prev.SP.Direction.6mo[sp.data.monthly$SP.Price.Change.Backward.6mo>0] <- "Up"
sp.data.monthly$Prev.SP.Direction.6mo[sp.data.monthly$SP.Price.Change.Backward.6mo<0] <- "Down"

# boxplot for monthly price change vs last price change direction (since 1950)
sp.data.monthly.midcent <- sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction) & 
                                            !is.na(sp.data.monthly$SP.Price.Change.Forward) &
                                            sp.data.monthly$Date>=as.Date("1950-01-01") &
                                            sp.data.monthly$Date<as.Date("1998-01-01"),]
ggplot(data=sp.data.monthly.midcent) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("Monthly S&P Price Change vs\nPrevious Month S&P Price Change Direction\n1950 - 1998") +
  xlab("Previous Month's Price Change Direction") + ylab("Current Month's Price Change") + 
  scale_y_continuous(limits=c(-0.1,0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/month_vs_month_boxplot_1950_1998.png")

# month vs last month price change direction chi-sq test, 1950 - 1998
chisq.test(sp.data.monthly.midcent$Prev.SP.Direction, sp.data.monthly.midcent$SP.Price.Change.Forward>0)
# p-value = 0.02282
aggregate(sp.data.monthly.midcent$SP.Price.Change.Forward, 
          by=list("Prev.Direction"=sp.data.monthly.midcent$Prev.SP.Direction),
          FUN=mean)
# Prev.Direction           x
# 1           Down 0.003815762
# 2             Up 0.010005625

# month vs last month, 1998 - 2018 only
sp.data.monthly.recent <- sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction) & 
                                          !is.na(sp.data.monthly$SP.Price.Change.Forward) &
                                          sp.data.monthly$Date > as.Date("1998-01-01"),]
ggplot(data=sp.data.monthly.recent) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("Monthly S&P Price Change vs\nPrevious Month S&P Price Change Direction\n1998 - 2018") +
  xlab("Previous Month's Price Change Direction") + ylab("Current Month's Price Change") + 
  scale_y_continuous(limits=c(-0.1,0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/month_vs_month_boxplot_1998_2018.png")

# month vs last month price change direction chi-sq test, 1998 - 2018
chisq.test(sp.data.monthly.recent$Prev.SP.Direction, sp.data.monthly.recent$SP.Price.Change.Forward>0)
# p-value = 0.01508
aggregate(sp.data.monthly.recent$SP.Price.Change.Forward, 
          by=list("Prev.Direction"=sp.data.monthly.recent$Prev.SP.Direction),
          FUN=mean)
# Prev.Direction            x
# 1           Down -0.002967110
# 2             Up  0.009394638

# month vs last month, 1870 - 1949
sp.data.monthly.premodern <- sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction) & 
                                            !is.na(sp.data.monthly$SP.Price.Change.Forward) &
                                            sp.data.monthly$Date<as.Date("1950-01-01"),]
ggplot(data=sp.data.monthly.premodern) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("Monthly S&P Price Change vs\nPrevious Month S&P Price Change Direction\n1870 - 1949") +
  xlab("Previous Month's Price Change Direction") + ylab("Current Month's Price Change") + 
  scale_y_continuous(limits=c(-0.1,0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/month_vs_month_boxplot_1870_1949.png")

# month vs last month price change direction chi-sq test, 1998 - 2018
chisq.test(sp.data.monthly.premodern$Prev.SP.Direction, sp.data.monthly.premodern$SP.Price.Change.Forward>0)
# p-value = 2.323e-11
aggregate(sp.data.monthly.premodern$SP.Price.Change.Forward, 
          by=list("Prev.Direction"=sp.data.monthly.premodern$Prev.SP.Direction),
          FUN=mean)
# Prev.Direction            x
# 1           Down -0.008038827
# 2             Up  0.011829221

## 6-month price vs last 6 months, using only January and July starting months

# pick out only Jan + July data points
library(lubridate)
sp.data.biannual <- sp.data.monthly[month(sp.data.monthly$Date) %in% c(1,7),]
head(sp.data.biannual$Date)

# 1870 - 2018
ggplot(data=sp.data.biannual[!is.na(sp.data.biannual$Prev.SP.Direction.6mo),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.6mo,y=SP.Price.Change.Forward.6mo), outlier.shape = NA, notch=FALSE) + 
  ggtitle("6-Month S&P Price Change vs\nPrevious 6-Month S&P Price Change Direction\n1870 - 2018 (Jan 1 + July 1 only)") +
  xlab("Previous 6-Month Price Change Direction") + ylab("Current 6-Month Price Change") + 
  scale_y_continuous(limits=c(-0.25,0.25), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/6month_vs_6month_boxplot_1870_2018.png")

sp.data.biannual.recent <- sp.data.biannual[sp.data.biannual$Date>=as.Date("2000-01-01"),]
table(sp.data.biannual.recent$Prev.SP.Direction.6mo, sp.data.biannual.recent$SP.Price.Change.Forward.6mo>0)
summary(sp.data.biannual.recent$SP.Price.Change.Forward.6mo[sp.data.biannual.recent$Prev.SP.Direction.6mo=="Up"])
summary(sp.data.biannual.recent$SP.Price.Change.Forward.6mo[sp.data.biannual.recent$Prev.SP.Direction.6mo=="Down"])

chisq.test(sp.data.biannual$Prev.SP.Direction.6mo, sp.data.biannual$SP.Price.Change.Forward.6mo>0)
# p-value = 0.005409

# 6-month price change, using all months as starting months (not independent data points)
# 1998 - 2018
ggplot(data=sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction.6mo) &
                              sp.data.monthly$Date >= as.Date("1998-01-01"),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.6mo,y=SP.Price.Change.Forward.6mo), outlier.shape = NA, notch=FALSE) + 
  ggtitle("6-Month S&P Price Change vs\nPrevious 6-Month S&P Price Change Direction\n1998 - 2018") +
  xlab("Previous 6-Month Price Change Direction") + ylab("Current 6-Month Price Change") + 
  scale_y_continuous(limits=c(-0.30,0.30), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/6month_vs_6month_boxplot_1998_2018_allmonths.png")

# 1950 - 1997
ggplot(data=sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction.6mo) &
                              sp.data.monthly$Date >= as.Date("1950-01-01") & 
                              sp.data.monthly$Date < as.Date("1998-01-01"),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.6mo,y=SP.Price.Change.Forward.6mo), outlier.shape = NA, notch=FALSE) + 
  ggtitle("6-Month S&P Price Change vs\nPrevious 6-Month S&P Price Change Direction\n1950 - 1997") +
  xlab("Previous 6-Month Price Change Direction") + ylab("Current 6-Month Price Change") + 
  scale_y_continuous(limits=c(-0.30,0.30), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/6month_vs_6month_boxplot_1950_1997_allmonths.png")

# 1870 - 1949
ggplot(data=sp.data.monthly[!is.na(sp.data.monthly$Prev.SP.Direction.6mo) &
                              sp.data.monthly$Date >= as.Date("1870-01-01") & 
                              sp.data.monthly$Date < as.Date("1950-01-01"),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.6mo,y=SP.Price.Change.Forward.6mo), outlier.shape = NA, notch=FALSE) + 
  ggtitle("6-Month S&P Price Change vs\nPrevious 6-Month S&P Price Change Direction\n1870 - 1949") +
  xlab("Previous 6-Month Price Change Direction") + ylab("Current 6-Month Price Change") + 
  scale_y_continuous(limits=c(-0.30,0.30), labels = scales::percent_format(accuracy = 1)) + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename="./momentum_analysis/momentum_charts_1/6month_vs_6month_boxplot_1870_1949_allmonths.png")


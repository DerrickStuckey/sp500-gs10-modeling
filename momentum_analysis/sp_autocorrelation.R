library(ggplot2)

# S&P 500 daily price data
sp.data <- read.csv("./prepared_data/sp500_daily_cleaned.csv", sep=",", stringsAsFactors = FALSE)
head(sp.data)

# format dates
sp.data$Date <- as.POSIXct(sp.data$Date)

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
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill="Previous S&P Move")

# boxplot for next S&P price change vs previous S&P price change direction
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_y_continuous(limits=c(-0.03,0.03), labels = scales::percent_format(accuracy = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

sp.data.sel <- sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$SP.Price.Change.Forward),]
chisq.test(sp.data.sel$Prev.SP.Direction, 
           sp.data.sel$SP.Price.Change.Forward>0)
t.test(sp.data.sel$SP.Price.Change.Forward ~ sp.data.sel$Prev.SP.Direction)

# boxplot vs last two price moves
ggplot(data=sp.data[!is.na(sp.data$Prev.SP.Direction) & !is.na(sp.data$Prev.SP.Direction.2),]) + 
  geom_boxplot(aes(x=Prev.SP.Direction.Last.2,y=SP.Price.Change.Forward), outlier.shape = NA, notch=FALSE) + 
  ggtitle("S&P Price Change vs Last S&P Price Change Direction") +
  xlab("% Change in S&P 500") + ylab("Freqency") + 
  scale_y_continuous(limits=c(-0.03,0.03), labels = scales::percent_format(accuracy = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

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


sp.data.monthly <- read.csv("./prepared_data/schiller_sp_data_formatted.tsv", sep="\t", stringsAsFactors = FALSE)
head(sp.data.monthly)


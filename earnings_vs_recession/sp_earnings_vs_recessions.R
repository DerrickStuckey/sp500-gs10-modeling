library(tidyverse)
library(scales)

# Schiller monthly S&P 500 data
schiller.data <- read.csv("./prepared_data/schiller_sp_data_formatted.tsv",
                          sep="\t",
                          stringsAsFactors = FALSE)
schiller.data$Date <- as.Date(schiller.data$Date, format="%Y-%m-%d")

# View(schiller.data)

# plot Earnings vs Time, Log-scaled
schiller.data.withearnings <- filter(schiller.data,
                                     !is.na(schiller.data$Earnings))
ggplot(data=schiller.data.withearnings, aes(x=Date,y=Earnings)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("S&P 500 Earnings") +
  ylab("S&P 500 Earnings per Share") + 
  scale_y_continuous(trans="log10") +
  # geom_abline(slope=0,intercept=0)
  geom_hline(yintercept=0)

# add recessions data (from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
recessions <- read.table("./raw_data/recessions_may2021.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
head(recessions)
recessions.trim <- recessions %>% filter(
  Trough >= min(schiller.data.withearnings$Date),
  Peak <= max(schiller.data.withearnings$Date)
)
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)

# plot Earnings vs Time, Log-scaled
# with recessions shaded
ggplot(data=schiller.data.withearnings) +
  geom_line(aes(x=Date,y=Earnings)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("S&P 500 Earnings") +
  ylab("S&P 500 Earnings per Share") + 
  scale_y_continuous(trans="log10", labels = scales::label_dollar()) +
  geom_hline(yintercept=0) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.3)

# calculate Earnings Drawdown from Peak
previous.peak.earnings <- schiller.data.withearnings$Earnings[[1]]
schiller.data.withearnings$previous.peak.earnings <- previous.peak.earnings
schiller.data.withearnings$earnings.drawdown <- 0
for (i in 1:nrow(schiller.data.withearnings)) {
  if (schiller.data.withearnings$Earnings[i] > previous.peak.earnings) {
    previous.peak.earnings <- schiller.data.withearnings$Earnings[i]
  }
  schiller.data.withearnings$previous.peak.earnings[i] <- previous.peak.earnings
  schiller.data.withearnings$earnings.drawdown[i] <- 
    min(0, schiller.data.withearnings$Earnings[i] / previous.peak.earnings - 1)
}

# plot Earnings Drawdown from Peak
schiller.data.withearnings.modern <- schiller.data.withearnings %>% filter(
  Date > "1950-01-01"
)
recessions.modern <- recessions %>% filter(
  Trough >= min(schiller.data.withearnings.modern$Date),
  Peak <= max(schiller.data.withearnings.modern$Date)
)
ggplot(data=schiller.data.withearnings.modern) +
  geom_line(aes(x=Date,y=earnings.drawdown),col="red") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("S&P 500 Earnings Drawdowns") +
  ylab("S&P 500 Earnings % Off Peak") + 
  scale_y_continuous(labels = scales::label_percent(),
                     limits = c(-1,0)) +
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
ggsave("./earnings_vs_recession/charts/earnings_drawdowns_modern_era.png")

# calculate S&P Price Drawdown from Peak
previous.peak.price <- schiller.data$SP.Price[[1]]
schiller.data$previous.peak.price <- previous.peak.price
schiller.data$price.drawdown <- 0
for (i in 1:nrow(schiller.data)) {
  if (schiller.data$SP.Price[i] > previous.peak.price) {
    previous.peak.price <- schiller.data$SP.Price[i]
  }
  schiller.data$previous.peak.price[i] <- previous.peak.price
  schiller.data$price.drawdown[i] <- 
    min(0, schiller.data$SP.Price[i] / previous.peak.price - 1)
}

# plot Price Drawdown from Peak
schiller.data.withprice.modern <- schiller.data %>% filter(
  Date > "1950-01-01",
  !is.na(SP.Price)
)
recessions.withprice.modern <- recessions %>% filter(
  Trough >= min(schiller.data.withprice.modern$Date),
  Peak <= max(schiller.data.withprice.modern$Date)
)
ggplot(data=schiller.data.withprice.modern) +
  geom_line(aes(x=Date,y=price.drawdown),col="red") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("S&P 500 Price Drawdowns") +
  ylab("S&P 500 Price % Off Peak") + 
  scale_y_continuous(labels = scales::label_percent(),
                     limits = c(-1,0)) +
  geom_rect(data=recessions.withprice.modern, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
ggsave("./earnings_vs_recession/charts/price_drawdowns_modern_era.png")


# overlay with GDP drawdowns




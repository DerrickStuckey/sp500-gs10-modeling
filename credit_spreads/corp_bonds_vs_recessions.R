library(ggplot2)
library(tidyverse)

# Moody's Aaa Corporate bond yield
# note: underlying bonds are 20+ year maturities, so not 100% comparable to 10-year treasury
# https://fred.stlouisfed.org/series/AAA
corp.bonds <- read.csv("./raw_data/AAA.csv", stringsAsFactors = FALSE)
head(corp.bonds)
tail(corp.bonds)
corp.bonds$Date <- corp.bonds$DATE %>% as.Date(format="%Y-%m-%d")
corp.bonds <- corp.bonds %>% select(-DATE)
head(corp.bonds)

# recessions data
# from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
head(recessions)
tail(recessions)
recessions.trim <- recessions[recessions$Peak>min(corp.bonds$Date),]


# plot AAA bond yield only
ggplot(data=corp.bonds) + 
  geom_line(aes(x=Date,y=AAA/100),color="blue") + 
  ggtitle("AAA Bond Yield") + 
  xlab("Date") + ylab("AAA-GS10") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)

# calculate months since last recession
corp.bonds$months.since.last.recession <- -1
first.recession.start <- as.Date(first(recessions.trim$Peak))
start.index <- match(first.recession.start, corp.bonds$Date)
months.since <- -1
for (i in start.index:nrow(corp.bonds)) {
  current.date <- corp.bonds$Date[i]
  if (current.date %in% recessions.trim$Peak) {
    months.since <- 0
  } else {
    months.since <- months.since + 1
  }
  corp.bonds$months.since.last.recession[i] <- months.since
}
# View(corp.bonds)

# calculate months until next recession




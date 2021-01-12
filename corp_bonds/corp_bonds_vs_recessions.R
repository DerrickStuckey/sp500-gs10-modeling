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

# change in bond yields
corp.bonds$AAA.Change <- NULL
corp.bonds$AAA.Change[2:nrow(corp.bonds)] <- diff(corp.bonds$AAA)
head(corp.bonds)

# calculate months since last recession
corp.bonds$months.since.last.recession <- -1
corp.bonds$latest.recession <- as.Date("0001-01-01")
first.recession.start <- as.Date(first(recessions.trim$Peak))
start.index <- match(first.recession.start, corp.bonds$Date)
months.since <- -1
latest.recession <- first.recession.start
for (i in start.index:nrow(corp.bonds)) {
  current.date <- corp.bonds$Date[i]
  if (current.date %in% recessions.trim$Peak) {
    months.since <- 0
    latest.recession <- current.date
  } else {
    months.since <- months.since + 1
  }
  corp.bonds$months.since.last.recession[i] <- months.since
  corp.bonds$latest.recession[i] <- latest.recession
}
# View(corp.bonds)

# calculate months until next recession
corp.bonds$months.until.next.recession <- -1
corp.bonds$next.recession <- as.Date("0001-01-01")
last.recession.start <- as.Date(last(recessions.trim$Peak))
end.index <- match(last.recession.start, corp.bonds$Date)
months.until <- -1
next.recession <- last.recession.start
for (j in end.index:1) {
  current.date <- corp.bonds$Date[j]
  if (current.date %in% recessions.trim$Peak) {
    months.until <- 0
    next.recession <- current.date
  } else {
    months.until <- months.until + 1
  }
  corp.bonds$months.until.next.recession[j] <- months.until
  corp.bonds$next.recession[j] <- next.recession
}
# View(corp.bonds)

# take 12 months preceding a recession start
just.before <- corp.bonds %>%
  filter(months.until.next.recession <= 12
         & months.until.next.recession >= 0)
# View(just.before)
head(just.before)
next.recessions <- corp.bonds %>%
  filter(months.until.next.recession == 0) %>%
  select(next.recession, AAA) %>%
  rename(AAA.at.Next.Recession=AAA)
head(next.recessions)
just.before <- just.before %>%
  inner_join(next.recessions, by="next.recession")
head(just.before)

# plot AAA yield change prior to each recession start
ggplot(data=just.before, aes(x=months.until.next.recession*-1, y=(AAA-AAA.at.Next.Recession)/100, color=as.factor(next.recession))) +
  geom_line(aes(group=next.recession)) +
  ggtitle("AAA Bond Yield Change Prior to Recessions") +
  xlab("Months Until Recession") + ylab("AAA Bond Yield Change\nRelative to Yield at Next Recession") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Next Recession") + scale_y_continuous(labels = scales::percent_format(accuracy=0.1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")

just.before %>% filter(months.until.next.recession==12) %>%
  mutate(AAA.vs.Next.Recession = AAA-AAA.at.Next.Recession) %>% 
  View()


# take 12 months following a recession start
just.after <- corp.bonds %>%
  filter(months.since.last.recession <= 12
         & months.since.last.recession >= 0)
# View(just.before)
head(just.after)
prev.recessions <- corp.bonds %>%
  filter(months.since.last.recession == 0) %>%
  select(latest.recession, AAA) %>%
  rename(AAA.at.Latest.Recession=AAA)
head(prev.recessions)
just.after <- just.after %>%
  inner_join(prev.recessions, by="latest.recession")
head(just.after)

# plot AAA yield change prior to each recession start
ggplot(data=just.after, aes(x=months.since.last.recession, y=(AAA-AAA.at.Latest.Recession)/100, color=as.factor(latest.recession))) +
  geom_line(aes(group=latest.recession)) +
  ggtitle("AAA Bond Yield Change Following Recession Onset") +
  xlab("Months After Recession Started") + ylab("AAA Bond Yield\nChange Since Recession Onset") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color="Next Recession") + scale_y_continuous(labels = scales::percent_format(accuracy=0.1)) +
  geom_hline(yintercept=0, linetype="solid", color = "black")




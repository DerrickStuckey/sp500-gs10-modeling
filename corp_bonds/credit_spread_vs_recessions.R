library(ggplot2)
library(tidyverse)

# 10-year treasury bond yield
# from https://fred.stlouisfed.org/series/GS10
GS10 <- read.csv("./raw_data/GS10.csv", stringsAsFactors = FALSE)
head(GS10)
tail(GS10)
# normalize GS10
GS10$GS10 <- GS10$GS10 / 100

# Moody's Aaa Corporate bond yield
# note: underlying bonds are 20+ year maturities, so not 100% comparable to 10-year treasury
# https://fred.stlouisfed.org/series/AAA
corp.bonds <- read.csv("./raw_data/AAA.csv", stringsAsFactors = FALSE)
head(corp.bonds)
tail(corp.bonds)
# normalize AAA
corp.bonds$AAA <- corp.bonds$AAA / 100

combined.data <- GS10 %>%
  inner_join(corp.bonds, by=c("DATE"="DATE"))
combined.data$spread <- combined.data$AAA - combined.data$GS10
combined.data$Date <- combined.data$DATE %>% as.Date(format="%Y-%m-%d")
combined.data <- combined.data %>%
  select(-DATE)
head(combined.data)
tail(combined.data)

# recessions data
# from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
head(recessions)
tail(recessions)
recessions.trim <- recessions[recessions$Peak>min(combined.data$Date),]

# plot credit spread with recessions shaded
ggplot(data=combined.data) + 
  geom_line(aes(x=Date,y=spread),color="blue") + 
  ggtitle("AAA Bonds Spread vs 10-Year Treasury") + 
  xlab("Date") + ylab("AAA-GS10") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)


# 20-year treasury yield
# from https://fred.stlouisfed.org/series/DGS20
DGS20 <- read.csv("./raw_data/DGS20.csv", stringsAsFactors = FALSE)
DGS20$DGS20 <- as.numeric(DGS20$DGS20) / 100

combined.data.20 <- DGS20 %>%
  inner_join(corp.bonds, by=c("DATE"="DATE"))
combined.data.20$spread <- combined.data.20$AAA - combined.data.20$DGS20
combined.data.20$Date <- combined.data.20$DATE %>% as.Date(format="%Y-%m-%d")
combined.data.20 <- combined.data.20 %>%
  select(-DATE)
head(combined.data.20)
tail(combined.data.20)

# plot credit spread with recessions shaded
ggplot(data=combined.data.20) + 
  geom_line(aes(x=Date,y=spread),color="blue") + 
  ggtitle("AAA Bonds Spread vs 20-Year Treasury") + 
  xlab("Date") + ylab("AAA-DGS20") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)

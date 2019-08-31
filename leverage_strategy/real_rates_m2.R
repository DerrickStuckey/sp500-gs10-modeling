# analysis of real interest rates, using M2 change instead of CPI change as deflator

# Effective Fed Funds rate since 1954
# from https://fred.stlouisfed.org/series/FEDFUNDS
fedfunds <- read.csv("./raw_data/FEDFUNDS.csv", stringsAsFactors = FALSE)
head(fedfunds)
fedfunds$DATE <- as.Date(fedfunds$DATE)

# CPI from Schiller Data
schiller <- read.csv("./prepared_data/Schiller Market Data - Formatted Data.tsv",sep="\t",stringsAsFactors = FALSE)
schiller$Date <- as.Date(schiller$Date)

# M2 supply since 1959, seasonally adjusted
# from https://fred.stlouisfed.org/series/M2SL
m2 <- read.csv("./raw_data/M2SL.csv", stringsAsFactors = FALSE)
head(m2)
m2$DATE <- as.Date(m2$DATE)

# calculate the rate of change in M2
m2$M2SL.1Mo.Prev <- NA
m2$M2SL.6Mo.Prev <- NA
m2$M2SL.12Mo.Prev <- NA
m2$M2SL.24Mo.Prev <- NA
for (i in 1:nrow(m2)) {
  if (i-1>0) {
    m2$M2SL.1Mo.Prev[i] <- m2$M2SL[i-1]
  }
  if (i-6>0) {
    m2$M2SL.6Mo.Prev[i] <- m2$M2SL[i-6]
  }
  if (i-12>0) {
    m2$M2SL.12Mo.Prev[i] <- m2$M2SL[i-12]
  }
  if (i-24>0) {
    m2$M2SL.24Mo.Prev[i] <- m2$M2SL[i-24]
  }
}

m2$M2SL.Change.1Mo <- m2$M2SL / m2$M2SL.1Mo.Prev
m2$M2SL.Change.12Mo <- m2$M2SL / m2$M2SL.12Mo.Prev
plot(m2$DATE,m2$M2SL.Change.12Mo,type="l")
m2$M2SL.Change.24Mo <- m2$M2SL / m2$M2SL.24Mo.Prev
plot(m2$DATE,m2$M2SL.Change.24Mo,type="l")

combined.data <- merge(fedfunds,m2,by="DATE",all.x=FALSE,all.y=FALSE)
head(combined.data)
tail(combined.data)

combined.data$fedfunds.m2.deflated <- combined.data$FEDFUNDS - (combined.data$M2SL.Change.12Mo-1)*100
plot(combined.data$DATE,combined.data$fedfunds.m2.deflated,type="l")
plot(combined.data$DATE,combined.data$FEDFUNDS,type="l")

combined.data <- merge(combined.data,schiller,by.x="DATE",by.y="Date")
View(combined.data)

combined.data$YC <- combined.data$GS10 - combined.data$FEDFUNDS/100
plot(combined.data$DATE, combined.data$YC,type="l")
View(subset(combined.data,select=c(DATE,YC)))

# write.table(combined.data,file="~/Downloads/yc_scratch.tsv",sep = "\t", row.names=FALSE)
# https://docs.google.com/spreadsheets/d/1bN9FyyikgB0x-R7ttqxs21O87eXZdMRCqbcIv0E_a0w/edit#gid=782627791



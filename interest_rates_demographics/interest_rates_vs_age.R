# from http://databank.worldbank.org/data/AgeGenderHistory/id/37786463
# formatted by format_age_gender_history.R
demodata <- read.table("./prepared_data/age_gender_history.tsv",
                       sep="\t", header=TRUE, stringsAsFactors = FALSE)
head(demodata)

# Demographic proportions
demodata$population.20.34.proportion <- (demodata$Female.population.20.24 + demodata$Female.population.25.29 + demodata$Female.population.30.34 +
                                           demodata$Male.population.20.24 + demodata$Male.population.25.29 + demodata$Male.population.30.34) / 
  demodata$Population..total

demodata$population.65.plus.proportion <- (demodata$Female.population.65.69 + demodata$Female.population.70.74 + 
                                             demodata$Female.population.75.79 + demodata$Female.population.80. + 
                                             demodata$Male.population.65.69 + demodata$Male.population.70.74 + 
                                             demodata$Male.population.75.79 + demodata$Male.population.80.) / 
  demodata$Population..total

demodata.proportions <- subset(demodata, select=c("Country.Name","Year","population.20.34.proportion","population.65.plus.proportion"))

## United States

# US 3-month treasury bill rates
# from https://fred.stlouisfed.org/series/TB3MS
us.interest.rates <- read.csv("./raw_data/TB3MS.csv", stringsAsFactors = FALSE)
us.interest.rates$country <- "United States"
names(us.interest.rates)[names(us.interest.rates)=="TB3MS"] <- "interest.rate"
head(us.interest.rates)

# US CPI inflation
# from https://fred.stlouisfed.org/series/FPCPITOTLZGUSA
us.inflation <- read.csv("./raw_data/FPCPITOTLZGUSA.csv", stringsAsFactors = FALSE)
us.inflation$country <- "United States"
names(us.inflation)[names(us.inflation)=="FPCPITOTLZGUSA"] <- "inflation.rate"
head(us.inflation)

## India 

# https://fred.stlouisfed.org/series/FPCPITOTLZGIND
# India CPI Inflation
# from https://fred.stlouisfed.org/series/FPCPITOTLZGIND
india.inflation <- read.csv("./raw_data/FPCPITOTLZGIND.csv", stringsAsFactors = FALSE)
names(india.inflation)[names(india.inflation)=="FPCPITOTLZGIND"] <- "inflation.rate"
india.inflation$country <- "India"
head(india.inflation)

# India discount rate
# from https://fred.stlouisfed.org/series/INTDSRINM193N
india.interest.rates <- read.csv("./raw_data/INTDSRINM193N.csv", stringsAsFactors = FALSE)
india.interest.rates$country <- "India"
names(india.interest.rates)[names(india.interest.rates)=="INTDSRINM193N"] <- "interest.rate"
head(india.interest.rates)

# combine rates data across countries
interest.rates.all <- rbind(us.interest.rates, india.interest.rates)
inflation.rates.all <- rbind(us.inflation, india.inflation)

# merge to get real interest rates
rates.all <- merge(interest.rates.all, inflation.rates.all, by=c("DATE","country"))
head(rates.all)
tail(rates.all)
rates.all$Year <- as.numeric(substr(rates.all$DATE,1,4))
rates.all$Real.Rate <- rates.all$interest.rate - rates.all$inflation.rate

# merge US demographic data and interest rates
full.data <- merge(demodata.proportions, rates.all, by.x=c("Year","Country.Name"), by.y=c("Year","country"))
head(full.data)
tail(full.data)




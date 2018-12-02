# from http://databank.worldbank.org/data/AgeGenderHistory/id/37786463
demodata.wide <- read.table("./raw_data/AgeGenderHistory/12b2968d-5f4f-4d71-8cd9-d89fb2b4f44e_Data.txt",
                       sep="\t",header=TRUE)
head(demodata.wide)
names(demodata.wide)

## transform the demo data to a usable format
library(reshape2)

demodata.long <- melt(demodata.wide, id.vars=c("Country.Name","Country.Code","Series.Name","Series.Code"))
names(demodata.long)[names(demodata.long)=="variable"] <- "Year"
head(demodata.long)

demodata.long <- subset(demodata.long,select=c("Country.Name","Series.Name","Year","value"))
demodata.reshaped <- dcast(demodata.long, Country.Name + Year ~ Series.Name, value.var="value", fun.aggregate=mean)
View(demodata.reshaped)

# convert the 'Year' values to integers
demodata.reshaped$Year <- as.numeric(substr(as.character(demodata.reshaped$Year),2,5))
table(demodata.reshaped$Year)


